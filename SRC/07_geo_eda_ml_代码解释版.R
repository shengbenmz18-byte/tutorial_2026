# ============================================================
# 中文代码解释版
# 文件来源：07_geo_eda_ml(3).R
# 主题：地理空间数据探索性分析（Geo-EDA）与空间机器学习建模
#
# 阅读方式：
# 1. 每个“【解释】”注释块说明后面一小段代码在做什么。
# 2. 原始代码尽量保留，只在前面增加中文说明，方便你对照学习。
# 3. 本脚本涉及 sf / terra / sp / spdep / qgisprocess / randomForest 等包；
#    有些代码需要本地数据、QGIS、SAGA 工具和正确 CRS 才能顺利运行。
# 4. 文件末尾附有整体流程总结和几个容易出错的地方。
# ============================================================
# --------------------------------------------
# Script Name: geodata EDA and modeling
# Purpose: Here is the script about how to conduct EDA of geodata 
#          and model the spatial pattern, using doubs river fishes
#          as example to show how to extract env information along 
#          doubs river with the integration of R and QGIS. Also,
#          writing the code includes how to create spatial lag features 
#          using a spatial weight matrix and how to include these 
#          spatial features in a machine learning model.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-12
# --------------------------------------------
cat("\014") # Clears the console

# 【解释】清空当前 RStudio 控制台和工作环境。
# cat("\014") 用于清屏；rm(list = ls()) 会删除当前环境中的所有对象。
# 这样可以避免旧变量影响后面的地理数据分析和建模流程。
rm(list = ls()) # Remove all variables

##################################################
# 01- the vector and raster data
#################################################

# 【解释】01 部分介绍两类最基本的地理数据：
# 1）矢量数据 vector：例如河流线、采样点、缓冲区、多边形；
# 2）栅格数据 raster：例如 DEM 高程数据、坡度、汇流面积等连续表面。
# 本段先读取 Doubs 河流的 shapefile，并查看其数据类型和坐标参考系统 CRS。
## A) load the shapefile of Doubs rive 
library(sf)
doubs_river <- st_read("data/gisdata/doubs_river.shp") # 06_eda
class(doubs_river)
st_crs(doubs_river)
st_crs(doubs_river)$proj4string

# 【解释】使用 ggplot2 + geom_sf() 可视化 sf 矢量对象。
# geom_sf() 是专门用于绘制 sf 空间数据的几何图层函数。
st_crs(doubs_river)$epsg

library(ggplot2)
ggplot(data = doubs_river) +
  geom_sf()


# 【解释】这一小段说明 DEM（Digital Elevation Model，数字高程模型）的来源。
# 被注释掉的 elevatr 代码表示：可以根据河流范围在线下载高程栅格，
# 然后保存为 GeoTIFF 文件 doubs_dem.tif。
# 因为已经有本地 tif 文件，所以实际运行时从本地读取。
# B) the digital elevation model (dem) of the doubs river
# # install.packages("remotes")
# 

# library(elevatr)
# doubs_elev <- get_elev_raster(doubs_river, z = 10) # z resolution
# doubs_elev
# terra::writeRaster(doubs_elev, "data/gisdata/doubs_dem.tif",
#                    filetype = "GTiff", overwrite = TRUE)


# 【解释】使用 terra 包读取 DEM 栅格文件，并检查它的 CRS。
# rast() 读入栅格；crs() 用于查看坐标系统。
# terra::crs(doubs_dem, proj=TRUE) 会尝试以 PROJ 字符串形式输出 CRS。
library(terra)
doubs_dem <- terra::rast("data/gisdata/doubs_dem.tif")
crs(doubs_dem)
terra::crs(doubs_dem, proj=TRUE)


# 【解释】这一段处理 Doubs 鱼类数据集中的采样点坐标。
# ade4::doubs 数据包含鱼类物种、环境变量和采样点坐标。
# doubs$xy 是采样点坐标表，写出为 csv 后可以导入 QGIS 辅助定位。
# C) getting the CRS of the sample points using qgis

data(doubs, package = "ade4")
doubs_xy <- doubs$xy
write.csv(doubs_xy, "data/gisdata/pointcoord_utm.csv")


# 【解释】下面这一大段是 QGIS 手动操作流程，不是 R 会执行的代码。
# 目的：把原始采样点坐标与真实地图、河流图层对齐，
# 再通过 QGIS 导出带有经纬度或投影坐标的采样点 shapefile。
# // The 1st step  

# importing coordinates_utm.csv to make an image using qgis
# Add Layer -> Add Delimited Text Layer -> 
# Project -> Export -> Export as image (sample_points.png)

# // The 2nd step 
# loading basic OSM map and doubs_river.shp (epsg=4326)  
# as reference for georeferencing the sample_points.png

# AD (sample_points.png) -> geroreferencing for an exact image
# https://www.youtube.com/watch?v=fzz8jw7Qp18 
# or Layer -> # Georeferencer.. -> raster for an exact image
# https://www.youtube.com/watch?v=XV62QEk0Cxg&t=106s

# // The 3rd step
# extracting the long and lat from the referenced image 

# Layer -> Create Layer -> New Shapefile Layer 
# Toggle -> Add Point Feature (sampling) -> save layer edits
# processing -> textbox -> add geometry attributes
# added geometry info -> Export -> save features as
# -> comma seperated value [csv] 
# https://www.youtube.com/watch?v=y8JKVciv26g


# 【解释】这一段把 DEM、采样点和河流同时画出来。
# 它先用 terra::plot() 画 DEM 栅格，再叠加采样点和河流线，
# 用来检查三个数据图层的位置是否对齐。
# D) visualizing river data and sampling points
par(mfrow = c(1,1))
library(sf)

doubs_dem <- terra::rast("data/gisdata/doubs_dem.tif")
doubs_dem
terra::plot(doubs_dem) # plot() from different packages

doubs_pts <- read_sf("data/gisdata/sample_points.shp")
doubs_pts
names(doubs_pts)
plot(doubs_pts, add = TRUE, cex =1.8, col = "red")

doubs_river <- read_sf("data/gisdata/doubs_river.shp")

# 【解释】这里把 DEM 栅格转成普通数据框 dem_df。
# xy=TRUE 表示保留每个栅格像元的 x、y 坐标；
# na.rm=TRUE 表示去掉无效值。
# 转成数据框后，可以用 ggplot2::geom_raster() 绘制更美观的栅格图，
# 并叠加 sf 采样点和河流图层。
doubs_river
names(doubs_river)
plot(st_geometry(doubs_river), # specifying the geometry column
     add = TRUE, col = "yellow")

dem_df <- as.data.frame(doubs_dem, xy = TRUE, na.rm = TRUE)
colnames(dem_df)
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = doubs_dem)) +
  geom_sf(data = doubs_pts, aes(geometry = geometry), 
          color = "red", size = 1) +
  geom_sf(data = doubs_river, aes(geometry = geometry), 
          color = "yellow", size = 0.5) +
  theme_minimal()

########################################################
# 02-extracting spatial features as predictors
########################################################

# 【解释】02 部分的目标是：从空间数据中提取可用于建模的预测变量。
# 例如：每个采样点附近的高程、坡度、汇流面积、距离河流的位置等。
# 这一小段先重新读取 DEM、河流和采样点三个基础空间图层。
## A) setting a 5-km buffer along rive

library(terra)
library(sf)

doubs_dem <- terra::rast("data/gisdata/doubs_dem.tif")
doubs_river <- sf::st_read("data/gisdata/doubs_river.shp")
doubs_pts <- sf::st_read("data/gisdata/sample_points.shp")


# 【解释】将河流矢量数据重投影到 UTM 31N 坐标系 EPSG:32631。
# 注意：经纬度单位是“度”，不适合直接按米做缓冲区；
# UTM 坐标单位是“米”，因此适合做距离、面积、缓冲区分析。
# re-projecting the vector data of river
doubs_river_utm <- st_transform(doubs_river, 32631) 


# 【解释】创建河流缓冲区。
# st_buffer(..., dis = 8000) 表示以河流为中心，向外扩展 8000 米。
# 注：原注释写的是 5-km buffer，但代码实际是 8000 m，也就是 8 km。
# creating and visualizing the buffer
doubs_river_buff <- st_buffer(doubs_river_utm, dis = 8000)
names(doubs_river_buff)
plot(st_geometry(doubs_river_buff), axes = TRUE)

# ggplot(doubs_river_buff) +
#   geom_sf(fill = "blue", color = "black")
# st_write(doubs_river_buff,
#          "data/gisdata/doubs_river_buff.geojson")


# 【解释】这一段把 DEM 栅格从原始 CRS 重新投影到 UTM 31N。
# 目的：让 DEM 和河流缓冲区使用同一个坐标系统，
# 否则 crop() 和 mask() 这类叠加操作可能失败或结果错位。
# B) Clipping or intersecting dem covered by the river buffer
# re_projecting raster data

terra::crs(doubs_dem, proj = TRUE) # get CRS
utm_crs <- "EPSG:32631" # set CRS
doubs_dem_utm <- terra::project(doubs_dem,utm_crs) # for sf using st_transform()
crs(doubs_dem_utm, proj = TRUE)# check crs


# 【解释】用河流缓冲区裁剪 DEM。
# crop() 先按缓冲区外接矩形裁剪，速度较快；
# mask() 再按缓冲区真实形状掩膜，只保留缓冲区内部 DEM 值。
# 结果是只覆盖 Doubs 河附近一定范围的 DEM。
# Clipping or intersecting dem by the doubs river

doubs_dem_utm_cropped = terra::crop(doubs_dem_utm,
                             doubs_river_buff)
plot(doubs_dem_utm_cropped)
doubs_dem_utm_masked = terra::mask(doubs_dem_utm_cropped,
                            doubs_river_buff)
plot(doubs_dem_utm_masked)
# writeRaster(doubs_dem_utm_masked, "data/gisdata/doubs_dem_masked.tif")


# 【解释】这一小节通过 QGIS/SAGA 地形分析算法从 DEM 中派生地形变量。
# 这里用 qgisprocess 调用 QGIS 后端算法 sagawetnessindex，
# 得到 catchment area（汇流面积）和 slope（坡度）等栅格。
# C) extracting raster values of points as predictors
# https://r.geocompx.org/eco

library(qgisprocess)
qgis_configure()
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title, algorithm) |>
  head(2)


# 【解释】qgis_run_algorithm() 调用 SAGA Wetness Index 算法。
# DEM 是输入高程栅格；
# SLOPE 和 AREA 是输出结果，使用临时 .sdat 文件保存；
# .quiet = TRUE 表示减少运行时输出信息。
# catchment slope and catchment area
topo = qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagawetnessindex",
  DEM = doubs_dem_utm_masked,
  SLOPE_TYPE = 1, 
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)
str(topo)


# 【解释】把 QGIS 输出结果转换为 terra 栅格对象。
# topo$AREA 是汇流面积，topo$SLOPE 是坡度；
# c() 可以把多个栅格层合并成一个多层 SpatRaster。
# 最后再与 DEM 合并，形成包含高程、汇流面积、坡度的多层栅格。
topo_slo_area <- c(qgis_as_terra(topo$AREA), 
                   qgis_as_terra(topo$SLOPE))
names(topo_slo_area) <- c("carea", "cslope")

topo_dem_slo_area <- c(doubs_dem_utm_masked, topo_slo_area)

writeRaster(topo_dem_slo_area,
 "data/gisdata/topo_dem_slo_area.tif",
 overwrite=FALSE)


# 【解释】把采样点也重投影到 UTM 31N。
# 只有点数据和栅格数据处在同一 CRS 下，
# 才能准确提取每个采样点对应位置的栅格值。
# re-projecting points to utm

doubs_pts_utm <- sf::st_transform(doubs_pts, utm_crs)

# st_write(doubs_pts_utm,"data/gisdata/doubs_pts_utm.geojson")


# 【解释】重新读取已经保存好的多层地形栅格和 UTM 采样点。
# terra::extract() 会把每个采样点所在位置的栅格值提取出来，
# 得到每个样点对应的 DEM、汇流面积、坡度等地形变量。
# extracting raster values
topo_dem_slo_area <- rast("data/gisdata/topo_dem_slo_area.tif")
doubs_pts_utm <- st_read("data/gisdata/doubs_pts_utm.geojson")
  
doubs_pts_topo <- terra::extract(topo_dem_slo_area, 
                                 doubs_pts_utm, ID=FALSE)
glimpse(doubs_pts_topo)


# 【解释】把三类信息合并到同一个空间数据表中：
# 1）采样点空间位置 doubs_pts_utm；
# 2）从栅格提取的地形变量 doubs_pts_topo；
# 3）Doubs 数据集中已有的水化学/环境变量 doubs$env。
# 合并后的 doubs_pts_env 可作为后续空间建模的基础数据。
# aggregating topo and water chemical env

doubs_pts_env = cbind(doubs_pts_utm, doubs_pts_topo, doubs$env) # convert dataframe to SpatRaster

# st_write(doubs_pts_env, "data/gisdata/doubs_pts_env.geojson",
# append=TRUE)


# 【解释】构建最终的“物种-环境-空间”数据。
# doubs$fish 是鱼类物种丰度表；
# rowSums(spe) 计算每个采样点的鱼类总丰度；
# spe$abund != 0 用来删除没有鱼类记录的样点。
# 最后把总丰度 abund、坐标 x/y、地形变量和环境变量整合到 env_spe_spa。
# the final spe-env data with spatial attributes
spe <- doubs$fish
spe$abund <- rowSums(spe) 
spe_clean <- spe[spe$abund != 0, ]

doubs_pts_env_clean <- 
  doubs_pts_env[spe$abund != 0, ]

env_spe_spa <- doubs_pts_env_clean %>%
  mutate(abund = spe_clean$abund) %>%
  relocate(abund, .before = geometry) %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  relocate(x, y, .before = doubs_dem)

# st_write(env_spe_spa, "data/gisdata/env_spe_spa.gpkg") # gpkg = spatial sqite


# 【解释】下面一整大段代码都被注释掉了，属于“空间多边形 ESDA 和空间模型”的示例材料。
# 它使用 spData 包中的 Columbus 区域犯罪数据，演示：
# 1）多边形邻接关系；
# 2）空间权重矩阵；
# 3）全局/局部空间自相关；
# 4）空间滞后模型、空间误差模型、地理加权回归。
# 因为全部以 # 开头，所以默认不会执行。
# # ########################################################
# # ## Methods for ESDA and ML of spatial polygons
# # #######################################################
# # # https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html
# # 
# # A) the global spatial autocorrelation
# 
# library(spData)
# library(sf)
# library(spdep)
# library(ggplot2)
# 
# map <- st_read(system.file("shapes/columbus.shp",
#                            package = "spData"), quiet = TRUE)
# plot(st_geometry(map), border = "lightgray")
# 
# map$vble <- map$CRIME # the focusing variable
# # mapview(map, zcol = "vble")
# 
# p_vble = # create the map
#   ggplot(map) +
#   geom_sf(aes(fill = map$vble)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
#   theme_minimal()
# 
# p_vble
# 
# 

# 【解释】示例：poly2nb(map, queen = TRUE) 根据多边形边界构造 Queen 邻接。
# Queen 邻接表示两个多边形只要共享边或角，就被认为是邻居。
# library(spdep)
# nb <- poly2nb(map, queen = TRUE) # determine adjacency
# 
# library(tmap)
# # examine zero links locations
# map$rn = rownames(map)
# tmap_mode("view")
# tm_shape(map) +
#   tm_borders() +
#   tm_text(text = "rn") +
#   tm_basemap("OpenStreetMap")
# tmap_mode("plot")
# 
# # Create a line layer showing Queen's case contiguity
# gg_net <- nb2lines(nb,coords=st_geometry(st_centroid(map)),
#                    as_sf = F)
# # Plot the contiguity and the map layer
# p_adj =
#   ggplot(map) + geom_sf(fill = NA, lwd = 0.1) +
#   geom_sf(data = gg_net, col='red', alpha = 0.5, lwd = 0.2) +
#   theme_minimal() + labs(subtitle =  "Adj")
# p_adj
# 
# # spatial weights Matrix and the lagged means

# 【解释】示例：把邻接关系 nb 转成空间权重矩阵 nbw。
# style = "W" 表示行标准化权重，即每个区域邻居权重之和为 1。
# lag.listw() 用邻居值的加权平均计算空间滞后变量。
# 
# nbw <- spdep::nb2listw(nb, style = "W") # compute weight matrix from nb
# nbw$weights[1:3]
# map$lagged_means <- lag.listw(nbw, map$vble) # compute lagged means
# p_lagged =
#   ggplot(map) + geom_sf(aes(fill = lagged_means)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
#   theme_minimal()
# 
# cowplot::plot_grid(p_vble, p_lagged)
# 
# p_lm = # create a lagged mean plot
#   ggplot(data = map, aes(x = vble, y = lagged_means)) +
#   geom_point(shape = 1, alpha = 0.5) +
#   geom_hline(yintercept = mean(map$lagged_means), lty = 2) +
#   geom_vline(xintercept = mean(map$vble), lty = 2) +
#   geom_abline() +
#   coord_equal()
# p_lm
# 
# # create a Moran plot and statistic test using weighted list

# 【解释】示例：Moran's I 用来检验一个变量是否存在空间自相关。
# 如果高值区域附近也是高值、低值区域附近也是低值，则通常是正空间自相关。
# moran.plot() 画的是变量值与空间滞后值之间的关系。
# moran.plot(x = map$vble, listw = nbw, asp = 1)
# 
# moran.test(x = map$vble, listw = nbw) # for Moran’s I for statistic test
# 
# moran.range <- function(lw) {
#   wmat <- listw2mat(lw)
#   return(range(eigen((wmat + t(wmat))/ 2) $values))
# }
# 
# moran.range(nbw) # strongly clustered
# 
# # B) local spatial autocorrelation and clusters

# 【解释】示例：Local Moran's I 用于识别局部空间聚集或离群。
# 常见类型包括 HH、LL、HL、LH：
# HH：高值周围也是高值；LL：低值周围也是低值；
# HL：高值点周围是低值；LH：低值点周围是高值。
# 
# # Compute the local Moran’s I
# map$lI <- localmoran(x = map$vble, listw = nbw)[, 1]
# 
# p_lisa = # create the map
#   ggplot(map) +
#   geom_sf(aes(fill= lI), lwd = 0.1) +
#   scale_fill_gradient2(midpoint = 0, name = "Local\nMoran's I",
#                        high = "darkgreen", low = "white") +
#   theme_minimal()
# p_lisa # print the map
# 
# # Create the local p values
# map$pval <- localmoran(map$vble,nbw)[, 5]
# map$pval
# 
# p_lisa_pval =
#   ggplot(map) +
#   geom_sf(aes(fill= pval), lwd = 0.1) +
#   scale_fill_gradient2(midpoint = 0.05,
#                        name = "p-values",
#                        high = "red", low = "white") +
#   theme_minimal()
# 
# p_lisa_pval # print the map
# 
# cowplot::plot_grid(p_lisa + theme(legend.position = "bottom"),
#           p_lisa_pval + theme(legend.position = "bottom"),
#           ncol = 2)
# 
# index  = map$pval <= 0.05
# p_vble + geom_sf(data = map[index,], fill = NA,
#                  col = "black", lwd = 0.5)
# 
# # Getis-Ord G statistic

# 【解释】示例：Getis-Ord G 统计量用于识别热点和冷点。
# 它关注局部区域内高值或低值是否显著聚集。
# 
# map$gstat <- as.numeric(localG(map$vble, nbw))
# 
# p_geto =
#   ggplot(map) +
#   geom_sf(aes(fill = gstat)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue",
#                        name = "G Statistic")+
#   theme_minimal()
# p_geto
# 
# # C) Incorporating spatial AC and heterogeneity into ML

# 【解释】示例：把空间自相关纳入回归建模。
# 普通线性回归 lm() 不显式考虑空间依赖；
# lagsarlm() 是空间滞后模型；
# errorsarlm() 是空间误差模型。
# 通过比较模型和残差 Moran's I，可以判断空间相关是否被解释掉。
# 
# # a) Simple Linear Regression
# 
# formula <- "vble ~ AREA + PERIMETER + HOVAL + INC + OPEN + X + Y"
# # compute model
# model1 <- lm(formula = formula, data = map)
# # view model statistics
# summary(model1)
# 
# # b) Spatial Regress Models accounting for spatial AC
# 
# model2 <- spatialreg::lagsarlm( # lag model
#   formula = formula,
#   data = map,
#   listw = nbw
# )
# 
# model3 <- spatialreg::errorsarlm( # error model
#   formula = formula,
#   data = map,
#   listw = nbw
# )
# 
# jtools::export_summs(model1, model2, model3) # compare to linear model
# 
# spdep::moran.test(model2$residuals, nbw) # model2 and models Moran's I test
# spdep::moran.test(model3$residuals, nbw)
# 
# # c) Geographically Weighted Regress accounting for heterogeneity
# # load packages
# library(SpatialML)
# library(GWmodel)
# map_sp <- map %>% # convert to sp object
#   as_Spatial()
# 
# library(tidyverse)
# library(sf)
# system.file("gpkg/nc.gpkg", package="sf") |>
#   read_sf() -> nc
# 
# nc1 <- nc |> mutate(SID = SID74/BIR74, NWB = NWBIR74/BIR74)
# lm(SID ~ NWB, nc1) |>
#   predict(nc1, interval = "prediction") -> pr
# bind_cols(nc, pr) |> names()


# 【解释】03 部分开始对 Doubs 鱼类数据做探索性空间数据分析 ESDA。
# 核心问题是：鱼类总丰度 abund 在空间上是否有聚集、离群或空间相关？
########################################################
# 03- the Exploratory Spatial Data analysis (ESDA)
########################################################
# 1) the EDA analysis on the table-data part


# the spatial dependence and heterogeneity


# 【解释】读取包含空间位置和鱼类/环境变量的 gpkg 文件。
# install.packages('spdep') 只需要首次安装时运行；
# 如果已经安装，建议注释掉，避免每次运行都重新安装。
# env_spe 是 sf 空间对象，geom_sf() 可快速检查点位分布。
# A) calculating the lagged mean and visualizing it
# https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-1.html

library(sf)
install.packages('spdep')
library(spdep)
library(ggplot2)
library(tidyverse)

env_spe <- st_read("data/gisdata/env_spe.gpkg")
par(mfrow = c(1,1))
ggplot(env_spe) +
  geom_sf()

# 【解释】提取每个采样点的 x/y 坐标，并把它们放入属性表。
# st_coordinates(.) 返回 sf 几何对象的坐标矩阵。
# relocate(x, y, .after = id) 将坐标列移动到 id 后面，方便查看。

env_spe_xy <- env_spe %>%
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
    ) %>%
  relocate(x, y, .after = id)


# 【解释】点数据本身没有多边形邻接关系，因此这里先用 Voronoi/Thiessen 多边形
# 为每个采样点生成一个空间影响区域。
# 后续再根据这些多边形是否相邻来建立邻接关系和空间权重矩阵。
# points -> voronoi polygons -> nb -> w

library(deldir)
library(sp)
vtess <- deldir(env_spe_xy$x, 
                env_spe_xy$y) # voronoi polygons
class(vtess) 

# 【解释】自定义函数 voronoipolygons_sp()：
# 输入 deldir 生成的 Voronoi 结果；
# tile.list() 提取每个 Voronoi 多边形的边界坐标；
# Polygon/Polygons/SpatialPolygons 把坐标转成 sp 多边形对象；
# SpatialPolygonsDataFrame 给多边形附加属性表。
plot(vtess, wlines = "tess", lty=1)

voronoipolygons_sp = function(thiess) {# voronoi polygons to sp
  w = tile.list(thiess) # extracting coordinates of polygons
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = sp::Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(
    SP, 
    data=data.frame(
      dummy = seq(length(SP)), 
      row.names=sapply(slot(SP, 'polygons'), 
                       function(x) slot(x, 'ID'))))
}

# 【解释】将 sp 格式的 Voronoi 多边形转换成 sf 格式。
# 这一步是为了使用 sf 的空间关系函数 st_relate() 构建邻接关系。

vtess_sp <- voronoipolygons_sp(vtess)
plot(vtess_sp)


# 【解释】自定义 Queen 邻接函数。
# st_relate(a, b, pattern = "F***T****") 使用 DE-9IM 空间关系模式，
# 用来判断多边形之间是否边界接触，从而生成类似 Queen 邻接的关系。
vtess_sf <- st_as_sf(vtess_sp) # converting sp to sf 
plot(vtess_sf$geometry)

st_queen <- function(a, b = a) { # Queen Contiguity Function

# 【解释】st_queen() 返回的是 sgbp（稀疏几何二元谓词）对象。
# spdep 的 nb2listw() 需要 nb 类型邻接表，因此这里写 as_nb_sgbp()
# 把 sgbp 转换成 spdep 可识别的 nb 对象。
  st_relate(a, b, pattern = "F***T****") # DE-9IM pattern
}

queen_sgbp <- st_queen(vtess_sf) # Sparse Geometry Binary Predicate
as_nb_sgbp <- function(x, ...) {# converting sgbp to nb
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}


# 【解释】把邻接表 queen_nb 转换为空间权重列表 queen_w。
# style = "W" 代表行标准化权重。
# summary(queen_w) 可查看每个点的邻居数量、权重分布等信息。
queen_nb <- as_nb_sgbp(queen_sgbp) #  Convert sgbp to nb

queen_w <- spdep::nb2listw(queen_nb, style = "W") # from nb to weights
queen_w$weights[1:3]
summary(queen_w)


# 【解释】计算鱼类丰度 abund 的空间滞后均值。
# lag.listw(queen_w, abund) 表示：
# 对每个采样点，计算其邻居点 abund 的加权平均值。
# lagged_mean_plot 用横轴表示自身丰度，纵轴表示邻居平均丰度，
# 可直观看出“高值是否靠近高值、低值是否靠近低值”。
# computing the lagged means of abund 
# https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html

env_spe_xy <- env_spe_xy %>%
  mutate(lagged_means_abund = lag.listw(queen_w, abund)) %>%
  relocate(lagged_means_abund, .before = geom)

lagged_mean_plot = 
  ggplot(data = env_spe_xy, 
         aes(x = abund, y = lagged_means_abund)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(env_spe_xy$lagged_means_abund), lty = 2) +
  geom_vline(xintercept = mean(env_spe_xy$abund), lty = 2) +
  geom_abline() +
  coord_equal()
lagged_mean_plot


# 【解释】计算全局 Moran's I。
# moran.test() 会返回 Moran's I、期望值、方差和显著性检验结果。
# zscore = (观测 Moran's I - 期望 Moran's I) / 标准差；
# 如果 zscore 大致落在 -1.96 到 1.96 之间，通常表示 0.05 水平下不显著。
# B) Global Moran's I and test if statistically significant
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html
# https://rpubs.com/laubert/SACtutorial

library(spdep)
gI <- moran.test(x = env_spe_xy$abund, 
                 listw = queen_w)#queen_w是空间权重

mI <- gI$estimate[[1]] # global moran's Index
eI <- gI$estimate[[2]] # Expected moran's index
var <- gI$estimate[[3]] # Variance of values
zscore <- (mI-eI)/var**0.5 
zscore # -1.96 <zscore <1.96, no spatial correlation


# 【解释】moran.range() 用空间权重矩阵的特征值范围辅助判断 Moran's I 的合理范围。
# listw2mat() 把权重列表转成矩阵；
# eigen() 求特征值。
# if moran.range between min-max, else cluster or dispersed
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/ 2) $values))
}

moran.range(queen_w) 


# 【解释】moran.plot() 画全局 Moran 散点图。
# 横轴是变量自身值，纵轴是空间滞后值；
# 斜率方向反映空间自相关方向。

moran.plot(x = env_spe_xy$abund, listw = queen_w, 
           asp = 1) 
title(main = "Global Moran's Scatter Plot")


# 【解释】计算 Local Moran's I。
# 全局 Moran's I 只能说明整体有没有空间自相关；
# Local Moran's I 可以告诉你哪些具体点位是显著的空间聚集或空间离群。
# C) Local Spatial Autocorrelation and test
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html#spatial-autocorrelation
# https://www.kaggle.com/code/jankuper192/spatial-regression

lI <- localmoran(env_spe_xy$abund, queen_w)


# 【解释】根据 Local Moran's I 的显著性和变量高低值，把样点分类为：
# HH：高值点周围也是高值；
# LL：低值点周围也是低值；
# HL：高值点周围是低值；
# LH：低值点周围是高值；
# Insignificant：局部空间关系不显著。
# derive the cluster/outlier types 
significanceLevel <- 0.05
meanVal <- mean(env_spe_xy$abund)

library(magrittr)
lisaRslt <- lI %>%  
  tibble::as_tibble() %>% 
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr()")) %>% 
  dplyr::mutate(coType = dplyr::case_when(
    `Pr()` > 0.05 ~ "Insignificant",
    `Pr()` <= 0.05 & Ii >= 0 & env_spe_xy$abund >= meanVal ~ "HH",
    `Pr()` <= 0.05 & Ii >= 0 & env_spe_xy$abund < meanVal ~ "LL",
    `Pr()` <= 0.05 & Ii < 0 & env_spe_xy$abund >= meanVal ~ "HL",
    `Pr()` <= 0.05 & Ii < 0 & env_spe_xy$abund < meanVal ~ "LH"
  ))

print(lisaRslt, n =29)


# 【解释】把 LISA 分类结果 coType 加回原始 sf 数据表。
# replace_na("Insignificant") 表示没有分类结果的点默认设为不显著。
# Now add this coType to the original sf
env_spe_xy$coType <- lisaRslt$coType %>%  
  tidyr::replace_na("Insignificant")


# 【解释】对自身丰度和空间滞后丰度做标准化。
# z-score = (原值 - 均值) / 标准差。
# 标准化后可以用 0 作为分界线，构造 LISA 四象限图。
# Standardize the variable and its spatial lag
env_spe_xy$z_abund <- 
  (env_spe_xy$abund - mean(env_spe_xy$abund)) / sd(env_spe_xy$abund)
env_spe_xy$z_laged_means_abund <- 
  (env_spe_xy$lagged_means_abund - mean(env_spe_xy$lagged_means_abund)) / sd(env_spe_xy$lagged_means_abund)


# 【解释】根据标准化后的自身值和空间滞后值划分四象限：
# 右上：High-High；左上：Low-High；
# 右下：High-Low；左下：Low-Low。
# Create a 'quadrant' variable to classify points
env_spe_xy$quadrant <- with(env_spe_xy, 
                            case_when(
                              z_abund>= 0 & z_laged_means_abund >= 0 ~ "High-High (HH)",
                              z_abund < 0 & z_laged_means_abund >= 0 ~ "Low-High (LH)",
                              z_abund >= 0 & z_laged_means_abund < 0 ~ "High-Low (HL)",
                              z_abund < 0 & z_laged_means_abund < 0 ~ "Low-Low (LL)"
                            )
)

ggplot(env_spe_xy, 

# 【解释】绘制 LISA 散点图。
# 横轴是标准化丰度，纵轴是标准化空间滞后丰度；
# 颜色表示 HH、LL、HL、LH 四类空间关系。
       aes(x = z_abund, y = z_laged_means_abund)) + # Create LISA plot
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(color = quadrant), shape = 16, alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c(
    "High-High (HH)" = "#E41A1C",
    "High-Low (HL)" = "#377EB8",
    "Low-High (LH)" = "#4DAF4A",
    "Low-Low (LL)" = "#984EA3"
  )) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  coord_equal() +
  labs(
    title = "LISA Scatter Plot with Quadrants",
    x = "Standardized Value (z-score)",
    y = "Standardized Spatial Lag",
    color = "LISA Type"
  ) +
  theme_minimal()

# 【解释】把 LISA 分类结果映射回地理空间位置。
# geom_sf(aes(color = coType)) 按 coType 给采样点着色，
# 用于观察空间聚集或离群点具体在哪里。


ggplot(env_spe_xy) +
  geom_sf(aes(color = coType), size = 2) +  # use color, not fill
  scale_color_manual(values = c('red', 'lightgray', 'blue', 'yellow'), 
                     name = 'Clusters & \nOutliers') +
  labs(title = "abundance of Fishes") +
  theme_minimal()

########################################################
# 04-spatial autocorrelation and heterogeneity into ML
#######################################################

# 【解释】04 部分把空间信息纳入机器学习/回归模型。
# 这里先做一个简单线性回归，把 x/y 坐标也作为解释变量，
# 用来粗略捕捉空间趋势。
# A) Simple Linear Regression with X and Y coordinates
# https://rpubs.com/zulfiqar_stat/1131164


# 【解释】读取已经整理好的 spe_env_spa.shp 空间数据。
# 这个数据应包含鱼类丰度、环境变量和空间位置。
# geom_sf() 用于查看点位空间分布。
# Loading data and packages
library(sf)
library(sp)
library(tidyverse)

spe_env_spa <- st_read("data/geo_data/spe_env_spa.shp")

ggplot(data = spe_env_spa) +
  geom_sf() +  # Plot the spatial data

# 【解释】从 sf 几何列中提取 x/y 坐标。
# st_drop_geometry() 删除空间几何列，把 sf 对象转成普通数据框；
# na.omit() 删除含缺失值的样本；
# subset(select = -points) 删除 points 字段；
# relocate(c("x","y")) 把坐标列放到前面。
  theme_minimal()  # Use a minimal theme
              
env_fish_xy <- spe_env_spa %>%
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
    ) 

env_fish_df <- env_fish_xy |>
  st_drop_geometry() |> # remove geometry
  na.omit() |> # omit NA
  subset(select = -points) |>
  relocate(c("x","y"))

str(env_fish_df)


# 【解释】这一段用于处理多重共线性。
# 先计算除响应变量 spe_abund 以外所有变量的 Pearson 相关矩阵；
# caret::findCorrelation() 找出相关系数超过 0.8 的变量；
# model_vars 删除这些高度相关变量，降低模型中变量冗余。
# removing correlated factors from doubs env
# env_factors <- env_fish_df |>
#   dplyr::select(-fish_abund)
# PerformanceAnalytics::chart.Correlation(env_factors, 
#                                         histogram = TRUE,  
#                                         pch = 19)

cor_matrix <- env_fish_df |>
  subset(select = -spe_abund) |>
  cor(use = "complete.obs", method = "pearson")

threshold <- 0.8
highly_correlated_vars <- 
  caret::findCorrelation(cor_matrix, 
                         cutoff = threshold, 
                         verbose = TRUE)


# 【解释】建立普通线性回归模型。
# formula = spe_abund ~ . 表示用 model_vars 中除 spe_abund 以外的所有变量预测 spe_abund。
# summary(model_lm) 查看回归系数、显著性、R²、残差等。
model_vars <- env_fish_df[, -highly_correlated_vars]
str(model_vars)

library(spatialreg)
formula = spe_abund ~.
model_lm <- lm(formula = formula, data = model_vars)
summary(model_lm) # degree of freedom: m =independent, n-m-1


# 【解释】下面开始构建“基于缓冲距离的空间随机森林”。
# 思路是：不直接只用 x/y 坐标，而是把“到不同丰度等级样点的距离”
# 构造成空间特征，再用随机森林预测鱼类丰度的空间分布。
# B) spatial random forest with only buffer distances


# 【解释】把普通数据框转换为 sp 空间点对象。
# coordinates(env_fish_sp) <- ~ x + y 指定 x/y 是空间坐标；
# proj4string() 指定投影坐标系为 UTM 31N。
# a) making a prediction grid (SpatialPixelsDataFrame)

env_fish_sp <- env_fish_df
coordinates(env_fish_sp) <- ~ x + y # df -> sp
class(env_fish_sp)
proj4string(env_fish_sp) <- CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")
plot(env_fish_sp)

# 【解释】设置预测网格分辨率。
# res <- 1000 表示网格间距为 1000 米。
# 下面根据观测点的 bbox 边界生成规则网格的 x/y 范围。

res <- 1000 # Set resolution
# Round bounding box to resolution
x_min <- bbox(env_fish_sp)[1,1] %/% res * res
x_max <- (bbox(env_fish_sp)[1,2] + res) %/% res * res
y_min <- bbox(env_fish_sp)[2,1] %/% res * res
y_max <- (bbox(env_fish_sp)[2,2] + res) %/% res * res
str(env_fish_sp@data)


# 【解释】expand.grid() 生成规则网格点坐标。
# 每个网格点都是将来要预测鱼类丰度的位置。
# Creating the grid coordinates
grid_df <- expand.grid(x = seq(x_min, x_max, by = res),
                       y = seq(y_min, y_max, by = res))
str(grid_df)


# 【解释】把 grid_df 转成 SpatialPixelsDataFrame。
# 先指定 coordinates，再设置 gridded(grid_sp) <- TRUE，
# R 就会把规则点阵识别为空间像元网格。
# converting the grid into a SpatialPixelsDataFrame

grid_sp <- grid_df
grid_sp$dummy <- 1 # Assigning dummy data for SpatialPixelsDataFrame
coordinates(grid_sp) <- ~ x + y # Converting to the SpatialPoints
gridded(grid_sp) <- TRUE # creating SpatialPixelsDataFrame
class(grid_sp)


# 【解释】把预测网格裁剪到 Doubs 河附近缓冲区。
# 先把 grid_sp 转成 sf，再读取河流线并重投影到 UTM，
# st_buffer() 创建河流缓冲区。
# b) clipping the grid region of doubs river
# Convert SpatialPixelsDataFrame to sf Object

grid_sf <- st_as_sf(grid_sp)
sf::st_crs(grid_sf) <- 32631

river <- sf::st_read("data/geo_data/doubs_river.shp")
river_utm <- st_transform(river, 32631) 
river_buff <- st_buffer(river_utm, dis = 8000)
plot(st_geometry(river_buff), axes = TRUE)


# 【解释】st_intersection(grid_sf, river_buff) 只保留落在河流缓冲区内的网格。
# 这样预测范围不会扩展到远离河流、没有生态意义的区域。
# clipping the grid limited to the river buffer area
clipped_grid <- st_intersection(grid_sf, river_buff)
plot(st_geometry(clipped_grid), axes = TRUE)
class(clipped_grid)
glimpse(clipped_grid)

# st_write(clipped_grid, 
#          "data/geo_data/clipped_grid.shp",
#          append=FALSE)


# 【解释】重新读取裁剪后的网格，并转换回 SpatialPixelsDataFrame。
# 后续 landmap::buffer.dist() 和 spplot() 等函数常使用 sp/raster 风格对象。
# back to the SpatialPixelsDataFrame format

clipped_grid <- st_read("data/geo_data/clipped_grid.shp")

clipped_grid_sp <- as(clipped_grid, "Spatial")
gridded(clipped_grid_sp) <- TRUE
class(clipped_grid_sp) # Check result

plot(clipped_grid_sp, pch = 20, cex = 0.5, # verify
     main = "Prediction Grid")

# saveRDS(clipped_grid_sp, "data/geo_data/clipped_grid_sp.rds")


# 【解释】把观测点的鱼类丰度 spe_abund 按分位数分级。
# seq(0,1,by=0.0625) 会生成 17 个分位点，也就是大约 16 个等级区间。
# cut() 根据这些分位数把每个样点划入一个丰度等级。
# c) Target quantiles  and distances to each quantile

(q_abund <- quantile(env_fish_df$spe_abund, 
                     seq(0,1,by=0.0625)))
classes_q_abund <- cut(env_fish_df$spe_abund, 
                                breaks=q_abund, 
                                ordered_result=TRUE, 
                                include.lowest=TRUE)
levels(classes_q_abund)

# 【解释】landmap::buffer.dist() 计算每个预测网格点到不同丰度等级样点的距离。
# 输入：
# env_fish_sp["spe_abund"]：带响应变量的观测点；
# clipped_grid_sp：预测网格；
# classes_q_abund：观测点所属丰度等级。
# 输出 grid_dist 是多个距离栅格层，每一层表示到某个丰度等级的距离。


grid_dist <- landmap::buffer.dist(env_fish_sp["spe_abund"],
                                  clipped_grid_sp, 
                                  classes_q_abund)
head(grid_dist)
dim(grid_dist)

# save(grid_dist, # Save as R data object
#      file = "data/geo_data/grid_dist.RData")

# saveRDS(grid_dist, # Save as RDS file
#         file = "data/geo_data/grid_dist.rds")

load("data/geo_data/grid_dist.RData")
summary(grid_dist)

plot(raster::stack(grid_dist))


# 【解释】把网格距离特征提取到原始观测点位置。
# over(env_fish_sp, grid_dist) 会得到每个真实采样点对应的距离特征。
# 再与原始环境变量表合并，形成可用于训练随机森林的数据 env_fish_dist。
# exacting distance to each point
buffer_dists <- over(env_fish_sp, grid_dist)
dim(buffer_dists)
dim(env_fish_sp)
buffer_dists[1,]

env_fish_dist <- cbind(env_fish_sp@data, buffer_dists)
str(env_fish_dist)

# write.csv(env_fish_dist, "data/geo_data/env_fish_dist.csv")


# 【解释】训练仅使用“距离缓冲特征”的随机森林模型。
# dn 把 grid_dist 的所有特征名拼成公式右侧；
# fm = spe_abund ~ 距离特征1 + 距离特征2 + ...
# randomForest() 用这些空间距离特征预测鱼类丰度。
# d) buffer distances-based Spatial random forest 

env_fish_dist <- read.csv("data/geo_data/env_fish_dist.csv", row.names = 1)
str(env_fish_dist)

set.seed(123)
dn <- paste(names(grid_dist), collapse="+")
# str(grid_dist@data)
(fm <- as.formula(paste("spe_abund ~", dn)))

library(randomForest)
set.seed(123)
(model_rf <- randomForest(fm, # rf for predicting obs
                    env_fish_dist, 
                    importance=TRUE, 
                    min.split=5, 

# 【解释】模型预测与训练集拟合效果评估。
# predict(model_rf, newdata=env_fish_dist) 得到观测点上的预测丰度；
# plot(model_rf) 查看随机森林误差随树数量变化；
# varImpPlot() 查看变量重要性；
# 预测值 vs 实际值图越靠近 1:1 线，说明拟合越好。
# 注意：rmse_rf 这一行原代码用了 env_fish_dist$fish_abund，
# 但前面响应变量叫 spe_abund，这里很可能应改为 env_fish_dist$spe_abund。
                    mtry=5, 
                    ntree=800))

pred_rf <- predict(model_rf, newdata=env_fish_dist) 
plot(model_rf)
varImpPlot(model_rf, type=1)
plot(env_fish_sp$spe_abund ~ pred_rf, 
     asp=1, 
     pch=20, 
     xlab="Random forest fit", 
     ylab="Actual value", 
     main="fish abundance")
abline(0,1); grid(nx=30,ny=30)
(rmse_rf <- 
    sqrt(sum((pred_rf-env_fish_dist$fish_abund )^2)/length(pred_rf)))


# 【解释】把训练好的随机森林模型应用到整个预测网格。
# newdata=grid_dist@data 表示对每个网格像元的距离特征进行预测；
# clipped_grid_sp$model_rf 保存每个网格点的预测鱼类丰度；
# spplot() 把预测结果画成空间分布图。
# e) mapping the prediction on the distance grid 

pred_grid <- predict(model_rf, 
                     newdata=grid_dist@data)
str(grid_dist@data)


clipped_grid_sp$model_rf <- pred_grid
str(clipped_grid_sp@data)
breaks <- seq(2, 90, by=.5)
p1 <- spplot(clipped_grid_sp, # SpatialPixelsDataFrame of grid
            zcol="model_rf", 
             main="fish abund", 
             sub="RRF 16 distance buffers", 
             at=breaks)
print(p1)


# 【解释】D 部分进一步加入协变量 covariates。
# 上一模型只使用“到不同丰度等级样点的距离”；
# 这里把地形、环境等共同变量也加入随机森林，提高解释能力和预测能力。
# D) Spatial random forest on buffer distances and cocovariates


# 【解释】读取地形/环境协变量栅格 topo_char.tif，
# 并在预测网格 clipped_grid 上提取对应值。
# grid_topo_dist 把协变量和距离特征合并到同一个预测网格数据表中。
# extracting raster values
topo_char <- terra::rast("data/geo_data/topo_char.tif")

grid_topo <- terra::extract(topo_char, clipped_grid, 
                                    ID=FALSE)
str(grid_topo)
grid_topo_dist <- cbind(grid_topo, grid_dist)
str(grid_topo_dist)

grid_topo_sp <- grid_topo_dist
coordinates(grid_topo_sp) <- ~ x + y # Converting to the SpatialPoints
gridded(grid_topo_sp) <- TRUE # creating SpatialPixelsDataFrame


# 【解释】自动找出观测点数据和预测网格数据中共有的协变量名。
# covars 是这些协变量拼成的公式字符串；
# fm_covars = spe_abund ~ 距离特征 + 协变量。
# 这样随机森林既利用空间距离结构，又利用环境/地形解释变量。
# spatial random forest on buffer distances and co_vars 

(covars <- paste(intersect(names(env_fish_sp@data), 
                           names(grid_topo_sp)), 
                 collapse="+"))

(fm_covars <- as.formula(paste("spe_abund ~", 
                               dn, 
                               "+", 
                               covars)))

(model_rf_covars <- randomForest(fm_covars, 
                                 env_fish_dist,

# 【解释】训练加入协变量后的随机森林，并在观测点上预测。
# varImpPlot() 可以比较距离特征和环境协变量的重要性。
# 注意：rmse_rf_covars 这一行同样用了 fish_abund，
# 如果数据中没有 fish_abund，通常应改为 spe_abund。
                                 importance=TRUE, 
                                 min.split=5, 
                                 mtry=5, 
                                 ntree=1000))

pred_rf_covars <- predict(model_rf_covars, 
                          newdata=env_fish_dist) 
plot(model_rf_covars)
varImpPlot(model_rf_covars, type=1)
plot(env_fish_dist$spe_abund ~ pred_rf_covars, 
     asp=1, 
     pch=20, 
     xlab="Random forest fit with covars", 
     ylab="Actual value", 
     main="fish abundance")
abline(0,1); grid(nx=30,ny=30)
(rmse_rf_covars <- 
    sqrt(sum((pred_rf_covars-env_fish_dist$fish_abund )^2)/length(pred_rf_covars)))


# 【解释】把“距离特征 + 协变量”的随机森林模型预测到整个网格。
# grid_topo_sp$model_rf_covars 保存预测值；
# spplot() 输出最终空间预测图，可以与仅距离特征模型 p1 对比。
# mapping the prediction on the distance grid 

pred_grid_covars <- predict(model_rf_covars, 
                     newdata=grid_topo_sp@data)

grid_topo_sp$model_rf_covars <- pred_grid_covars
str(grid_topo_sp@data)
breaks <- seq(2, 90, by=.5)
p2 <- spplot(grid_topo_sp, # SpatialPixelsDataFrame of grid
            zcol="model_rf_covars", 
            main="fish abund", 
            sub="RRF 16 distance buffers", 
            at=breaks)
print(p2)
# ============================================================
# 【整份代码的整体逻辑总结】
#
# 这份脚本可以分成四条主线：
#
# 1. 空间数据读取与可视化
#    - 使用 sf 读取河流、采样点等矢量数据；
#    - 使用 terra 读取 DEM 等栅格数据；
#    - 使用 ggplot2 / terra::plot / plot() 检查图层是否对齐。
#
# 2. 空间特征提取
#    - 把河流、采样点、DEM 统一到 UTM 31N 坐标系；
#    - 沿河流建立缓冲区；
#    - 裁剪 DEM；
#    - 通过 QGIS/SAGA 算法从 DEM 提取坡度、汇流面积等地形变量；
#    - 将这些栅格变量提取到采样点位置，并与鱼类丰度、环境变量合并。
#
# 3. 探索性空间数据分析 ESDA
#    - 用 Voronoi 多边形为点数据构建邻接关系；
#    - 用 nb2listw() 构建空间权重矩阵；
#    - 计算空间滞后均值；
#    - 使用全局 Moran's I 判断整体空间自相关；
#    - 使用 Local Moran's I / LISA 判断局部 HH、LL、HL、LH 聚集或离群。
#
# 4. 空间机器学习建模
#    - 普通线性回归：把 x/y 坐标和环境变量作为解释变量；
#    - 空间随机森林：
#      先构建预测网格；
#      再计算网格点到不同丰度等级观测点的距离；
#      用这些距离特征训练随机森林；
#      最后把模型预测到整个空间网格上；
#    - 加入地形/环境协变量后，可建立更完整的空间预测模型。
#
# ============================================================
# 【几个重要注意点】
#
# 1. CRS 必须统一：
#    DEM、河流、采样点、预测网格必须处在同一个 CRS 中。
#    如果一个是 EPSG:4326 经纬度，一个是 EPSG:32631 米制坐标，
#    crop、mask、extract、buffer 等操作都可能出错。
#
# 2. 缓冲区距离单位：
#    st_buffer(doubs_river_utm, dis = 8000) 的单位是米。
#    因此这是 8 km，不是注释中的 5 km。
#
# 3. QGIS/SAGA 依赖：
#    qgisprocess::qgis_run_algorithm() 需要本机正确安装 QGIS，
#    并且 QGIS 能找到 SAGA 算法。
#    如果 qgis_search_algorithms("wetness") 找不到算法，
#    说明 QGIS/SAGA 配置可能有问题。
#
# 4. sp 与 sf 对象混用：
#    脚本中同时使用 sf、sp、terra、raster 风格对象。
#    常见转换包括：
#    st_as_sf()：sp 转 sf 或普通空间对象转 sf；
#    as(obj, "Spatial")：sf 转 sp；
#    coordinates(df) <- ~ x + y：普通数据框转 sp 点对象。
#
# 5. RMSE 变量名可能有误：
#    原代码中 RMSE 使用 env_fish_dist$fish_abund，
#    但前文模型公式和数据中更像是 spe_abund。
#    如果运行报错 “object fish_abund not found”，
#    应检查是否需要改成 env_fish_dist$spe_abund。
#
# 6. 训练/测试集问题：
#    空间随机森林部分主要是在训练点上评估拟合效果，
#    不等于严格的外部验证。
#    如果要评价预测能力，建议进一步做训练集/测试集划分，
#    或做空间交叉验证 spatial cross-validation。
#
# ============================================================
