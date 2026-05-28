# --------------------------------------------
# Script Name: Exploratory data analysis on doubs
# Purpose: This section is to show how to do EDA for 
#          finding eco-patterns. The data analysis is 
#          to take the dataset of doubs as an example,
#          and do EDA on the data of a community.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-05
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################
# 01-getting doubs OMS data for a map
##################################################

# A) using qgis 
# https://www.youtube.com/watch?v=gahG3OAdZQs
# // installing plugins: quickmapservices and quickOSM
# quickservice -> metasearch -> add default (basic map)

# // filling key-value in QUICKOSM for downloading 
# quickOSM -> waterway and river -> doubs →runs

# // obtaining Le Doubs river by selection features
# open attribution table -> selection by Expression 
# "name" LIKE '%Le Doubs%'

# // copying and pasting features for saving the river
# Edit -> Copy -> paste features as

# //saving the doubs OSM data to postgresql
# https://www.youtube.com/watch?v=H9o0wme0nuk

# B) using R 
# asking chatGPT for downloading river data from OSM

"based on openstreetmap data, write R code to find Le Doubs
in France-Switzerland, and use mapview to visulize it on a map"

# Installing and loading necessary packages

library(osmdata) # downloading data from OSM
library(mapview) # interactively visualizing spatial data
library(dplyr)
# getting the bounding box for Le Doubs river

bbox <- c(left = 5.5, bottom = 46.5, right = 7.5, top = 48)
#定义一个经纬度范围，也就是搜索Doubs河的大致范围
# left 左边界经度；right:右边界经度；bottom:下边界纬度
# Query OSM for  "Le Doubs" waterways in a bounding box
DOUBS_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "waterway", value = "river") %>% # 表示只查河流
  add_osm_feature(key = "name", value = "Le Doubs") #OSM 查询条件，表示只查名字叫 Le Doubs 的河流
DOUBS_query

# set_overpass_url("https://lz4.overpass-api.de/api/interpreter") # select a url
osm_data <- osmdata_sf(DOUBS_query) # getting the data,真正执行查询，把 OSM 数据下载下来，并转成 sf 空间数据格式
DOUBS_river <- osm_data$osm_lines # extracting river geometry (lines)
#提取其中的线状要素，因为河流一般是线
class(DOUBS_river) # Inspecting,查看这个对象是什么类型
# mapview(DOUBS_river, color = "blue", lwd = 2) # Visualizating
library(ggplot2)
ggplot(data = DOUBS_river) +
  geom_sf(color="blue")

# st_write(DOUBS_river, "data/gisdata/DOUBS_river.gpkg")
# st_write(DOUBS_river, "doubs.geojson")

##################################################
# 02-loading fish-env data and pre-Processing them
##################################################
# 1) loading the Doubs data from postgresql

# library(DBI) # helps connecting R to database
# library(RPostgreSQL) # provides an interface with SQLite
library(DBI)
#con <- dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = 'myclasses',
                          host = 'localhost',
                          port = 5432,
                          user = 'postgres')
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connect
                       dbname = 'myclasses',
                       host = 'localhost',
                       port = 5432,
                       user = 'postgres',
                       password = 'root')
dbListTables(con)
dbListFields(con, "doubs_env") # List fields of doubs_env table

# ?dbReadTable
doubs_spe <- dbReadTable(conn = con,"doubs_spe")
doubs_env <- dbReadTable(conn = con,"doubs_env")
doubs_spa <- dbReadTable(conn = con,"doubs_spa")
dbGetInfo(con)
dbDisconnect(con)


# 2) pre-Processing of the fishes and env data 鱼类数据预处理

# A) about the data of fish community

# a. deleting the rows without fishes

str(doubs_spe) # structure of objects 数据结构
head(doubs_spe) # first 6 rows
summary(doubs_spe) # summary statistics查看最大值，最小值等
dim(doubs_spe) # dimensions查看行数和列数
names(doubs_spe) # Names of objects列名
#删除没有鱼的采样点
row_sums <- rowSums(doubs_spe)
which(row_sums == 0)
spe_clean <- doubs_spe[-8,] # remove the sites with no fish
spe_clean

# spe_clean <- doubs_spe %>%
#   filter(rowSums(.) != 0)

str(doubs_env) # structure of objects
summary(doubs_env) # summary statistics
head(doubs_env) # first 6 rows
dim(doubs_env) # dimensions
names(doubs_env) # Names of objects
# 环境数据和空间数据同步删除第8行
env_clean <- doubs_env[-8,] # remove site with no fish
spa_clean <- doubs_spa[-8,] # remove site with no fish

# b. frequency distribution and transformation 鱼类丰度和数据转换

library(vegan) #生态学数据分析里非常常用的包，用来做群落分析、距离矩阵、PCA、RDA、CCA 等 

range(spe_clean)  #查看鱼类丰度的最小值和最大值
(ab <- table(unlist(spe_clean)))
# df→vector→table→output
# unlist(spe_clean) 把整个数据框压成一个长向量
# table() 统计每个丰度值出现了多少次
barplot(ab,
        las = 1, # set labels to horizontal 表示坐标轴标签横着显示
        xlab = "Abundance class",
        ylab = "Frequency",
        col = gray(5 : 0 / 5) #灰度颜色
)
sum(spe_clean == 0) / (nrow(spe_clean) * ncol(spe_clean))
# 计算0的比列
# Transforming the fish community data

# spe_pa <- decostand(spe_clean, method = "pa") 对鱼类数据做转换
spe_hel <- decostand(spe_clean, method = "hellinger") # 适合处理很多0的物种丰度，Hellinger 转换后更适合做 PCA、RDA 等线性方法
spe_log <- decostand(spe_clean,method = "log") # log可以压缩极端大值的影响
# B) detecting and replacing outliers in the env columns 环境变量异常值检测

# # detecting outlier for each variable
dfs <- env_clean$dfs
Q1 <- quantile(dfs, 0.25) #第一四分数
Q3 <- quantile(dfs, 0.75) #第三四分数
IQR <- Q3 - Q1 #四分位距
IQR
# Lower and Upper Bounds 用箱线图规则判断异常值
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- dfs[dfs < lower_bound | dfs > upper_bound]
print(outliers)

par(mfrow= c(1, 2)) # 一行画两个图
boxplot(dfs, ylab = "dfs")
boxplot(dfs, ylab = "dfs", horizontal = TRUE)
par(mfrow= c(1, 1)) #恢复成一图一个窗口

# a. detecting all columns of a dataframe and replacing with NA
# 检查环境中的异常值
library(dplyr)
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

outlier <- env_clean %>%
  mutate(across(where(is.numeric), detect_outliers)) #对环境数据中所有数值列都应用 detect_outliers()
outlier

boxplot(env_clean, horizontal = TRUE, 
        main = "Boxplot for all variables")
# 把异常值替换为NA
library(dplyr)
replace_outliers <- function(x) {
  if (!is.numeric(x)) return(x) #如果这一列不是数值列，就不处理，直接返回原数据
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[x < lower | x > upper] <- NA
  return(x)
}

env_cleanNA <- env_clean %>%
  mutate(across(everything(), replace_outliers))
env_cleanNA

# b. filled NA using mean for each column 用均值填补NA

env_replaced <- env_cleanNA %>%
  mutate(across(where(is.numeric), #只处理数值列
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) #如果是NA，就替换成均值，如果不是就保留原值
env_replaced

# 3) pre-explorating the relationship between fishes and env
# A) the distribution of sampling locations

plot(spa_clean, # using plot(…) function with lines(…), 
     # points(…), text(…), polygon(…) etc. 
     # to build sophisticated graphs.
     asp = -1, #纵横比列
     type = "p", # a plot point data
     main = "Sampling sites",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)"
)

lines(spa_clean, col = "light blue") # Add a line/labels/text 采样点连成线
text(spa_clean, row.names(spa_clean), cex = 1.0, col = "red")

text(40, 20, "Upstream", cex = 1.2, col = "blue")
text(15, 120, "Downstream", cex = 1.2, col = "blue")

# B) the distribution of indicative fishes 典型鱼类空间分布
par(mfrow=c(2,2)) # Plot four species
xl <- "x coordinate (km)"
yl <- "y coordinate (km)"
plot(spa_clean, asp=1, col="brown", cex=spe_clean$Neba,  #cex 控制点的大小。某个采样点 Neba 丰度越高，点越大
     main="Stone loach", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, col="brown", cex=spe_clean$Cogo, 
     main="European bullhead", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, col="brown", cex=spe_clean$Baba, 
     main="Barbel", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, col="brown", cex=spe_clean$Abbr, 
     main="Common bream", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
par(mfrow=c(1,1))

# the distribution of typic Environmental Variables画海拔alt,流量flo，溶解氧oxy,硝酸盐nit的空间分布

par(mfrow=c(1,4))
plot(spa_clean, asp=1, main="Altitude", pch=21, col="white",
     bg="red", cex=5*env_replaced$alt/max(env_replaced$alt), xlab="x", ylab="y")
#cex 点越大，说明海拔越高
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Discharge", pch=21, col="white",
     bg="blue", cex=5*env_replaced$flo/max(env_replaced$flo), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Oxygen", pch=21, col="white",
     bg="green3", cex=5*env_replaced$oxy/max(env_replaced$oxy), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Nitrate", pch=21, col="white",
     bg="brown", cex=5*env_replaced$nit/max(env_replaced$nit), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
par(mfrow=c(1,1))

############################################
# 03 Q- and R-mode analysis on fish and env
###########################################
# 1) for the env data
# Q-mode：分析样本之间的关系。行和行
# 比如不同采样点之间，环境条件是否相似，鱼类组成是否相似
# A) R-mode (relationships among variables or columns)分析变量之间的关系。列和列
# 比如环境变量中，海拔、流量、氧气、硝酸盐之间是否相关
# https://www.rpubs.com/dvallslanaquera/pca

# standardizing env variables (z-score) 
library(vegan)
# 标准化和相关性分析
env_z <- decostand(env_replaced, "standardize") # 标准化
apply(env_z, 2, mean) # means = 0 # 其最终2代表对列求均值
apply(env_z, 2, sd) # standard deviations = 1
vif.cca()
 #vif.cca() 需要输入一个 cca 或 rda 模型对象，不能空着用
# ep: model <- rda(spe_hel ~ ., data = env_z)
# vif.cca(model)
# # env_z equal to env_scaled <- scale(env_replaced)
# apply(env_scaled, 2, mean) # means = 0
# apply(env_scaled, 2, sd) # standard deviations = 1

# the correlation analysis

PerformanceAnalytics::chart.Correlation(env_z, 
                                        histogram = TRUE, 
                                        pch = 19) # collinearity,画环境变量之间的相关性图
# B) Q-mode (dissimilarity among sites or rows) 采样点聚类

par(mfrow = c(1,1))
env_d <- dist(env_z) # from built-in stats package 计算环境变量标准化后，不同采样点之间的距离,默认为欧氏距离
env_d_single <- hclust(env_d, method = "single") # 对采样点做层次聚类，method = "single" 表示单连接法
plot(env_d_single)  # plot() 画出聚类树

# C) R- and Q-modes' PCA

pca_env1 <- prcomp(env_z) # from built-in stats package prcomp(env_z)做PCA主成分分析
summary(pca_env1) # variance explanation #查看每个主成分解释多少方差
pca_env1$rotation # variable contribution 查看变量在主成分上的载荷
pca_env1$x # sample scores 查看每个采样点的主成分得分
biplot(pca_env1) # dot=pca$x; arrow=pca$ration 画 PCA 双标图
biplot(pca_env1, scale = 0) # for samples, similar to Q-mode
biplot(pca_env1, scale = 1) # for variables, similar to R-mode

# using rda() from vegan package
pca_env2 <- vegan::rda(env_z)  # 用 vegan::rda() 做 PCA
biplot(pca_env2, scaling = 1) # 1 for samples
biplot(pca_env2, scaling = 2) # 2 for variables

# vegan::decorana(env_clean) # DCA1 >4 Unimodal, DCA1 <3 linear 
# env_pca <- rda(env_clean, # run PCA, same to rda(env_z)
#                scale = TRUE) # calls a standardization
# 
# biplot(env_pca, scaling =1, # Q-mode
#        main="scaling=1: object similarity") 
# biplot(env_pca, scaling =2, # R-mode
#        main="scaling=2: variable correlation") 

# 2) for the spe data
# A) R-mode analysis

spe_hel <- decostand(spe_clean, "hellinger")
cor_mat <- cor(spe_hel) # co-occurrence 计算物种之间相关系数矩阵
heatmap(cor_mat)

# B) Q-mode analysis

# a. for the quantitative data

# bray-curtis dissimilarity with raw data
spe_db <- vegan::vegdist(spe_clean)	# Bray-Curtis as default 计算鱼类群落的 Bray-Curtis 距离
# 距离越小，说明两个采样点的鱼类组成越相似。
# 距离越大，说明鱼类组成越不同
# equal to vegdist(spe_clean, "bray")	
spe_db

# hellinger dissimilarity 
spe_hel <- decostand(spe_clean, "hel")
spe_dh <- vegdist(spe_hel, method="euclidean")  # 欧氏距离

# b. for the presence-absence data

# Jaccard matrix
spe_dj <- dist(spe_clean, "binary")  #计算 Jaccard 距离
# equal to vegdist(spe_clean, "jac", binary=TRUE)
spe_dj 
# Bray-Curtis：看丰度差异；
# Jaccard：看出现/不出现差异；
# Hellinger + Euclidean：适合后续 PCA/RDA 的群落距离分析

# 鱼类群落层次聚类
spe_db_single <- hclust(spe_db, method = "single") #基于 Bray-Curtis 距离，用单连接法聚类
plot(spe_db_single) 

spe_db_complete <- hclust(spe_db, method = "complete") #完全连接法聚类
plot(spe_db_complete) 

spe_db_ward <- hclust(spe_db, method = "ward.D2") # 用 Ward 方法聚类
plot(spe_db_ward) 

library(vegan)
spe_dh <- vegdist(spe_hel, method="euclidean") 
par(mfrow=c(1,1))
spe_dh_single<-hclust(spe_dh, method="single")
plot(spe_dh_single, main="Single linkage clustering", 
     hang =-1) #hang = -1 表示让聚类树底部的标签对齐，看起来更整齐

spe_dh_complete <- hclust(spe_dh, method = "complete")
plot(spe_dh_complete, main="Complete linkage clustering", 
     hang=-1) 

# C) R- and Q-modes' PCA 对鱼类群落数据做PCA
# 这里 PCA 的对象是 Hellinger 转换后的鱼类丰度矩阵
pca_spe_prcomp<- prcomp(spe_hel)
summary(pca_spe_prcomp) 
pca_spe_prcomp$rotation # 表示物种在各主成分上的载荷，如果某个物种在 PC1 上数值很大，说明它对 PC1 这个变化方向贡献很大
#正值和负值说明在PC1方向上相反
pca_spe_prcomp$x  # 表示每个采样点在 PCA 主成分上的得分
biplot(pca_spe_prcomp) 

# using rda() from vegan package

spe_pca_rda <- rda(spe_hel) # run pca with rda() 用rda实现PCA
summary(spe_pca_rda, scaling = 2) 
summary(spe_pca_rda, scaling = 1) 

biplot(spe_pca_rda, scaling =1, main="PCA scaling=1") # Q-mode
biplot(spe_pca_rda, scaling =2, main="PCA scaling=2") # R-mode

# 3) RDA--the relationship between spe and env 分析鱼类群落和环境变量之间的关系
spe_hel <- decostand(spe_clean, "hellinger")
vegan::decorana(spe_hel) # DCA1 >4 Unimodal, DCA1 <3 linear 
# decorana() 做 DCA 分析，用来判断应该用线性模型还是单峰模型
# A) doing RDA with manually removed vif > 10 手动删除VIF高的变量后做RDA
# initial RDA analysis 建立 RDA 模型
env_spe_rda <- rda(spe_hel ~ ., data = env_z)  #用所有环境变量解释鱼类群落组成
vif.cca(env_spe_rda) # 用来计算方差膨胀因子 VIF，检查多重共线性
# 如果某个变量 VIF > 10，通常说明它和其他变量高度相关，可以考虑删除

# for further optimizing RDA
vif.cca(env_spe_rda) # deleting vif >10 variables
env_z_selected <- env_z %>% # remove VIF > 10
  select(-dfs, -alt) #删除 dfs 和 alt 两个环境变量（vif > 10）
env_spe_rda_sel <- rda(spe_hel ~ ., 
                       data = env_z_selected) 
vif.cca(env_spe_rda_sel) 
 #用删除高 VIF 变量后的环境数据重新做 RDA

#做置换检验
anova.cca(env_spe_rda_sel, permutations = 999) # Goodness of fit 检验整个 RDA 模型是否显著
anova.cca(env_spe_rda_sel, by = "term", permutations = 999) # 检验每个环境变量是否显著
# permutations = 999代表随机置换999次

plot(env_spe_rda_sel, scaling=1, main="scaling 1")# Scaling 1
plot(env_spe_rda_sel, main="scaling 2")# Scaling 2 最好写scaling 2

# B) doing RDA with automatically selected by ordiR2step
# which maximizes R2
env_spe_rda_null <- rda(spe_hel ~ 1, data = env_z) # 空模型，不包含任何环境变量
env_spe_rda_all <- rda(spe_hel ~ ., data = env_z) # 全模型
(env_spe_rda_pars <- # Parsimonious model
    vegan::ordiR2step(env_spe_rda_null, # 如果两个采样点的 PCA 得分接近，说明它们的鱼类群落组成相似
               scope = formula(env_spe_rda_all),  # 表示最多可以选择到全模型里的变量范围
               direction = "forward", # 表示前向选择
               pstep = 999))
summary(env_spe_rda_pars)

# 比较两个 RDA 模型的调整 R²
RsquareAdj(env_spe_rda_pars)$adj.r.squared
RsquareAdj(env_spe_rda_sel)$adj.r.squared

#对自动选择出来的 RDA 模型做显著性检验
anova.cca(env_spe_rda_pars, permutations = 999) # 检验整个模型
anova.cca(env_spe_rda_pars, permutations = 999, by = "term") # 检验模型中的每个环境变量

par(mfrow = c(1, 2)) # plot triplot
plot(env_spe_rda_pars, scaling = 1, 
     # sp(species), lc(linear constraints),cn(constrained variables)
     display = c("sp", "lc", "cn"),  #sp物种，lc样点在线性约束轴上的得分，cn环境变量箭头
     main = "Scaling 1") # Scaling 1

plot(env_spe_rda_pars, 
     display = c("sp", "lc", "cn"), 
     main = "Scaling 2") # Scaling 2
graphics.off()
#1. 获取 Doubs 河 OSM 地图数据
#2. 从 PostgreSQL 读取鱼类、环境、空间坐标数据
#3. 删除没有鱼的采样点
# 4. 检查鱼类丰度分布，并做 Hellinger/log 转换
# 5. 检测环境变量异常值，并用均值填补
# 6. 画采样点、鱼类、环境变量的空间分布图
# 7. 对环境数据做相关性、聚类和 PCA
# 8. 对鱼类群落做相关性、距离矩阵、聚类和 PCA
# 9. 用 RDA 分析环境变量如何解释鱼类群落变化