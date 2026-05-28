# ============================================================
# 文件名：09_net_eda_ml_代码解释版.R
# 来源脚本：09_net_eda_ml (1)(3).R
# 说明：本文件在原始 R 代码基础上加入中文注释，用于解释每一段代码的含义。
# 主题：生态网络分析、共现网络构建、网络指标分析、链路预测、随机森林与 Keras 深度学习。
# ============================================================

# --------------------------------------------
# Script Name: Eco-network analysis
# Purpose: This script is shown how to construct a
#          eco-network and analyze the eco-network
#          properties, as well as predict possible
#          links of the econetwork.
#
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-29
# --------------------------------------------

# 【代码含义】清空 RStudio 控制台，并删除当前环境中已有的所有对象，保证后面的代码从干净环境开始运行。
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables


###############################################################
# 01- Econetwork graph representation and visualization
###############################################################

# 【本节总体含义】
# 这一部分演示生态网络或一般网络数据的三种常见表示方法：
# 1. 边列表 edgelist：每一行表示一条边，例如 from 节点连接到 to 节点；
# 2. 邻接矩阵 adjacency matrix：用矩阵中的数值表示节点之间是否连接以及连接强度；
# 3. 关联矩阵 / 二部图矩阵 incidence matrix：常用于两类节点之间的关系，例如蜜蜂-植物互作网络。


# ------------------------------------------------------------
# A) As the format of a spreadsheet or edgelist
# ------------------------------------------------------------

# 【代码含义】手动构造一个简单的边列表示例。
# from 表示起点节点，to 表示终点节点，weight 表示边的权重，也可以理解为两个节点之间联系的强弱。
from <- c("1","1","2","2","3","4","4","5")
to <- c("2","4","3","4","4","5","6","6")
weight <- c(0.1,0.5,0.8,0.2,0.4,0.9,1.0,0.5)

# 【代码含义】把 from、to、weight 三个向量合并成一个数据框 edgelist。
# 这个数据框每一行是一条边，例如第 1 行表示节点 1 和节点 2 之间有一条权重为 0.1 的边。
edgelist <- data.frame(from, to, weight)

# 【代码含义】这一行被注释掉了。
# 如果取消注释，就会把手动构造的边列表保存成 CSV 文件。
# write.csv(edgelist, "data/netdata/edgelist.csv", row.names = F)

# 【代码含义】从外部 CSV 文件读取边列表。
# readr::read_csv() 是 tidyverse 体系中读取 CSV 的函数。
# show_col_types = FALSE 表示不显示每一列被识别成什么数据类型的提示信息。
edgelist <- readr::read_csv("data/netdata/edgelist.csv",
                            show_col_types = FALSE)

# 【代码含义】在控制台显示读取到的边列表，便于检查文件是否正确读入。
edgelist

# 【代码含义】加载 igraph 包。
# igraph 是 R 中非常常用的网络分析包，可以构建、可视化和计算网络指标。
library(igraph)

# 【代码含义】根据边列表构建网络图对象。
# d = edgelist 表示输入数据是边列表。
# directed = F 表示构建无向图，即 from-to 和 to-from 视为同一种连接。
edgelist_g <- graph_from_data_frame(d = edgelist, 
                                    directed = F)

# 【代码含义】检查这个图对象是否带有边权重。
# 如果边列表中有 weight 列，igraph 通常会自动把它识别为边属性。
is_weighted(edgelist_g)

# 【代码含义】设置随机种子，使网络绘图的布局尽量保持稳定。
# 网络图的节点位置有时是随机生成的；设置 seed 后，每次画出来的位置更容易一致。
set.seed(3523) # Set random seed to ensure graph layout stays

# 【代码含义】绘制网络图。
# E(edgelist_g)$weight 表示取出每条边的 weight 属性。
# edge.width = E(edgelist_g)$weight 表示用边权重控制线条粗细，权重越大边越粗。
plot(edgelist_g, edge.width = E(edgelist_g)$weight)

# 【代码含义】打开 tkplot 函数的帮助文档。
# tkplot() 可以生成一个可交互的网络图窗口，用户可以手动拖动节点位置。
?tkplot

# 【代码含义】用 tkplot() 生成可交互网络图。
# edge.width = 1:10 表示把边宽度设置为 1 到 10 的序列。
# 注意：这里并不是直接使用真实权重，而是手动给不同边赋予不同线宽。
tkplot(edgelist_g, edge.width = 1:10)


# ------------------------------------------------------------
# B) As the format of an adjacency matrix
# ------------------------------------------------------------

# 【代码含义】加载 networkR 包。
# 这里主要使用 adjacency() 函数，把 from-to-weight 转换成邻接矩阵。
library(networkR)

# 【代码含义】再次构造 from、to、weight 三个向量。
# 这和前面边列表部分使用的是同一组示例网络。
from <- c("1","1","2","2","3","4","4","5")
to <- c("2","4","3","4","4","5","6","6")
weight <- c(0.1,0.5,0.8,0.2,0.4,0.9,1.0,0.5)

# 【代码含义】根据 from、to、weight 生成邻接矩阵。
# 邻接矩阵中，行和列都代表节点；矩阵中的值表示两个节点之间的连接权重。
adj_mat <- adjacency(from, to, weight)

# 【代码含义】给邻接矩阵的行名和列名都设置为 1 到 6。
# 这样矩阵的每一行、每一列都对应一个节点编号。
rownames(adj_mat) <- 1:6
colnames(adj_mat) <- 1:6

# 【代码含义】显示邻接矩阵，检查节点之间的连接关系和权重。
adj_mat

# 【代码含义】把邻接矩阵转换成 igraph 网络图对象。
# weighted = T 表示矩阵中的数值作为边权重。
# mode = "max" 常用于把非对称矩阵转为无向图时取两个方向中的较大值。
# diag = F 表示不使用对角线元素，即不保留节点连接自己的自环。
adj_mat_g <- graph_from_adjacency_matrix(adj_mat,
                                       weighted=T,
                                       mode="max", 
                                       diag=F)

# 【代码含义】用交互式方式查看邻接矩阵生成的网络图。
# 这里没有把权重映射到边宽，所以只是普通显示网络连接关系。
tkplot(adj_mat_g) # plot without edge weights

# 【代码含义】绘制带边权重的网络图。
# edge.width = E(adj_mat_g)$weight 表示边的线宽由矩阵中的权重决定。
plot(adj_mat_g, # plot with edge weights
     edge.width = E(adj_mat_g)$weight) 


# ------------------------------------------------------------
# C) As the format of incidence matrix
# ------------------------------------------------------------
# https://rpubs.com/lgadar/load-bipartite-graph

# 【代码含义】构造一个 3×3 的关联矩阵 / 二部图矩阵。
# 行代表 Bee1、Bee2、Bee3，列代表 PlantA、PlantB、PlantC。
# 1 表示存在互作关系，0 表示不存在互作关系。
inc_mat <- matrix(
  c(1,0,1,
    0,1,1,
    1,1,0),
  nrow = 3,
  byrow = TRUE
)

# 【代码含义】给关联矩阵添加行名和列名。
# 行名是蜜蜂节点，列名是植物节点。
rownames(inc_mat) <- c("Bee1","Bee2","Bee3")
colnames(inc_mat) <- c("PlantA","PlantB","PlantC")

# 【代码含义】显示关联矩阵，检查蜜蜂和植物之间的互作关系。
inc_mat

# 【代码含义】根据二部图矩阵构建 igraph 图对象。
# graph_from_biadjacency_matrix() 专门用于从二部图矩阵构建网络。
inc_mat_g <- graph_from_biadjacency_matrix(inc_mat)

# 【代码含义】显示二部图对象的基本信息，例如节点数、边数等。
inc_mat_g

# 【代码含义】查看二部图中每个节点的 type 属性。
# 在 igraph 的二部图中，type = FALSE 通常对应矩阵的行节点，type = TRUE 对应矩阵的列节点。
# 这里也就是 FALSE 对应 Bee，TRUE 对应 Plant。
V(inc_mat_g)$type # FALSE for rows and TURE for cols

# 【代码含义】计算适合二部图显示的布局。
# layout_as_bipartite() 会把两类节点分开放置，便于观察两类节点之间的连接。
layout <- layout_as_bipartite(inc_mat_g)

# 【代码含义】绘制二部网络图。
# vertex.color 根据节点类型设置颜色：植物节点 lightblue，蜜蜂节点 salmon。
# vertex.label.color 设置标签颜色；vertex.size 设置节点大小；edge.color 设置边颜色。
plot(inc_mat_g, layout = layout,
  vertex.color = ifelse(V(inc_mat_g)$type, "lightblue", "salmon"),
  vertex.label.color = "black",
  vertex.size = 20,
  edge.color = "grey50",
  main = "Bipartite Network"
)


#####################################################
# 02-molecular network construction and visualization
#####################################################
# https://github.com/YongxinLiu/Note/blob/master/R/igraph/co-occurrence_network.R

# 【本节总体含义】
# 这一部分以 16S rRNA 测序得到的 OTU 丰度数据为例，构建微生物分子生态共现网络。
# 基本思路是：
# 1. 读取 warming 和 control 两组 OTU 数据；
# 2. 删除缺失值太多的 OTU；
# 3. 把绝对丰度转换为相对丰度；
# 4. 计算 OTU 两两之间的 Spearman 相关系数；
# 5. 根据相关系数和显著性阈值筛选边；
# 6. 将相关矩阵转换为网络图，并删除孤立节点。


# ------------------------------------------------------------
# 1) load 16s RNA sequencing data (otu)
# ------------------------------------------------------------

# 【代码含义】读取 warming 处理组的 OTU 表。
# read.table() 用于读取文本表格；head = T 表示第一行是列名；row.names = 1 表示第一列作为行名。
# sep = "\t" 表示文件是制表符分隔的 txt 文件。
otu_warming <- read.table("data/netdata/warming.txt", 
                          head=T, row.names = 1, sep = "\t")

# 【代码含义】查看 warming 数据前几行，用于初步检查数据结构。
head(otu_warming)

# 【代码含义】查看 warming 数据维度。
# dim() 返回行数和列数，通常行是 OTU，列是样本或样地。
dim(otu_warming) 

# 【代码含义】统计 warming 数据中非缺失值的数量。
# !is.na() 判断哪些元素不是 NA，sum() 对 TRUE 计数。
sum(!is.na(otu_warming)) 

# 【代码含义】读取 control 对照组的 OTU 表，参数含义与 warming 组相同。
otu_control <- read.table("data/netdata/control.txt", 
                          head=T, row.names = 1, sep = "\t")

# 【代码含义】查看 control 数据前几行。
head(otu_control)

# 【代码含义】查看 control 数据行列数。
dim(otu_control)

# 【代码含义】统计 control 数据中非缺失值的数量。
sum(!is.na(otu_control))

# 【代码含义】这一段被注释掉了。
# 如果取消注释，就可以把 warming 和 control 两个对象保存成 RData 文件，之后可以直接 load() 读取。
# save(otu_warming, otu_control, 
#      file = "data/netdata/otu_data.RData")


# ------------------------------------------------------------
# 2) keep the rows in which the otus with < 7 NA in 14 plots
# ------------------------------------------------------------

# 【代码含义】这一部分是缺失值过滤。
# 原始注释意思是：保留在 14 个样地中缺失值少于 7 个的 OTU。
# 换句话说，如果某个 OTU 在太多样本中没有观测值，就认为它信息不足，后续分析中剔除。

# A) for warming plots

# 【代码含义】对 warming 数据逐行计算 NA 数量。
# apply(otu_warming, 1, ...) 中的 1 表示按行操作。
# function(z) sum(is.na(z)) 表示对每一行统计缺失值个数。
na_counts_warming <- apply(otu_warming, 1, function(z) sum(is.na(z)))

# 【代码含义】查看一共有多少个 OTU 被统计了缺失值数量。
length(na_counts_warming)

# 【代码含义】保留缺失值数量小于 7 的 OTU。
# 注意：原注释写的是删除 NA OUTs > 7，但代码实际保留的是 < 7。
# 如果某行 NA 数量等于 7，这里也会被删除。
otu_warming_clean <- otu_warming[na_counts_warming < 7,]

# 【代码含义】查看清理后的 warming 数据前几行和维度。
head(otu_warming_clean)
dim(otu_warming_clean)

# B) for control plots

# 【代码含义】对 control 数据逐行统计 NA 数量。
na_counts_control <- apply(otu_control, 1, function(z) sum(is.na(z)))

# 【代码含义】保留缺失值数量小于 7 的 control 组 OTU。
otu_control_clean <- otu_control[na_counts_control< 7,]

# 【代码含义】查看清理后的 control 数据维度。
dim(otu_control_clean)


# ------------------------------------------------------------
# 3) calculating the relative abundance of each OTU in each plot
# ------------------------------------------------------------

# 【代码含义】这一部分把 OTU 的原始丰度转换为相对丰度。
# 因为不同样本的测序深度或总丰度可能不同，所以常用相对丰度来消除样本总量差异。

# A) for warming plots

# 【代码含义】对 warming 清理后的数据做两步处理：
# 1. replace(is.na(...), 0)：把缺失值 NA 替换为 0；
# 2. t()：转置矩阵，使数据从 “OTU × 样地” 变成 “样地 × OTU”。
# 这样每一行就是一个样地，每一列就是一个 OTU，便于按样地计算相对丰度。
otu_warming_clean_transform <- otu_warming_clean |>
  replace(is.na(otu_warming_clean), 0) |> # replace NA with 0's value
  t() # transfer from outs x plot to the  plot x otus table

# 【代码含义】计算 warming 组每个样地中每个 OTU 的相对丰度。
# prop.table(..., margin = 1) 表示按行求比例，即每个样地内所有 OTU 的比例之和为 1。
# *100 表示把比例转换为百分比。
otu_warming_rel <- 
  prop.table(as.matrix(otu_warming_clean_transform), 
                              margin = 1)*100 # each OTU relative abundance in each plot

# 【代码含义】查看 warming 组相对丰度矩阵前几行。
head(otu_warming_rel)

# B) for control plots

# 【代码含义】对 control 清理后的数据进行缺失值替换和转置。
otu_control_transform <- otu_control_clean |>
  replace(is.na(otu_control_clean), 0) |>
  t()

# 【代码含义】计算 control 组每个样地中每个 OTU 的相对丰度百分比。
otu_control_rel <- prop.table(as.matrix(otu_control_transform), 
                              margin = 1)*100

# 【代码含义】查看 control 组相对丰度矩阵前几行。
head(otu_control_rel)


# ------------------------------------------------------------
# 4) calculating correlation coefficient
# ------------------------------------------------------------

# 【代码含义】这一部分计算 OTU 之间的相关性。
# 共现网络中，节点是 OTU，边表示两个 OTU 的丰度变化具有较强相关性。
# 这里使用 Spearman 相关，适合非正态、非线性单调关系的数据。

# A) for warming plots

# 【代码含义】加载 psych 包。
# corr.test() 可以同时计算相关系数矩阵和 p 值矩阵。
library(psych) # for correlation coefficient

# 【代码含义】计算 warming 组 OTU 两两之间的 Spearman 相关。
# use = "pairwise" 表示每一对变量使用各自可用的非缺失样本来计算。
# method = "spearman" 表示使用 Spearman 秩相关。
# adjust = "fdr" 表示对多重检验的 p 值进行 FDR 校正。
# alpha = 0.05 是显著性水平设置。
otu_warming_corr <- corr.test(otu_warming_rel, use = "pairwise",
                              method = "spearman", adjust = "fdr",
                              alpha = 0.05)

# 【代码含义】从 corr.test 结果中提取相关系数矩阵 r。
otu_warming_r <- otu_warming_corr$r # extracting r values

# 【代码含义】从 corr.test 结果中提取 p 值矩阵。
otu_warming_p <- otu_warming_corr$p # extracting p valutes

# 【代码含义】根据阈值筛选相关关系。
# 如果 p 值大于 0.5，或者相关系数绝对值小于 0.70，就把该相关系数设为 0。
# 设置为 0 的位置在后面构建网络时不会形成边。
# 注意：生态共现网络中常见阈值可能是 p < 0.05 且 |r| > 0.6/0.7。
# 这里使用 p > 0.5 作为过滤条件比较宽松，若用于正式分析，建议确认是否应为 p > 0.05。
otu_warming_r[otu_warming_p > 0.5 | abs(otu_warming_r) < 0.70] <-0

# 【代码含义】显示筛选后的 warming 相关矩阵。
# 非 0 值代表保留下来的潜在网络边。
otu_warming_r

# B) for control plots

# 【代码含义】计算 control 组 OTU 两两之间的 Spearman 相关和 FDR 校正 p 值。
otu_control_corr <- corr.test(otu_control_rel, use = "pairwise",
                              method = "spearman", adjust = "fdr",
                              alpha = 0.05)

# 【代码含义】提取 control 组相关系数矩阵和 p 值矩阵。
otu_control_r <- otu_control_corr$r # extracting r values
otu_control_p <- otu_control_corr$p # extracting p valutes

# 【代码含义】用同样的阈值筛选 control 组网络边。
# p 值过大或相关系数绝对值较小的连接都设为 0。
otu_control_r[otu_control_p >0.5 | abs(otu_control_r)<0.70] <-0

# 【代码含义】显示筛选后的 control 相关矩阵。
otu_control_r


# ------------------------------------------------------------
# 5) constructing molecular ecological network
# ------------------------------------------------------------

# 【代码含义】这一部分把筛选后的相关矩阵转成网络。
# 矩阵中的每个 OTU 是节点，非 0 相关系数对应节点之间的边，相关系数值作为边权重。

# A) for warming plots 

# 【代码含义】加载 igraph 包，用于构建和绘制网络。
library(igraph)

# 【代码含义】查看 otu_warming_r 的对象类型，一般应为 matrix。
class(otu_warming_r)

# 【代码含义】根据 warming 相关矩阵构建无向加权网络。
# mode = "undirected" 表示无向网络；weighted = TRUE 表示保留相关系数作为边权重。
# diag = FALSE 表示不保留自己和自己的连接。
otu_warming_g <- graph_from_adjacency_matrix(otu_warming_r,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

# 【代码含义】初步绘制 warming 共现网络。
plot(otu_warming_g)

# 【代码含义】找出度为 0 的孤立节点。
# degree(g) 表示每个节点连接的边数；等于 0 说明该 OTU 没有与其他 OTU 形成有效相关边。
otu_warming_isol_vertex <- 
  V(otu_warming_g)[igraph::degree(otu_warming_g) == 0] # isolated vertices

# 【代码含义】从 warming 网络中删除孤立节点。
# 这样得到的网络只保留至少有一条边的 OTU，更方便可视化和计算网络指标。
otu_warming_g_optimal <- 
  igraph::delete_vertices(otu_warming_g, 
                          otu_warming_isol_vertex)

# 【代码含义】设置随机种子，使后续网络布局更稳定。
set.seed(123)

# 【代码含义】绘制删除孤立节点后的 warming 共现网络。
# vertex.frame.color = NA 去掉节点边框；vertex.label = NA 不显示节点标签；
# edge.width = 1 设置边宽；vertex.size = 5 设置节点大小；
# edge.curved = TRUE 让边稍微弯曲，减少重叠。
plot(otu_warming_g_optimal, main ="co-occurrence network",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=5,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)

# 【代码含义】用可交互窗口查看 warming 网络。
tkplot(otu_warming_g_optimal)

# 【代码含义】这一段被注释掉了。
# 如果取消注释，可以把 warming 共现网络保存为 edgelist 文件，后续网络指标分析会读取这个文件。
# write_graph(otu_warming_g_optimal,
#             "data/netdata/otu_warming_net.txt", "edgelist")


# B) for control plots

# 【代码含义】根据 control 相关矩阵构建无向加权网络。
otu_control_g <- graph_from_adjacency_matrix(otu_control_r,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

# 【代码含义】初步绘制 control 网络。
plot(otu_control_g)

# 【代码含义】找出 control 网络中度为 0 的孤立节点。
otu_control_isol_vertex <- 
  V(otu_control_g)[igraph::degree(otu_control_g) == 0] # isolated vertices

# 【代码含义】删除 control 网络中的孤立节点。
otu_control_g_optimal <- igraph::delete_vertices(otu_control_g, 
                                                 otu_control_isol_vertex)

# 【代码含义】设置随机种子，绘制清理后的 control 共现网络。
set.seed(123)
plot(otu_control_g_optimal, main ="co-occurrence network",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=5,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)

# 【代码含义】这一段被注释掉了。
# 如果取消注释，可以把 control 共现网络保存为 edgelist 文件。
# write_graph(otu_control_g_optimal,
#             "data/netdata/otu_control_net.txt", "edgelist")


################################################
# 03-network properties and exploratory analysis
################################################

# 【本节总体含义】
# 这一部分读取已经保存好的 warming 共现网络，并计算网络结构指标。
# 网络指标分为两类：
# 1. 网络整体水平指标：例如 connectance、modularity；
# 2. 节点水平指标：例如 degree、betweenness、closeness、eigenvector centrality、三角形数量和聚类系数。


# ------------------------------------------------------------
# 1) Network level properties
# ------------------------------------------------------------

# A) connectance
# https://bookdown.org/creakysinger/r-note-learn/_book/Nchpter20.html

# 【代码含义】加载 igraph 包。
library(igraph)

# 【代码含义】从 edgelist 文件中读取 warming 网络。
# 注意：这个文件需要前面 write_graph() 生成，或者已经存在于 data/netdata/otu_warming_net.txt。
g <- read_graph("data/netdata/otu_warming_net.txt","edgelist")

# 【代码含义】把网络转为无向图。
# mode = "collapse" 表示如果有重复边或方向边，就合并成无向边。
g <- as_undirected(g, mode = "collapse")

# 【代码含义】用 tkplot() 交互式查看网络。
# 参数用于设置节点、边和标签的显示风格。
tkplot(g,
     vertex.frame.color=NA,
     vertex.label=NA,
     edge.width=1,
     vertex.size=5,
     edge.lty=1,
     edge.curved=F)

# 【代码含义】计算网络 connectance，也就是连接度或边密度。
# edge_density(g, loops = FALSE) = 实际边数 / 理论最大可能边数。
# loops = FALSE 表示不考虑节点连接自己的自环。
# connectance 越高，说明网络中节点之间连接越密集。
connectance = edge_density(g,loops=FALSE) # connectance
connectance

# B) modularity
# https://biosakshat.github.io/network-analysis.html

# 【代码含义】重新读取 warming 网络并转为无向图。
library(igraph)
g<-read_graph("data/netdata/otu_warming_net.txt","edgelist")
g <- as_undirected(g, mode = "collapse")

# 【代码含义】使用 edge betweenness 方法进行社群划分。
# cluster_edge_betweenness() 会寻找网络中的社区结构，也就是节点更容易在社区内部连接。
ceb <- cluster_edge_betweenness(g)

# 【代码含义】计算社群划分的模块度 modularity。
# modularity 越高，说明网络社区结构越明显。
modularity(ceb)

# 【代码含义】绘制网络，并根据社群划分结果给节点分组显示。
plot(ceb, g)


# ------------------------------------------------------------
# 2) Node level properties
# ------------------------------------------------------------

# A) degree and degree distribution

# 【代码含义】读取 warming 网络，转成无向图，并绘制基础网络图。
library(igraph)
g <-read_graph("data/netdata/otu_warming_net.txt","edgelist") 
g <- as_undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)

# 【代码含义】计算每个节点的度 degree。
# degree 表示一个节点连接了多少条边。
# 在微生物共现网络中，度高的 OTU 可能是与很多 OTU 共现的关键类群。
deg <- igraph::degree(g, mode="all") # calculate degree

# 【代码含义】显示每个节点的 degree。
deg

# 【代码含义】绘制度分布直方图。
# vcount(g) 是网络节点数；breaks 设置直方图分组。
# 度分布可以帮助判断网络中是否存在少数高度连接的枢纽节点。
hist(deg, breaks=1:vcount(g)-1) # degree distribution

# B) closeness and betweenness centrality

# 【代码含义】重新计算 degree。
deg=igraph::degree(g) 

# 【代码含义】用 Fruchterman-Reingold 算法固定网络布局。
# layout_with_fr() 是一种力导向布局：连接较紧密的节点会更靠近。
# 把布局保存到 lay 后，后续不同指标的图可以共用同一套节点位置，便于比较。
lay <- layout_with_fr(g) # fix layout
lay

# 【代码含义】设置颜色渐变精细程度。
# fine 越大，颜色分级越细。
fine = 500 # increase fine regulation

# 【代码含义】创建从蓝色到红色的渐变色函数。
# 通常可以用颜色表示节点 degree 的大小，蓝色低、红色高。
palette = colorRampPalette(c('blue','red')) # set color

# 【代码含义】根据 degree 把每个节点映射到一个颜色。
# cut(deg, breaks = fine) 把 degree 划分成 fine 个区间；
# palette(fine)[...] 根据区间编号给节点赋色。
degCol = palette(fine)[as.numeric(cut(deg,breaks = fine))]

# 【代码含义】绘制以 degree 为节点大小的网络图。
# vertex.size = deg * 1.5 表示 degree 越高，节点越大。
plot(g, layout=lay, vertex.color=degCol, 
     vertex.size=deg*1.5, vertex.label=NA)

# 【代码含义】计算 betweenness centrality，中介中心性。
# betweenness 越高，说明该节点位于更多最短路径上，可能起到桥梁或连接不同模块的作用。
betw <- igraph::betweenness(g) # betweenness

# 【代码含义】绘制以 betweenness 为节点大小的网络图。
# 节点越大，说明中介中心性越高。
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=betw*0.8, vertex.label=NA)

# 【代码含义】计算 closeness centrality，接近中心性。
# closeness 衡量一个节点到其他节点的平均距离远近。
# closeness 越高，说明该节点整体上离其他节点更近，信息或影响传播路径更短。
clos <- igraph::closeness(g) # closeness

# 【代码含义】绘制以 closeness 为节点大小的网络图。
# 这里乘以 15000 是为了把数值放大到适合显示的节点大小。
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=clos*15000,vertex.label=NA)

# 【代码含义】计算 eigenvector centrality，特征向量中心性。
# 它不仅考虑一个节点连接了多少节点，还考虑它连接的节点本身是否重要。
# 第一行得到完整结果对象，第二行只提取 vector 部分。
ev <- igraph::eigen_centrality(g)
ev <- igraph::eigen_centrality(g)$vector

# 【代码含义】显示每个节点的特征向量中心性数值。
ev

# 【代码含义】绘制以 eigenvector centrality 为节点大小的网络图。
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=ev*10, vertex.label=NA)

# C) triads and clustering coefficients

# 【代码含义】统计每个节点参与的三角形数量。
# 三角形表示三个节点两两相连，是网络局部闭合结构的重要指标。
triad <- igraph::count_triangles(g)
triad

# 【代码含义】计算全局聚类系数。
# transitivity(type = "global") 衡量整个网络形成闭合三角结构的倾向。
cc_global <- igraph::transitivity(g, type = "global")
cc_global

# 【代码含义】计算局部聚类系数。
# transitivity(type = "local") 返回每个节点的局部聚类系数。
# 局部聚类系数越高，说明该节点的邻居之间也更容易相互连接。
cc_local <- igraph::transitivity(g, type = "local")
cc_local


#####################################################
# 04- link prediction of otu_warming net with keras
#####################################################

# 【本节总体含义】
# 这一部分做网络链路预测 link prediction。
# 链路预测的目标是：给定两个节点，预测它们之间是否可能存在连接。
# 代码分为两条路线：
# 1. 传统机器学习：先构造边特征，再用随机森林分类；
# 2. 深度学习：用 Keras 构建神经网络做二分类。
#
# 正样本：网络中真实存在的边，label = 1。
# 负样本：理论上可能存在、但当前网络中不存在的边，label = 0。


# ------------------------------------------------------------
# 1) traditional machine learning for link pred
# ------------------------------------------------------------

# A) the positive/negative edges

# 【代码含义】加载 warming 网络，并转成无向图。
# 这里继续使用 otu_warming_net.txt 作为链路预测的数据来源。
library(igraph)
g <- read_graph("data/netdata/otu_warming_net.txt", 
                format = "edgelist")
g <- as_undirected(g, mode = "collapse")

# 【代码含义】用 tkplot() 可视化网络。
tkplot(g)  # Visualize the graph

# 【代码含义】提取网络中的所有节点 ID。
node_ids <- V(g)  # Get the node IDs
print(node_ids)

# 【代码含义】生成网络中所有理论上可能的无向边。
# combn(V(g), 2) 表示从所有节点中两两组合。
# t() 是转置，使每一行是一对节点。
possible_edges_mat <- t(combn(V(g), 2))

# 【代码含义】把每一对可能边转换为统一格式的字符串，例如 "A-B"。
# sort(x) 是为了保证 A-B 和 B-A 被视为同一条无向边。
# collapse = "-" 表示用短横线连接两个节点名。
possible_edges_links <- apply(possible_edges_mat, 1, function(x) paste(sort(x), collapse = "-"))

# 【代码含义】统计所有理论可能边的数量。
length(possible_edges_links)

# 【代码含义】提取网络中实际存在的边。
positive_edgelist_mat <- as_edgelist(g)  # Getting the edge list

# 【代码含义】把实际存在的边也转换成统一字符串格式，例如 "A-B"。
positive_edges_links <- apply(positive_edgelist_mat, 1, function(x) paste(sort(x), collapse = "-"))

# 【代码含义】统计真实存在的边数量，即正样本数量。
length(positive_edges_links)

# 【代码含义】加载 dplyr，用于管道、重命名列、添加变量等数据整理操作。
library(dplyr)

# 【代码含义】把真实存在的边整理成带标签的数据框。
# strsplit("-")：把 "A-B" 拆成 A 和 B；
# do.call(rbind, .)：把拆分结果合并成矩阵；
# as.data.frame()：转成数据框；
# rename(from = V1, to = V2)：把两列改名为 from 和 to；
# mutate(label = 1)：给真实边加上标签 1。
positive_edges_labels <- positive_edges_links %>%
  strsplit("-") %>%  # Splitting char
  do.call(rbind, .) %>%  # Merging into a matrix
  as.data.frame(stringsAsFactors = FALSE) %>%  # Converting to dataframe
  rename(from = V1, to = V2) %>%  # Renaming columns
  mutate(label = 1)  # Adding labels for positive edges

# 【代码含义】再次得到真实存在的边字符串，用于和所有可能边做差集。
existing_edges_links <- apply(positive_edgelist_mat, 1, function(x) paste(sort(x), collapse = "-"))

# 【代码含义】找出负样本边。
# setdiff(possible_edges_links, existing_edges_links) 表示：所有理论可能边中，去掉真实存在的边。
# 剩下的就是当前网络中不存在的边，作为 label = 0 的样本。
negative_edges_links <- setdiff(possible_edges_links, existing_edges_links)

# 【代码含义】统计负样本数量。
length(negative_edges_links)

# 【代码含义】把不存在的边整理成带标签的数据框。
# 处理方式与正样本相同，只是 label 设置为 0。
negative_edges_labels <- negative_edges_links %>%
  strsplit("-") %>%  # Splitting char
  do.call(rbind, .) %>%  # Merging into a matrix
  as.data.frame(stringsAsFactors = FALSE) %>%  # Converting to dataframe
  rename(from = V1, to = V2) %>%  # Renaming columns
  mutate(label = 0)  # Adding labels for negative edges

# 【代码含义】合并正样本和负样本，并随机打乱顺序。
# bind_rows() 纵向合并两个数据框；sample_frac(1) 表示按 100% 比例随机抽样，也就是打乱所有行。
all_edges_labels <- bind_rows(positive_edges_labels, negative_edges_labels) %>%
  sample_frac(1)  # Shuffling edges

# 【代码含义】这一行被注释掉了。
# 如果取消注释，就可以把所有边及其标签保存成 RDS 文件，后面直接 readRDS() 读取。
# saveRDS(all_edges_labels, "data/netdata/all_edge_label.rds")


# ------------------------------------------------------------
# B) constructing features
# ------------------------------------------------------------

# 【代码含义】定义一个函数，用来为任意两个节点 i 和 j 构造链路预测特征。
# 输入：节点 i、节点 j、网络 g。
# 输出：四个经典链路预测指标：CN、Jaccard、PA、AA。
get_edge_features <- function(i, j, g) {
  
  # 【代码含义】获取节点 i 和节点 j 的邻居节点集合。
  ni <- neighbors(g, i)
  nj <- neighbors(g, j)
  
  # 【代码含义】Common Neighbors，共同邻居数。
  # 如果两个节点有很多共同邻居，它们未来形成连接的可能性通常更高。
  cn <- length(intersect(ni, nj))  # Common Neighbors
  
  # 【代码含义】Jaccard Index，Jaccard 相似系数。
  # 它等于共同邻居数 / 邻居并集大小。
  # 如果并集大小为 0，就把 Jaccard 设为 0，避免除以 0。
  union_size <- length(union(ni, nj))  # Jaccard Index
  jaccard <- ifelse(union_size == 0, 0, cn / union_size)
  
  # 【代码含义】Preferential Attachment，优先连接指标。
  # 它等于两个节点度的乘积。
  # 度越大的节点越可能获得新连接，这是复杂网络中的常见假设。
  pa <- length(ni) * length(nj)  # Preferential Attachment
  
  # 【代码含义】Adamic-Adar Index。
  # 它也是基于共同邻居，但会降低高度连接共同邻居的贡献。
  # 直觉：如果两个节点共享一个比较“稀有”的共同邻居，这个共同邻居更有信息量。
  common_nodes <- intersect(ni, nj)  # Adamic-Adar Index
  aa <- ifelse(length(common_nodes) == 0, 0,
               sum(1 / log(degree(g, common_nodes) + 1)))
  
  # 【代码含义】返回四个特征，顺序为 CN、Jaccard、PA、AA。
  return(c(cn, jaccard, pa, aa))
}

# 【代码含义】对 all_edges_labels 中的每一条边计算链路预测特征。
# lapply() 逐行处理；get_edge_features() 计算每条边的四个特征；
# do.call(rbind, ...) 把每条边的特征合并为一个特征矩阵。
edge_features <- do.call(rbind, lapply(1:nrow(all_edges_labels), function(i) {
  e <- all_edges_labels[i, 1:2]
  get_edge_features(e[1], e[2], g)
}))

# 【代码含义】给特征矩阵命名。
# CN = Common Neighbors；Jaccard = Jaccard 相似系数；
# PA = Preferential Attachment；AA = Adamic-Adar。
colnames(edge_features) <- c("CN", "Jaccard", "PA", "AA")

# 【代码含义】下面两行被注释掉了。
# 第一行可以对特征进行标准化；第二行可以把特征矩阵保存成 RDS 文件。
# 注意：随机森林通常不强制要求标准化，但神经网络通常更适合使用标准化特征。
# edge_features_scaled <- scale(edge_features)
# saveRDS(edge_features, "data/netdata/edge_features.rds")


# ------------------------------------------------------------
# C) splitting training and validation sets
# ------------------------------------------------------------

# 【代码含义】从 RDS 文件读取已经保存好的边特征和边标签。
# 注意：这两个文件需要前面 saveRDS() 生成过，或者已经存在。
edge_features <- readRDS("data/netdata/edge_features.rds")
all_edges_labels <- readRDS("data/netdata/all_edge_label.rds")

# 【代码含义】设置随机种子，使训练集和验证集划分可以复现。
set.seed(123)

# 【代码含义】随机抽取 80% 的样本作为训练集索引。
# nrow(edge_features) 是总样本数；size = 0.8 * 总样本数表示抽取 80%。
idx <- sample(1:nrow(edge_features), size = 0.8 * nrow(edge_features))

# 【代码含义】根据索引划分训练集特征和训练集标签。
x_train <- edge_features[idx, ]
y_train <- all_edges_labels$label[idx]

# 【代码含义】剩余 20% 作为验证集特征和验证集标签。
x_val <- edge_features[-idx, ]
y_val <- all_edges_labels$label[-idx]


# ------------------------------------------------------------
# D) training and evaluating rf model
# ------------------------------------------------------------

# 【代码含义】加载 randomForest 包，用于训练随机森林分类模型。
library(randomForest)

# 【代码含义】训练随机森林模型。
# x = x_train 是训练特征；y = as.factor(y_train) 把 0/1 标签转为分类变量；
# ntree = 200 表示构建 200 棵决策树。
model <- randomForest(
  x = x_train,
  y = as.factor(y_train),
  ntree = 200  # Number of trees
)

# 【代码含义】用训练好的随机森林预测验证集样本为“存在边”的概率。
# type = "prob" 返回每个类别的概率；[, 2] 取 label = 1 的概率。
pred_prob <- predict(model, x_val, type = "prob")[, 2]

# 【代码含义】加载 pROC 包，用于画 ROC 曲线并计算 AUC。
library(pROC)

# 【代码含义】根据真实标签 y_val 和预测概率 pred_prob 构建 ROC 对象。
roc_obj <- roc(y_val, pred_prob)

# 【代码含义】绘制 ROC 曲线。
plot(roc_obj)

# 【代码含义】计算 AUC 值。
# AUC 越接近 1，模型区分正负边的能力越强；接近 0.5 则接近随机猜测。
auc(roc_obj)


# ------------------------------------------------------------
# 2) deep learning for link pre with keras in R
# ------------------------------------------------------------

# A) configuring a python env for running keras

# 【代码含义】这一段是 Keras/TensorFlow 的 Python 环境配置示例，目前被整体注释掉。
# R 中的 keras3 依赖 Python 后端，因此如果环境没有配置好，可能会报 reticulate、tensorflow 或 keras 相关错误。
# 这些代码的作用包括：查看虚拟环境、删除旧环境、创建新环境、安装 tensorflow 和 keras、测试 TensorFlow 是否可用。

# library(reticulate)
# virtualenv_list()
# virtualenv_remove("r-reticulate")
# virtualenv_create("r-reticulate", python = "/usr/bin/python3.10")
# # use_virtualenv("r-reticulate", required = TRUE)
# 
# # installing tensorflow and keras
# 
# reticulate::py_install("tensorflow")
# reticulate::py_install("keras")
# 
# library(tensorflow)
# tf$constant("ok")

# B) training and evaluation DL model

# a. defining the model

# 【代码含义】加载 keras3 包，用于在 R 中构建深度学习模型。
library(keras3)

# 【代码含义】定义一个顺序神经网络模型。
# 模型结构为：
# 输入层：输入维度等于 edge_features 的列数，也就是链路预测特征数；
# 第 1 个隐藏层：32 个神经元，ReLU 激活；
# 第 2 个隐藏层：16 个神经元，ReLU 激活；
# Dropout 层：随机丢弃 50% 神经元，用于降低过拟合；
# 输出层：1 个神经元，sigmoid 激活，用于输出“存在边”的概率。
model <- keras_model_sequential() |>
  layer_dense(32, activation = "relu", input_shape = c(ncol(edge_features))) |>
  layer_dense(16, activation = "relu") |>
  layer_dropout(0.5) |>
  layer_dense(1, activation = "sigmoid")

# 【代码含义】编译神经网络模型。
# loss = "binary_crossentropy" 用于二分类问题；
# optimizer = "adam" 是常用的自适应优化器；
# metrics = accuracy 和 AUC，分别评估分类准确率和排序区分能力。
model |> compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = list("accuracy", "AUC")
)

# b. training DL model

# 【代码含义】训练神经网络模型。
# x_train、y_train 是训练数据；validation_data 指定验证集；
# epochs = 50 表示完整训练 50 轮；batch_size = 32 表示每次用 32 个样本更新一次参数。
history <- model |> fit(
  x_train,
  y_train,
  validation_data = list(x_val, y_val),
  epochs = 50,
  batch_size = 32
  )

# 【代码含义】绘制训练过程曲线。
# 通常包括 loss、accuracy、AUC 以及验证集上的变化，用于判断是否过拟合或欠拟合。
plot(history)

# c. evaluating DL model

# 【代码含义】用训练好的神经网络预测验证集样本为正样本的概率。
# predict(x_val) 输出矩阵或数组；as.vector() 转成普通向量。
pred_prob <- model |> 
  predict(x_val) |> 
  as.vector()

# 【代码含义】加载 pROC 包，计算深度学习模型的 ROC 和 AUC。
library(pROC)

# 【代码含义】构建 ROC 对象。
roc_obj <- roc(y_val, pred_prob)

# 【代码含义】绘制深度学习模型的 ROC 曲线。
plot(roc_obj, main = "ROC Curve")

# 【代码含义】计算深度学习模型的 AUC。
auc(roc_obj)

# 【代码含义】加载 caret 包，用于计算混淆矩阵等分类评价指标。
library(caret)

# 【代码含义】把预测概率转成 0/1 类别。
# 如果概率大于 0.5，就预测为存在边 1；否则预测为不存在边 0。
pred_label <- ifelse(pred_prob > 0.5, 1, 0)

# 【代码含义】计算混淆矩阵。
# 第一个 factor 是模型预测标签，第二个 factor 是真实标签。
# levels = c(0,1) 指定类别顺序，避免 R 自动排序或缺失某一类导致错误。
conf_matrix <- confusionMatrix(
  factor(pred_label, levels = c(0,1)),
  factor(y_val, levels = c(0,1))
)

# 【代码含义】显示混淆矩阵结果。
# 其中包括 Accuracy、Sensitivity、Specificity、Kappa 等评价指标。
conf_matrix


# ============================================================
# 整体逻辑总结
# ============================================================
# 1. 先介绍网络数据的三种表达方式：边列表、邻接矩阵、二部图关联矩阵。
# 2. 然后用 OTU 表构建微生物共现网络：
#    原始 OTU 丰度 -> 缺失值过滤 -> 相对丰度 -> Spearman 相关 -> 阈值筛选 -> 网络图。
# 3. 接着对网络进行探索性分析：
#    整体指标包括 connectance 和 modularity；
#    节点指标包括 degree、betweenness、closeness、eigenvector centrality 和聚类系数。
# 4. 最后做链路预测：
#    把真实存在的边作为正样本，把不存在的可能边作为负样本；
#    构造 CN、Jaccard、PA、AA 四个拓扑特征；
#    分别用随机森林和 Keras 神经网络预测两节点之间是否可能存在边。
#
# 需要注意的地方：
# 1. 本脚本部分对象依赖外部文件，例如 edgelist.csv、warming.txt、control.txt、otu_warming_net.txt、edge_features.rds。
#    如果文件路径不存在，代码会报错。
# 2. 相关网络筛选中使用了 p > 0.5 的阈值。正式分析中通常更常见的是 p > 0.05 即删除不显著相关，
#    因此建议结合课程要求确认这里是否是教学示例还是笔误。
# 3. 正负样本数量可能严重不平衡。链路预测时除了 AUC，也建议关注混淆矩阵、Precision、Recall、F1 等指标。
# 4. 神经网络部分建议对 edge_features 做标准化，尤其是不同特征量纲差异较大时。
# 5. Keras3 依赖 Python/TensorFlow 环境，运行失败时通常需要先检查 reticulate 的 Python 环境配置。
# ============================================================
