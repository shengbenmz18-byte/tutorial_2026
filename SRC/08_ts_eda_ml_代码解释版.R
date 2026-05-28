# ============================================================
# 08_ts_eda_ml(2)_代码解释版.R
# 说明：
#   1. 本文件是在原始脚本 08_ts_eda_ml(2).R 的基础上整理得到的“解释版 R 文件”。
#   2. 原始代码基本保留，并在每一段代码前/关键行旁边加入中文注释。
#   3. 该脚本主题是：时间序列数据的创建、可视化、预处理、异常检测、自相关分析，
#      以及使用机器学习模型进行时间序列预测。
#   4. 运行前请确认：
#        - 工作目录下存在 data/tsdata/DOUBS_fishBiomassData.txt
#        - 已安装 forecast、timetk、tidyverse、tsibble、TSrepr、patchwork、
#          DataExplorer、ggthemes、randomForest、tidymodels、modeltime、
#          recipes、xgboost、ranger、fpp2 等包。
# ============================================================


# --------------------------------------------
# Script Name: timeseries and forecasting
# Purpose: The script will illustrate how to create
#          a ts object and to build a ml model for 
#          forecasting. 
# 
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-23
# --------------------------------------------

# 作用：清空 RStudio 控制台，相当于让控制台界面重新变干净。
cat("\014") # Clears the console

# 作用：删除当前 R 环境中已经存在的所有变量，避免之前运行残留变量影响本次分析。
rm(list = ls()) # Remove all variables


##############################################
# 01-creating a time series and representing it
# 第 1 部分：创建时间序列对象，并对时间序列进行表示和可视化
#############################################

# A) use ts() to create a time series
# A 小节：使用 R 基础函数 ts() 创建时间序列对象。


# 读取鱼类生物量数据。
# read.table() 用于读取 txt 表格数据。
# 'data/tsdata/DOUBS_fishBiomassData.txt' 是数据文件路径。
# h = TRUE 表示第一行是列名，相当于 header = TRUE。
data=read.table('data/tsdata/DOUBS_fishBiomassData.txt',h=TRUE)

# 直接输出 data，查看读入的数据内容。
data

# 数据清理：
# 1. dplyr::select(-YEAR)：删除 YEAR 这一列。
#    这里可能是因为数据中已经有 DATE 或其他时间字段，YEAR 列暂时不需要。
# 2. distinct()：去除完全重复的行，避免重复记录影响时间序列分析。
# 3. |> 是 R 的原生管道符，把左边结果传给右边函数。
data_clean <- data |>
  dplyr::select(-YEAR) |>
  distinct() # Identify and Remove Duplicate Data

# 查看 STATION 列中有哪些唯一站点名。
# 作用：检查数据里包含哪些采样站点。
unique(data_clean$STATION) # check stations

# 统计每个站点出现了多少条记录。
# 作用：了解不同站点样本数量是否均衡。
table(data_clean$STATION)

# 查看 SP 列中有哪些唯一鱼类物种。
# 作用：检查数据中包含哪些物种。
unique(data_clean$SP) # check species

# 统计每个物种出现了多少条记录。
# 作用：了解不同鱼类物种的记录数量。
table(data_clean$SP)

# 从清洗后的数据中筛选出：
#   STATION == "VOLPla"：采样站点为 VOLPla；
#   SP == "CHE"：鱼类物种为 CHE。
# 最终得到某一个站点、某一个物种的时间序列数据。
CHE_VOL_data <- data_clean |>
  subset(STATION=="VOLPla" & SP == "CHE")


# 创建 R 基础时间序列对象 ts。
# CHE_VOL_data[, -c(1:5)] 表示删除前 5 列，只保留后面的数值型变量，
# 通常这里保留的是 BIOMASS、DENSITY 等随年份变化的指标。
# start = 1994 表示时间序列从 1994 年开始。
# end = 2020 表示时间序列到 2020 年结束。
# frequency = 1 表示一年一个观测值，即年度数据。
CHE_ts = ts(data = CHE_VOL_data[, -c(1:5)], # creating ts
             start = 1994, # Start Year 1994
             end = 2020,
             frequency = 1)  # freq = 1


# Plot data with faceting
# 加载 forecast 包。
# forecast 包中的 autoplot() 可以把 ts 时间序列对象转换成 ggplot 风格图。
library(forecast) # work with ggplot2 for autoplot()

# 加载 ggplot2，用于绘图。
library(ggplot2)

# 对 CHE_ts 时间序列作图。
# facets = TRUE 表示如果 CHE_ts 中有多个变量，就分面分别画出来。
# ggtitle() 添加标题。
# ylab() 设置 y 轴标签。
# xlab() 设置 x 轴标签。
autoplot(CHE_ts, facets = TRUE) +
  ggtitle("CHE of Doubs river") +
  ylab("Changes") + xlab("Year")


# B) use timetk() to create a time series
# B 小节：使用 timetk 生态把普通数据框转换为适合 tidyverse 操作的时间序列数据。


# timetk：时间序列数据整理、特征构造、可视化和机器学习建模常用包。
library(timetk)

# tidyverse：包含 dplyr、tidyr、ggplot2、readr 等常用数据处理和绘图包。
library(tidyverse)

# tsibble：用于 tidy time series，也就是整洁时间序列数据结构。
library(tsibble)


# 将 CHE_VOL_data 转换成 tibble 格式，并整理时间列和指标列。
# tk_tbl()：把时间序列或普通对象转换为 tibble。
# select(index, DATE, BIOMASS, DENSITY)：保留索引、日期、生物量、密度四列。
# group_by(DATE)：按日期分组。
# mutate(DATE = ymd(paste0(year(DATE), "-01-01")))：
#   提取 DATE 中的年份，并把每个年份统一改成这一年的 1 月 1 日。
#   这样做的目的是把年度数据变成标准 Date 格式，方便后续时间序列处理。
# arrange(DATE)：按照日期从早到晚排序。
CHE_tk <- CHE_VOL_data |> 
  tk_tbl() |> # Convert to tibble
  select(index, DATE, BIOMASS, DENSITY) |>
  group_by(DATE) |>
  mutate(
    DATE = ymd(paste0(year(DATE), "-01-01"))
  ) |>
  arrange(DATE)


# 把宽格式数据转成长格式数据。
# 原来 BIOMASS 和 DENSITY 是两个不同的列。
# pivot_longer() 会把这两列合并成：
#   variable：变量名，比如 BIOMASS 或 DENSITY；
#   value：对应数值。
# 长格式更适合分组绘图、分面绘图和统一建模。
CHE_tk_long <- CHE_tk |>
  pivot_longer( # convert to to long format
  cols = c("BIOMASS", "DENSITY"),
  names_to = "variable",
  values_to = "value")

# 查看 CHE_tk 的维度，即行数和列数。
dim(CHE_tk)


# plot with timetk
# 使用 timetk 的 plot_time_series() 绘制时间序列。
# group_by(variable)：按变量分组，也就是 BIOMASS 和 DENSITY 分别绘制。
# .date_var = DATE：指定时间变量。
# .value = value：指定数值变量。
# .facet_ncol = 1：分面图每行 1 个图。
# .facet_scale = "free"：不同变量可以使用不同 y 轴尺度。
# .interactive = FALSE：输出静态 ggplot 图，而不是交互式 plotly 图。
# .title：设置图标题。
CHE_tk_long |>
  group_by(variable) |> 
  plot_time_series(
    .date_var    = DATE,
    .value       = value,
    .facet_ncol  = 1,
    .facet_scale = "free",  
    .interactive = FALSE,
    .title       = "CHE of Le Doubs River"
  )


# C) representation/reduce dimensions
# C 小节：时间序列表示与降维。
# 这里演示使用 PAA 方法将时间序列压缩表达。


# TSrepr：用于时间序列表示和降维的包。
library(TSrepr)


# 从 CHE_ts 中提取 BIOMASS 这一列，作为单变量时间序列。
# drop = TRUE 表示把结果简化成向量/单变量 ts，而不是保持矩阵结构。
CHE_biom_ts <- CHE_ts[, "BIOMASS", drop = TRUE]


# 绘制原始 BIOMASS 时间序列图。
p1 <- autoplot(CHE_biom_ts) +
  ggtitle("CHE biomass of Doubs river") +
  ylab("Changes") + xlab("Year")


# 使用 PAA（Piecewise Aggregate Approximation，分段聚合近似）表示时间序列。
# as.numeric(CHE_biom_ts)：把 ts 对象转换为普通数值向量。
# q = 2：表示压缩后的序列长度或分段数量为 2。
# func = meanC：每个分段用均值来代表。
# 作用：把原始较长时间序列压缩成较少的代表值，用于降维或简化趋势。
CHE_biom_paa <- repr_paa(as.numeric(CHE_biom_ts), q = 2, 
                         func = meanC) 

# 把 PAA 压缩后的结果重新转换成 ts 时间序列对象。
# start = c(1994, 1)：起始时间设置为 1994 年。
# frequency = 1：年度数据。
CHE_biom_paa_ts <- ts(CHE_biom_paa,
                  start= c(1994,1),
                  frequency =1)


# 绘制 PAA 降维后的时间序列图。
# 由于 q = 2，图中只有两个聚合后的点/阶段。
p2 <- autoplot(CHE_biom_paa_ts) +
  ggtitle("CHE biomass of Doubs river") +
  ylab("Changes") + xlab("Year")


# patchwork 可以把多个 ggplot 图组合在一起。
library(patchwork)

# p1 / p2 表示上下排列：
# 上面是原始时间序列，下面是 PAA 压缩后的时间序列。
p1 / p2


##################################################
# 02-data pre-processing and exploratory analysis
# 第 2 部分：时间序列数据预处理与探索性分析
##################################################

# A) missing imputation of a times eries object
# A 小节：缺失值补齐与插补。
# 目的：时间序列经常要求时间点连续。如果某些年份缺失，需要补齐并插补。
# https://www.kaggle.com/code/janiobachmann/time-series-i-an-introductory-start?scriptVersionId=53165252


# DataExplorer：常用于探索性数据分析。
library(DataExplorer)

# ggthemes：提供更多 ggplot 主题。
library(ggthemes)


# 构造可能含有缺失年份的年度时间序列数据。
# group_by(variable)：对 BIOMASS 和 DENSITY 分别处理。
# summarise_by_time(DATE, .by = "year", value = first(value))：
#   按年份汇总，每年取第一个 value。
# pad_by_time(DATE, .by = "year")：
#   按年度补齐时间序列，如果某些年份不存在，会补出该年份并产生 NA。
CHE_tk_missing <- 
  CHE_tk_long |>
  group_by(variable) |>
  summarise_by_time(
    DATE, 
    .by = "year",
    value = first(value)) |>
  pad_by_time(DATE, .by = "year")


# 绘制补齐年份后的时间序列。
# 如果某些年份缺失，图上可能会出现断点或 NA。
CHE_tk_missing |>
  plot_time_series(DATE, value, 
                   .facet_ncol = 1, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "CHE of Le Doubs river"
  ) 


# 查看 CHE_tk_missing 的最后几行。
# 作用：检查补齐时间后的数据末尾是否有 NA 或新增年份。
tail(CHE_tk_missing)


# imputation of missing data
# 对缺失值进行插补。
# https://business-science.github.io/timetk/articles/TK07_Time_Series_Data_Wrangling.html


# 对每个变量分别进行缺失值插补。
# group_by(variable)：BIOMASS 和 DENSITY 分开处理。
# pad_by_time(DATE, .by = "year")：再次确保每一年都有记录。
# mutate_at(vars(value), .funs = ts_impute_vec, period = 1)：
#   对 value 列使用 ts_impute_vec() 插补缺失值。
#   period = 1 表示年度数据，没有季节周期。
CHE_tk_imputed <- CHE_tk_missing |>
  group_by(variable) |>
  pad_by_time(DATE, .by = "year") |>
  mutate_at(vars(value), .funs = ts_impute_vec, period = 1) 


# 查看插补后的最后几行，检查 NA 是否被填充。
tail(CHE_tk_imputed)


# 绘制插补后的时间序列。
# 与前面的图相比，这里应该没有缺失导致的断点。
CHE_tk_imputed |>
  plot_time_series(DATE, value, 
                   .facet_ncol = 1, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "CHE of Le Doubs river"
  ) 


# B) Find the outlier in timeseries
# B 小节：时间序列异常值检测。
# 目的：寻找某些年份中明显偏离正常趋势的值。
# https://business-science.github.io/timetk/articles/TK07_Time_Series_Data_Wrangling.html


# plot_anomaly_diagnostics() 用于异常检测可视化。
# group_by(variable)：BIOMASS 和 DENSITY 分别检测异常。
# .date = DATE：指定日期列。
# .value = value：指定要检测异常的数值列。
# .facet_ncol = 1：每行一个分面。
# .interactive = FALSE：静态图。
# .anom_color = "#FB3029"：异常点颜色。
# .max_anomalies = 0.07：最多允许大约 7% 的数据被判为异常。
# .alpha = 0.05：异常检测显著性水平。
CHE_tk_imputed |>
  group_by(variable) |>
  plot_anomaly_diagnostics(
    .date = DATE,
    .value = value,
    .facet_ncol = 1,
    .interactive=FALSE,
    .title = "Anomaly Diagnostics",
    .anom_color ="#FB3029", 
    .max_anomalies = 0.07, 
    .alpha = 0.05
  )


# C) serial autocorrelation or ACF
# C 小节：自相关分析。
# 目的：判断当前年份的数值是否与前几年存在相关性。
# ACF：自相关函数，表示 y_t 与 y_{t-k} 的相关性。
# PACF：偏自相关函数，去掉中间滞后影响后的直接相关性。


# plot_acf_diagnostics() 会绘制 ACF 和 PACF 诊断图。
# .lags = "5 years" 表示最多查看滞后 5 年的相关性。
# 如果某个滞后阶数相关性明显，说明过去该年数值可能对当前预测有帮助。
CHE_tk_imputed |>
  group_by(variable) |>
  plot_acf_diagnostics(
    DATE, value,               # ACF & PACF
    .lags = "5 years",    
    .interactive = FALSE
  )


########################################################
# 03-the principle of a ts recursive forecast with embed
# 第 3 部分：使用 embed() 构造滞后变量，并进行递归式时间序列预测
########################################################

# 1) Focusing on the time series of fishBioms
# 1 小节：只关注鱼类生物量 BIOMASS 这一条单变量时间序列。


# 加载 tidyverse，用于数据处理和绘图。
library(tidyverse)

# randomForest：随机森林模型包。
library(randomForest)


# 从插补后的数据中筛选出 BIOMASS 变量。
CHE_tk_biom <- CHE_tk_imputed |>
  filter(variable == "BIOMASS")


# 绘制 BIOMASS 时间序列。
# .smooth = FALSE 表示不添加平滑曲线，只画原始序列。
CHE_tk_biom |>
  plot_time_series(
    DATE, value,
    .smooth = FALSE,
    .title = "BIOMASS Time Series"
  )

# 把 BIOMASS 的 value 列转换成基础 ts 对象。
# start = 1994：从 1994 年开始。
# frequency = 1：年度数据。
CHE_tk_biom_ts <- ts(CHE_tk_biom$value, start=1994, 
                     frequency=1)


# 2) splitting training/test and pre-processing
# 2 小节：划分训练集/测试集，并对训练集进行变换。


# 训练集使用 1994 到 2018 年的数据。
ts_train <- window(CHE_tk_biom_ts, end = 2018)

# 对训练集先取 log，再做一阶差分。
# log()：常用于稳定方差，减少极端值影响。
# diff(1)：一阶差分，用于减少趋势，使序列更接近平稳。
# 注意：做了 log + diff 后，后续预测结果需要反变换。
ts_train_trans <- ts_train |> log() |> diff(1)


# lag_order = 2 表示使用过去 2 个时间点作为预测特征。
lag_order <- 2 

# horizon = 2 表示向未来预测 2 年。
horizon <- 2                                              

# embed(ts_train_trans, lag_order + 1) 构造监督学习格式数据。
# 例如 lag_order = 2 时，每一行类似：
#   第 1 列：当前值 y_t，作为因变量 Y；
#   第 2 列：上一期 y_{t-1}；
#   第 3 列：上两期 y_{t-2}。
# 这样就把时间序列预测问题转成机器学习回归问题。
ts_train_mbd <- embed(ts_train_trans, lag_order + 1)

# Y_train 是模型要预测的目标变量，即当前期差分后的值。
Y_train <- ts_train_mbd[, 1] 

# X_train 是模型输入特征，即前 1 年、前 2 年的滞后值。
X_train <- ts_train_mbd[, -1] 


# 测试集为 2019 到 2020 年的真实 BIOMASS 值。
# 后面用预测值和这些真实值计算误差。
y_test <- window(CHE_tk_biom_ts, start = 2019, end = 2020) 

# x_test 是递归预测的初始输入。
# 取训练数据最后一行的两个滞后值作为第一个预测点的输入。
x_test <- ts_train_mbd[nrow(ts_train_mbd), c(1:lag_order)]


# 3) recursive forecasting with for loop
# 3 小节：使用 for 循环进行递归预测。
# 递归预测的意思是：
#   第一步预测 2019；
#   第二步预测 2020 时，会把 2019 的预测值当作新的滞后输入。


# 创建一个长度为 horizon 的空数值向量，用来存储每一步预测结果。
pred_rf <- numeric(horizon)

# 循环预测未来 horizon 年。
for (i in 1:horizon){

  # 设置随机种子，使随机森林结果可重复。
  set.seed(1) 

  # 用当前训练集 X_train 和 Y_train 训练随机森林回归模型。
  fit_rf <- randomForest(X_train, Y_train) 

  # 使用当前 x_test 预测下一期。
  # t(as.matrix(x_test)) 是把 x_test 转成一行矩阵，符合 predict() 输入格式。
  pred_rf[i] <- predict(fit_rf, t(as.matrix(x_test)))

  # 更新 x_test。
  # 新预测值放到最前面，原来的第 1 个滞后值后移。
  # 如果 lag_order = 2，则新的 x_test = c(预测值, 原先的第 1 滞后值)。
  x_test <- c(pred_rf[i], x_test[1:(lag_order-1)])

  # 更新 Y_train。
  # 去掉最早的一个训练目标，把当前预测值加入训练目标尾部。
  # 这是滚动递归训练的一种写法。
  Y_train <- c(Y_train[-1], pred_rf[i])

  # 更新 X_train。
  # 去掉最早一行特征，把新的 x_test 添加到最后一行。
  X_train <- rbind(X_train[-1, ], x_test)
}

# 输出随机森林在 log 差分尺度上的预测结果。
pred_rf


# 4) back-transforming and evaluating errors
# 4 小节：把预测值反变换回原始尺度，并计算预测误差。


# pred_rf 当前是 log 后一阶差分尺度上的预测值。
# cumsum(pred_rf)：对差分结果累加，相当于还原 log 序列的变化量。
# exp()：撤销 log 变换。
# exp_term 是相对于最后一个训练观测值的倍数变化。
exp_term <- exp(cumsum(pred_rf)) # Undoes differencing and log-transform

# 获取训练集最后一个真实观测值，即 2018 年 BIOMASS。
last_obs <- as.vector(tail(ts_train, 1)) 

# 将相对变化乘以最后一个真实值，得到原始 BIOMASS 尺度上的预测值。
backtrans_fc <- last_obs * exp_term 

# 把预测值转换为 ts 对象，起点为 2019 年。
y_pred <- ts(backtrans_fc, start = 2019, frequency = 1)

# 计算预测误差。
# forecast::accuracy() 会输出 ME、RMSE、MAE、MPE、MAPE 等指标。
# as.numeric() 是为了把 ts 对象转成普通数值向量。
forecast::accuracy(as.numeric(y_pred), as.numeric(y_test))


# fpp2：时间序列分析常用教材配套包，包含 forecast、ggplot2 等工具。
library(fpp2)

# 把原始 BIOMASS 时间序列和预测结果合并到同一个 ts 矩阵中。
# pred = c(rep(NA, length(ts_train)), y_pred)：
#   训练期没有预测值，所以用 NA 占位；
#   测试期放入 2019-2020 预测值。
ts_fc <- cbind(CHE_tk_biom_ts,pred = c(rep(NA, length(ts_train)), y_pred)) 

# 绘制真实序列和预测序列对比图。
plot_fc <- ts_fc |> autoplot() + theme_minimal() 

# 输出图。
plot_fc


#################################################
# 04- a ts pred with timetk + recipes + workflows
# 第 4 部分：使用 timetk + recipes + workflows 进行机器学习时间序列预测
#################################################

# 加载数据处理、建模和时间序列机器学习相关包。
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)


# 仍然只保留 BIOMASS 变量。
# ungroup()：取消之前的分组状态。
# select(-variable)：删除 variable 列，因为现在只剩 BIOMASS 一个变量，不再需要这列。
CHE_tk_biom <- CHE_tk_imputed |>
  filter(variable == "BIOMASS") |> 
  ungroup() |>
  select(-variable)


# 绘制 BIOMASS 时间序列。
CHE_tk_biom |>
  plot_time_series(
  DATE, value,
  .smooth = FALSE,
  .title = "BIOMASS Time Series"
)


# 1) Extracting features with timetk and perform ML
# 1 小节：使用 timetk 构造时间序列特征，然后进行机器学习建模。


# A) Calendar-based features
# A 小节：日历型特征。
# 日历型特征是从日期中提取的信息，例如年份、月份、季度、星期等。
# 对年度数据来说，最重要的通常是 year 和 index.num。


# 构造基于日历的特征。
# mutate(BIOM_log = log1p(x = value))：
#   对 value 做 log(1 + x) 变换，避免 value = 0 时 log(0) 的问题。
# mutate(BIOM_std = standardize_vec(BIOM_log))：
#   对 log 后数据标准化，使其均值约为 0，标准差约为 1。
# tk_augment_timeseries_signature(.date_var = DATE)：
#   从 DATE 中自动生成大量时间特征，如 year、month、day、index.num 等。
# glimpse()：
#   以紧凑形式查看数据结构。
biomtk_features_C <- CHE_tk_biom |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_timeseries_signature(.date_var = DATE) |>
  glimpse()

# 输出查看日历特征数据框。
biomtk_features_C


# Perform linear regression    
# 使用 plot_time_series_regression() 做线性回归并可视化拟合效果。
# .formula = BIOM_std ~ index.num + year：
#   用时间索引 index.num 和年份 year 来解释标准化后的 BIOMASS。
# .show_summary = TRUE：
#   显示回归模型摘要，包括系数、显著性、R² 等信息。
plot_time_series_regression(.date_var = DATE,
                            .data = biomtk_features_C,
                            .formula = BIOM_std ~ index.num + year,
                                .show_summary = TRUE)


# B) Fourier terms features
# B 小节：傅里叶项特征。
# 傅里叶项常用于表达周期性或波动性。
# 虽然这里是年度数据，但仍可尝试用 sin/cos 项捕捉周期波动。


# 构造傅里叶特征。
# tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1)：
#   以周期 5 年构造第 1 阶傅里叶 sin/cos 特征。
#   会生成类似 DATE_sin5_K1 和 DATE_cos5_K1 的列。
biomtk_features_F <- CHE_tk_biom |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) 

# 输出傅里叶特征数据。
biomtk_features_F


# Perform linear regression
# 用傅里叶 sin/cos 特征解释 BIOM_std。
# 作用：检查 5 年左右的周期波动能否解释 BIOMASS 的变化。
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_F,
                            .formula = BIOM_std ~ DATE_sin5_K1 + DATE_cos5_K1,
                            .show_summary = TRUE)


# C) Lag features
# C 小节：滞后特征。
# 滞后特征是时间序列预测中最常用的特征之一。
# 例如 value_lag1 表示上一年的 BIOMASS，value_lag2 表示前两年的 BIOMASS。


# 构造滞后特征。
# tk_augment_lags(.value = value, .lags = c(1, 2))：
#   为 value 生成滞后 1 期和滞后 2 期的列。
# 第一行或前两行通常会产生 NA，因为没有足够历史值。
biomtk_features_L <-  CHE_tk_biom  |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_lags(.value = value, .lags = c(1, 2))  

# 输出带有滞后特征的数据。
biomtk_features_L 


# Perform linear regression
# 用上一年和前两年的 BIOMASS 值预测当前标准化 BIOMASS。
# 如果滞后项显著，说明该时间序列有较强自相关性。
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_L,
                            .formula = BIOM_std ~ value_lag1 + value_lag2,
                            .show_summary = TRUE)


# D) rolling window statistics
# D 小节：滚动窗口统计特征。
# 滚动窗口特征可以描述局部趋势，比如最近 3 年均值、最近 6 年均值。


# 构造滚动均值特征。
# tk_augment_slidify()：
#   对 value 列计算滑动窗口统计量。
# .f = ~ mean(.x, na.rm = TRUE)：
#   窗口内部使用均值函数，忽略 NA。
# .period = c(3, 6)：
#   计算 3 年滚动均值和 6 年滚动均值。
# .partial = TRUE：
#   序列开头不足完整窗口时，也允许使用已有数据计算。
# .align = "center"：
#   窗口居中对齐。
biomtk_features_R <-  CHE_tk_biom  |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>
  tk_augment_slidify(.value   = contains("value"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")


# 输出带滚动窗口特征的数据。
biomtk_features_R


# Perform linear regression
# 使用 3 年和 6 年滚动均值解释 BIOM_std。
# 作用：检查局部平均趋势是否能解释当前生物量变化。
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_R,
                            .formula = BIOM_std ~ value_roll_3 + value_roll_6,
                            .show_summary = TRUE)


# E) put all features together 
# E 小节：把所有特征合并到一个数据框中。
# 包括：
#   1. log 和标准化后的目标变量；
#   2. 日历特征；
#   3. 傅里叶周期特征；
#   4. 滞后特征；
#   5. 滚动均值特征。


biomtk_features_all <- CHE_tk_biom  |>
  mutate(BIOM_log =  log1p(x = value)) |>
  mutate(BIOM_std =  standardize_vec(BIOM_log)) |>

  # Calendar-based (or signature) features
  # 生成时间签名特征。
  tk_augment_timeseries_signature(.date_var = DATE) |>

  # 删除一些暂时不需要或不适合年度数据的时间特征。
  # -diff：删除 diff 列。
  # -matches(...)：删除 xts、iso、小时、月份、季度、分钟、秒、日期、周等相关特征。
  # 因为本数据是一年一个点，很多细粒度日历特征没有实际意义。
  select(-diff, 
         -matches("(.xts$)|(.iso$)|(hour)|(half)|(quarter)|(month)|(minute)|(second)|(day)|(week)|(am.pm)")) |>

  # Add Fourier features
  # 添加 5 年周期的一阶傅里叶特征。
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) |>

  # Add lag features
  # 添加 value 的 1 年和 2 年滞后特征。
  tk_augment_lags(.value = value, .lags = c(1,2)) |>

  # Add rolling window statistics
  # 添加 3 年和 6 年滚动均值特征。
  tk_augment_slidify(.value   = contains("value"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")


# 查看综合特征数据结构。
biomtk_features_all |>
  glimpse()


# 使用所有核心特征做线性回归并可视化。
# 注意：这里公式中 DATE_sin5_K1 写了两次，理论上第二个可能应该是 DATE_cos5_K1。
# 如果只是运行代码，重复变量不会提供额外信息；
# 如果希望更合理，建议改成：
#   BIOM_std ~ index.num + year + DATE_sin5_K1 + DATE_cos5_K1 +
#              value_lag1 + value_lag2 + value_roll_3 + value_roll_6
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_features_all,
                            .formula = BIOM_std ~ index.num + year + 
                              # Fourier features
                              DATE_sin5_K1 + DATE_sin5_K1 + 
                              # lag features
                              value_lag1 + value_lag2 +
                              # rolling window statistics
                              value_roll_3 + value_roll_6,
                            .show_summary = TRUE)


# F) performing rf with tidymodels
# F 小节：使用 tidymodels 工作流训练随机森林模型。


# 再次加载 tidymodels。
library(tidymodels)


# 定义随机森林模型。
# rand_forest(mode = "regression")：建立回归型随机森林。
# set_engine("ranger")：使用 ranger 作为随机森林计算引擎。
rf_spec <- rand_forest(mode = "regression") |>
  set_engine("ranger")


# 创建 workflow 工作流。
# workflow()：tidymodels 中把模型、公式、预处理流程组合起来的对象。
# add_model(rf_spec)：加入随机森林模型。
# add_formula(...)：指定建模公式。
# 模型目标：BIOM_std。
# 模型特征：时间索引、年份、傅里叶项、滞后项、滚动均值。
# 注意：这里也重复写了 DATE_sin5_K1，通常建议其中一个改成 DATE_cos5_K1。
wf <- workflow() |>
  add_model(rf_spec) |>
  add_formula(BIOM_std ~ index.num + year + 
                DATE_sin5_K1 + DATE_sin5_K1 + 
                value_lag1 + value_lag2 +
                value_roll_3 + value_roll_6)


# 在完整特征数据 biomtk_features_all 上拟合随机森林模型。
# 注意：这里没有划分训练集/测试集，因此更像是“拟合已有数据”，不是严格预测评估。
rf_fit <- wf |> fit(data = biomtk_features_all)


# 使用训练好的随机森林对同一批数据进行预测。
# predict(rf_fit, biomtk_features_all)：生成预测值 .pred。
# bind_cols(biomtk_features_all)：把预测结果和原数据合并到一起。
rf_pred <- predict(rf_fit, biomtk_features_all) |>
  bind_cols(biomtk_features_all)


# 绘制真实标准化 BIOMASS 与随机森林拟合值的对比图。
# 蓝色是实际值 Actual，红色是模型拟合值 RF_pred。
# 注意：由于这里是在训练集上预测，图像更像拟合效果，而不是泛化预测能力。
ggplot(rf_pred, aes(x = DATE)) +
  geom_line(aes(y = BIOM_std, color = "Actual")) +
  geom_line(aes(y = .pred, color = "RF_pred")) +
  scale_color_manual(values = c("Actual" = "blue", "RF_pred" = "red")) +
  theme_minimal()


# 2) Extracting features with recipes and performing ML
# 2 小节：使用 recipes 自动构造特征，并结合 workflows 训练机器学习模型。
# recipes 可以把“预处理步骤”写成统一流程，便于训练集和测试集保持一致。
# https://www.r-bloggers.com/2022/01/time-series-forecasting-lab-part-3-machine-learning-with-workflows/


# A) splitting the training/test datasets
# A 小节：划分训练集和测试集。


# 计算 BIOMASS 数据总行数。
n_rows <- nrow(CHE_tk_biom)

# 训练集行数设置为总行数的 80%。
train_rows <- round(0.8 * n_rows)


# 训练集取前 80% 时间点。
# 时间序列不能随机抽样拆分，因为未来数据不能泄漏到过去。
train_data <- CHE_tk_biom |>
  slice(1:train_rows)


# 测试集取 train_rows 到最后一行。
# 注意：这里 slice(train_rows:n_rows) 会让第 train_rows 行同时出现在训练集和测试集中。
# 更严格的写法通常是 slice((train_rows + 1):n_rows)。
test_data <- CHE_tk_biom |>
  slice(train_rows:n_rows)   


# 下面这段被注释掉的代码是 timetk 推荐的时间序列切分方法。
# time_series_split() 可以更规范地划分训练集和测试集。
# assess 表示测试集长度。
# cumulative = TRUE 表示训练集从序列起点开始累计。
# 
# splits <- time_series_split(
#   CHE_tk_biom,
#   date_var   = DATE,
#   assess     = n_rows - round(0.8 * n_rows),
#   cumulative = TRUE
# )
# 
# train_data <- training(splits)
# test_data <- testing(splits) 


# 绘制训练集和测试集划分图。
# 蓝色表示训练集，红色表示测试集。
# 作用：检查切分是否符合时间顺序。
ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = value, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = value, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "BIOM") +
  theme_minimal()


# 2) creating features with recipes
# B 小节：使用 recipes 创建特征工程流程。


# recipes：tidymodels 体系中用于数据预处理和特征工程的包。
library(recipes)


# 创建 recipe 预处理配方。
# recipe(value ~ ., train_data)：目标变量是 value，其他列作为预测变量。
# step_timeseries_signature(DATE)：
#   从 DATE 中提取时间序列签名特征，比如 year、month、day、index.num 等。
# step_rm(DATE)：
#   删除原始 DATE 列，因为模型通常不能直接处理 Date 类型。
# step_zv(all_predictors())：
#   删除零方差预测变量，即所有样本都一样的列。
# step_dummy(all_nominal_predictors(), one_hot = TRUE)：
#   把分类变量转换为 one-hot 哑变量。
# step_naomit(all_predictors())：
#   删除预测变量中含 NA 的行。
recipe_spec <- recipe(value ~ ., train_data) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_naomit(all_predictors())


# prep(recipe_spec) 会基于训练集学习预处理规则。
# summary() 查看预处理后各变量的角色和类型。
summary(prep(recipe_spec))


# C) training and evaluating models
# C 小节：训练并评价机器学习模型。


# a. Training a boosted tree model
# a 小节：训练 boosted tree，也就是梯度提升树模型。


# 定义提升树模型。
# boost_tree(mode = "regression")：回归任务的 boosting tree。
# set_engine("xgboost")：使用 xgboost 作为计算引擎。
xgb_model <- boost_tree(mode = "regression") |>
  set_engine("xgboost")


# 创建 xgboost 工作流。
# add_model(xgb_model)：加入 xgboost 模型。
# add_recipe(recipe_spec)：加入前面定义好的特征工程流程。
xgb_wf <- workflow() |>
  add_model(xgb_model) |>
  add_recipe(recipe_spec)


# 在训练集 train_data 上训练 xgboost 工作流。
xgb_fit <- xgb_wf |> fit(train_data)

# 输出拟合后的模型对象。
xgb_fit 


# evaluating model performance
# 对测试集进行预测并评价模型表现。


# 使用 xgboost 模型预测测试集。
# predict() 输出 .pred 列。
# bind_cols(test_data) 把真实值和预测值合并。
xgb_pred <- predict(xgb_fit, test_data) |>
  bind_cols(test_data)


# Calculating forecast error
# 使用 yardstick::metrics() 计算预测误差。
# value 是真实值，.pred 是预测值。
# 常见输出包括 RMSE、RSQ、MAE 等。
xgb_pred |>
  metrics(value, .pred)


# 绘制 xgboost 模型的训练集、测试集真实值和测试集预测值。
# Train：训练集真实值。
# Test：测试集真实值。
# Test_pred：测试集预测值。
xgb_plot <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = value, color = "Train"), 
            linewidth = 1) +
  geom_line(data = xgb_pred, 
            aes(x = DATE, y = value, color = "Test"), 
            linewidth = 1) +
  geom_line(data = xgb_pred, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()


# 输出 xgboost 预测图。
xgb_plot


# B) training a random forest model
# B 小节：训练随机森林模型。


# 再次加载 tidymodels。
library(tidymodels)


# 定义随机森林模型。
# rand_forest(mode = "regression")：回归型随机森林。
# set_engine("ranger")：使用 ranger 引擎。
rf_model <- rand_forest(mode = "regression") |>
  set_engine("ranger")


# 创建随机森林工作流。
# add_recipe(recipe_spec) 表示使用同样的 recipes 特征工程流程。
rf_wf <- workflow() |>
  add_model(rf_model) |>
  add_recipe(recipe_spec)


# 在训练集上拟合随机森林模型。
rf_fit <- rf_wf |> fit(train_data)


# evaluating model performance
# 对随机森林模型进行测试集预测与评价。


# 预测测试集。
rf_pred <- predict(rf_fit, test_data) |>
  bind_cols(test_data)


# Calculating forecast error
# 计算随机森林预测误差指标。
rf_pred |> metrics(value, .pred)


# 绘制随机森林模型的训练集、测试集真实值和测试集预测值。
rf_plot <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = value, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_pred, 
            aes(x = DATE, y = value, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_pred, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()


# 输出随机森林预测图。
rf_plot


# 使用 patchwork 把 xgboost 图和随机森林图上下拼接，便于对比。
library(patchwork)
xgb_plot / rf_plot


# C) comparing among different algorithms
# C 小节：比较不同机器学习算法。
# 这里使用 modeltime 包对多个模型进行统一管理、校准和比较。


# create a Modeltime Table
# 创建 modeltime 模型表。
# modeltime_table() 把不同模型统一放入一个表格，便于后续比较。
model_tbl <- modeltime_table(
  xgb_fit,
  rf_fit
)

# 输出模型表。
model_tbl


# Calibration table
# 模型校准。
# modeltime_calibrate(new_data = test_data)：
#   使用测试集计算每个模型的预测残差，为后续 accuracy 和 forecast 做准备。
calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_data)

# 输出校准表。
calibrated_tbl 


# Model Evaluation
# 使用 modeltime_accuracy() 计算模型表现，并按 RMSE 从小到大排序。
# RMSE 越小，模型在测试集上的误差越小。
calibrated_tbl |>
  modeltime_accuracy(test_data) |>
  arrange(rmse)


# Forecast Plot
# 绘制多个模型的预测结果对比图。
# new_data = test_data：预测测试集。
# actual_data = CHE_tk_biom：提供完整真实数据作为背景。
# keep_data = TRUE：保留历史数据。
# plot_modeltime_forecast()：
#   生成模型预测对比图。
# .facet_ncol = 2：每行放两个模型图。
# .conf_interval_show = FALSE：不显示置信区间。
# .interactive = TRUE：生成交互式图。
calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_data,
    actual_data = CHE_tk_biom,
    keep_data   = TRUE 
  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )


# D) save the work
# D 小节：保存模型工作流结果。
# 目的：把训练好的模型和校准结果保存下来，后续可以直接读取，不用重新训练。


# 创建一个列表 workflow_Doubs。
# 其中包含：
#   workflows：保存随机森林和 xgboost 两个已训练工作流；
#   calibration：保存模型校准表。
workflow_Doubs <- list(

  workflows = list(

    wflw_random_forest = rf_fit,
    wflw_xgboost = xgb_fit

  ),

  calibration = list(calibration_tbl = calibrated_tbl)

)


# 将 workflow_Doubs 保存为 RDS 文件。
# write_rds() 是 readr 包函数，用于保存 R 对象。
# 保存后可用 read_rds("data/tsdata/workflows_Doubs_list.rds") 重新读取。
workflow_Doubs |>
  write_rds("data/tsdata/workflows_Doubs_list.rds")


# ============================================================
# 补充总结：这份脚本的整体逻辑
# ============================================================
# 1. 读取 Doubs 河流鱼类生物量数据，并筛选出 VOLPla 站点的 CHE 物种。
# 2. 使用 ts() 和 timetk 两种方式创建/整理时间序列。
# 3. 对 BIOMASS 和 DENSITY 进行时间序列可视化。
# 4. 使用 PAA 对 BIOMASS 时间序列进行简化表示。
# 5. 补齐缺失年份，并对缺失值进行插补。
# 6. 进行异常值诊断和自相关诊断。
# 7. 使用 embed() 把时间序列转成监督学习格式，并用随机森林递归预测 2019-2020。
# 8. 使用 timetk 构造多类时间序列特征：日历特征、傅里叶特征、滞后特征、滚动均值特征。
# 9. 使用 tidymodels + workflows 训练随机森林。
# 10. 使用 recipes 自动构造特征，分别训练 xgboost 和随机森林。
# 11. 使用 modeltime 对多个模型进行校准、误差比较和预测可视化。
# 12. 将最终模型工作流和校准结果保存为 RDS 文件。
#
# 需要特别注意的几个点：
# - 公式中 DATE_sin5_K1 被重复写了两次，建议其中一个改成 DATE_cos5_K1。
# - test_data <- slice(train_rows:n_rows) 会导致训练集最后一行和测试集第一行重叠。
#   更严格写法是 slice((train_rows + 1):n_rows)。
# - 在 rf_fit <- wf |> fit(data = biomtk_features_all) 这一段中，模型是在全部数据上拟合并预测全部数据，
#   这更适合展示拟合效果，不适合作为严格的测试集预测精度。
# - 时间序列预测中不建议随机打乱数据划分训练集和测试集，应保持时间先后顺序。
# ============================================================
