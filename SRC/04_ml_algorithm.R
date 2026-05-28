# --------------------------------------------
# Script Name: Machine learning
# Purpose:     The script will show how machine learning
#              works and how to build a model with data.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-03-21
# --------------------------------------------
cat("\014") # Clears the R console
rm(list = ls()) # Remove all variables

##############################################
# 01-From statistic models to machine learning
##############################################
# A) least square algorithm (statistic model)

x1 <- c(100,120,140,160,180,200,220,240,260,280)
y1 <- c(55,60,62,64,68,70,80,85,90,95)
df1 <- data.frame(x1,y1)
df1

plot(y1 ~ x1)
abline(lm(y1 ~ x1)) # check linear model
#lm 是建立线性回归模型，abline是把直线加上去

boxplot(x1, main="x", sub=paste("Outlier rows: ", # check the outliers
                               boxplot.stats(x1)$out))
#画x1的箱线图，主标题为x，在下方显示异常值
boxplot(y1, main="y", sub=paste("Outlier rows: ",
                               boxplot.stats(y1)$out))

library(e1071) # check whether the data meet normality distrib
plot(density(x1), main = "Density Plot: x", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(x1), 2)))
plot(density(y1), main = "Density Plot: y", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(y1), 2)))
# ylab为纵轴名称，round()计算偏度并保留两位小数，把结果存在副标题
lm_model1 <- lm(y1 ~ x1, data = df1)  # build a linear model
print(lm_model1) #输出模型的基本结果，包括截距和斜率
summary(lm_model1) # examine the significance

# if the dataset is df3, building a statistic model 
# may not be suitable

x2 <- c(84, 100, 180, 253, 264, 286, 400, 130, 480, 1000, 
       1990, 2000, 2110, 2120, 2300, 1610, 2430, 2500, 2590, 2680,
       2720, 2790,2880, 2976, 3870, 3910, 3960, 4320, 6670, 6900)
y2 <- c(6.176, 3.434, 3.683, 3.479, 3.178, 3.497, 4.205, 3.258,
       2.565, 4.605, 3.783, 2.833, 3.091, 2.565, 1.792, 3.045, 1.792,
       2.197, 1.792, 2.197, 2.398, 2.708, 2.565, 1.386, 1.792,
       1.792, 2.565, 1.386, 1.946, 1.099)

df2 <- data.frame(x2, y2)
plot(y2 ~ x2, df2)#明确数据的来源，如果不加df2 R会全局搜索y2和x2
abline(lm(y2 ~ x2))

lm_model2 <- lm(y2 ~ x2, data = df2)  # build a linear model
print(lm_model2)
summary(lm_model2) # examine the significance

# B) gradient descent algorithm (machine learning) 

x1 <- c(100,120,140,160,180,200,220,240,260,280)
x1_mean <- mean(x1) # standardizing the data
x1_sd <- sd(x1)
x1_std <- (x1 - x1_mean) / x1_sd

X <- cbind(1,x1_std) # # add a column of 1's as intercept

y1 <- c(55,60,62,64,68,70,80,85,90,95)

cost <- function(X, y1, theta){ # cost function
  sum(((X %*% theta -y1)^2)/2*length(y1))
}


alpha <- 0.01 # learning rate and iteration limit
num_iters <- 1000

cost_history <- rep(0, num_iters) # keep history#创建的是普通向量
theta_history <- list(num_iters)# 创建的是列表，每一次的theta都是二行一列的矩阵

theta <- matrix(c(0,0), nrow = 2) # initialize coefficients

for(i in 1:num_iters){ # gradient descent
  error <- (X %*% theta - y1)
  delta <- t(X) %*% error / length(y1) #计算梯度，t(X)表示对矩阵X转置，矩阵乘法为加和
  theta <- theta - alpha * delta #theta为现在的位置，delta为山坡往哪个方向上升，alpha控制每一步走多远
  cost_history[i] <- cost(X, y1, theta)# 记录当前损失
  theta_history[[i]] <- theta # 记录当前参数，#每一次迭代都需要保存一个矩阵，如果是theta_history[1]取出来的还是一个列表而[[1]]取出来的是矩阵本身
}

print(theta)

plot(x1_std, y1, main = "Linear regression by gradient descent")

for (i in c(1,3,6,10,14,seq(50,num_iters,by=50))) {
  abline(coef=theta_history[[i]])
}
#取 1，3，6，10，14，50，100，150，……1000这些迭代次数

abline为把这些迭代时的回归线画出来
abline(coef=theta, col='red')


#################################################
## 02-CRAT for classification and regression
#################################################
## 1) CART for regression
# https://medium.com/@justindixon91/decision-trees-afc984d161bf
# https://www.causalmlbook.com/classification-and-regression-trees-cart.html

x1 <- c(100,120,140,160,180,200,220,240,260,280)
y1 <- c(55,60,62,64,68,70,80,85,90,95)
df1 <- data.frame(x1, y1)
plot(x1, y1, pch = 21)

# A) the CART algorithm principle

# defined MSE function
mse_split <- function(x, y, s){
  left <- y[x < s] #按照切分点 s 把数据分成左边和右边
  right <- y[x >= s]
  
  if(length(left)==0 || length(right)==0) return(Inf) #如果某一边没有数据，这个切分无效，返回无限大
  
  mean_left <- mean(left)
  mean_right <- mean(right)
  
  sum((left - mean_left)^2) + sum((right - mean_right)^2) #计算左右两个区域的总平方误差。误差越小，切分点越好
}
# MSE是均方误差
# first split candidates（for all x1 and y1 values）
splits <- sort(unique(x1)) #把所有可能的 x1 值作为候选切分点

# calculate MSE for each split
mse_values <- sapply(splits, function(s) mse_split(x1, y1, s))
# 对每一个候选切分点计算 MSE
# function是一个临时函数，这个函数接受一个参数s然后把这个s代入mse_splite(x1, y1, s)里面
# 可以理解为临时函数 <- function(s){
# mse_split(x1, y1, s)
# }或者
# for(s in splits){
# mse_values <- c(mse_values, mse_split(x1, y1, s))
# }
# finding the best split
best_s <- splits[which.min(mse_values)]# 找到 MSE 最小的切分点，也就是当前最优切分点
best_s
c1 <- mean(y1[x1 < best_s])   
c2 <- mean(y1[x1 >= best_s])  
plot(x1, y1, pch = 21, bg = "lightblue")
lines(c(min(x1), best_s, best_s, max(x1)), #x坐标
      c(c1, c1, c2, c2), # y坐标
      col = "red", lwd = 2) #颜色为红色，线宽为2
abline(v = best_s, col = "blue", lty = 2)

# second split candidates (for left and right)
left_x <- x1[x1 < best_s] #取第一次切分后左边的数据
left_y <- y1[x1 < best_s]
splits_left <- sort(unique(left_x)) # 在左边区域继续寻找最优切分点
mse_values_left <- sapply(splits_left, function(s) mse_split(left_x, left_y, s))
best_s_left <- splits_left[which.min(mse_values_left)]
c1_left <- mean(left_y[left_x < best_s_left])  
c2_left <- mean(left_y[left_x >= best_s_left]) 


right_x <- x1[x1 >= best_s] #取第一次切分后右边的数据
right_y <- y1[x1 >= best_s]
splits_right <- sort(unique(right_x))
mse_values_right <- sapply(splits_right, function(s) mse_split(right_x, right_y, s))
best_s_right <- splits_right[which.min(mse_values_right)]
c1_right <- mean(right_y[right_x < best_s_right])  
c2_right <- mean(right_y[right_x >= best_s_right]) 

plot(x1, y1, pch = 21, bg = "lightblue")
lines(c(min(x1), best_s, best_s, max(x1)),
      c(c1, c1, c2, c2),
      col = "red", lwd = 2)

lines(c(min(left_x), best_s_left, best_s_left, max(left_x)),
      c(c1_left, c1_left, c2_left, c2_left),
      col = "green", lwd = 2)

lines(c(min(right_x), best_s_right, best_s_right, max(right_x)),
      c(c1_right, c1_right, c2_right, c2_right),
      col = "blue", lwd = 2)

abline(v = c(best_s, best_s_left, best_s_right), col = "blue", lty = 2)
# abline(v =)是画垂直线， h为水平线
# B) building a tree using the tree package

library(tree)

tree_model <- tree(
  y1 ~ x1,
  control = tree.control(length(y1), mincut = 1, minsize = 2) #mincut表示允许很小的切分，minsize表示一个节点至少有两个样本
)

summary(tree_model)
tree_model
tree_model$frame #查看树模型的摘要，结构和节点信息
plot(tree_model) #画出树的结构
text(tree_model) #在节点上标文字

z <- seq(min(x1), max(x1), length = 200) #生成一组连续的x1值（200个），用来画平滑/阶段预测曲线
y_pred <- predict(tree_model, newdata = data.frame(x1 = z))
plot(x1, y1, pch = 21, bg = "lightblue")
lines(z, y_pred, type = "s", col = "red", lwd = 2)
# "s"为阶梯状
# C) building a tree using the rpart package
# data and plot
x2 <- c(84, 100, 180, 253, 264, 286, 400, 130, 480, 1000, 
       1990, 2000, 2110, 2120, 2300, 1610, 2430, 2500, 2590, 2680,
       2720, 2790,2880, 2976, 3870, 3910, 3960, 4320, 6670, 6900)
y2 <- c(6.176, 3.434, 3.683, 3.479, 3.178, 3.497, 4.205, 3.258,
       2.565, 4.605, 3.783, 2.833, 3.091, 2.565, 1.792, 3.045, 1.792,
       2.197, 1.792, 2.197, 2.398, 2.708, 2.565, 1.386, 1.792,
       1.792, 2.565, 1.386, 1.946, 1.099)

df2 <- data.frame(x2, y2)
plot(x2, y2, pch=21)

# the first point for partitioning
library(tree)
thresh <- tree(y2 ~ x2)
print(thresh)
a <- mean(y2[x2<2115])
b <- mean(y2[x2>=2115])
lines(c(80, 2115, 2115, 7000),
      c(a, a, b, b))

lines(c(80, 2115, 2115, 7000), 
      c(a, a, b, b), col = "white", lwd = 2) 

# the final tree

tree_model <- tree(y2 ~ x2)
z <- seq(80, 7000) #生成从80到7000的整数序列
y2 <- predict(tree_model, list(x2 =z))# list(x2 = z)不够规范，应当为newdata = data.frame(x2 = z)
y2
lines(z, y2)
library(rpart)
tree_regres <- rpart(y2 ~ ., data = df2, method = "anova", 
                     control = rpart.control(minsplit = 10))
#. 表示使用数据框 df2 中除 y2 以外的所有其他变量作为预测变量（自变量）
#data = df2：指定训练数据为数据框 df2。
#method = "anova"：表示建立回归树（连续型因变量）。
#若因变量是分类变量，则用 method = "class"。
#control = rpart.control(minsplit = 10)：控制树生长的参数。
#minsplit = 10：每个内部节点至少包含 10 个样本才允许尝试分裂。
#较大的 minsplit 会生成更小的树（防止过拟合）。
#结果：训练好的回归树模型对象赋值给 tree_regres
print(tree_regres)
library(rpart.plot)
rpart.plot(tree_regres,main = "Regression Tree")

# 2) using CRAT algorithm for classification
# A) the CART algorithm priciple for classification

y <- c(0,0,1,0,1,2,2,2,2,2) # 3 class vector
x1 <- c(0.6,0.8,1.2,1.3,1.7,2.3,2.5,2.9,3.1,3.2) # feature1
x2 <- c(0.8,1.8,2.7,0.4,2.2,0.7,2.4,1.6,2.1,0.2) # feature2
df3 <- data.frame(y, x1, x2) 
df3
library(dplyr)
df3 <- df3 %>%
  mutate(pch_vals = case_when(
    y == 0 ~ 16,    # 16 = solid circle 类别0用实心圆
    y == 1 ~ 2,    # 17 = triangle
    y == 2 ~ 1      # 1 = hollow circle
  ))

# Plot with different shapes based on `pch_vals`
plot(df3$x1, df3$x2,
     pch = df3$pch_vals, lwd = 2,
     ylab = "x2", xlab = "x1")

#  the optimal cutoff point/split on x1 should be 2.0
abline(v = 2.0, lty = 5, lwd = 2) # lty为线型，5对应长虚线
abline(h = 2.0, lty = 5, lwd = 2)

# calculate Gini Impurity to decide The potential splits
min(x1)
max(x1)
Predictor1test <- seq(from = 0, to = 4, by = 0.1) # < min(x1) and >max(x1)
length(Predictor1test)
Predictor2test <- seq(from =0, to = 3, by = 0.1) 
length(Predictor2test)

# Function to calculate the proportion of obs in the split
CalculateP <- function(i, index, m, k) { # i候选分割点，index特征所在的列号，2表示x1; m区域“L”为左；k为类别 
  if(m=="L") { # region (m) which match to class (k) 
    Nm <- length(df3$y[which(df3[,index] <= i)]) # The number of obs in the region Rm 先通过 which 得到满足条件行的索引，再取 df2$y 对应元素，最后用 length 计数
    Count <- df3$y[which(df3[,index] <= i)] == k # The number of obs that match the class k 提取该区域中所有 y 值，判断是否等于 k，返回 TRUE/FALSE 的逻辑向量
  } else {
    Nm <- length(df3$y[which(df3[,index] > i)])
    Count <- df3$y[which(df3[,index] > i)] == k
  } 
  P <- length(Count[Count==TRUE]) / Nm # Proportion calculation
  return(c(P,Nm)) # Returns the proportion and the number of obs
}

CalculateGini <- function(x, index) { # calculate the Gini Impurity
  Gini <- NULL # Create the Gini variables
  for(i in x) {
    pl0 <- CalculateP(i, index, "L", 0) # Proportion in the left region with class 0
    pl1 <- CalculateP(i, index, "L", 1)
    GiniL <- pl0[1]*(1-pl0[1]) + pl1[1]*(1-pl1[1]) # The Fini for the left region
    pr0 <- CalculateP(i, index, "R", 0)
    pr1 <- CalculateP(i, index, "R", 1)
    GiniR <- pr0[1]*(1-pr0[1]) + pr1[1]*(1-pr1[1])
    Gini <- rbind(Gini, sum(GiniL * pl0[2]/(pl0[2] + pr0[2]),GiniR * pr0[2]/(pl0[2] + pr0[2]), na.rm = TRUE)) # Need to weight both left and right Gini scores when combining both
  }
  return(Gini)
}
# pl0[2]为左区域的总样本数Nm;pl0[1]为样本比例P
Gini <- CalculateGini(Predictor1test, 2)#Predictor1test 是之前生成的候选分割点向量
Gini
Predictor1test_gini <- cbind.data.frame(Predictor1test, Gini)
Predictor1test_gini

library(ggplot2)

ggplot(data = Predictor1test_gini, aes(x = Predictor1test, 
                                       y = Gini)) +
  geom_line() 

Gini <- CalculateGini(Predictor2test, 3)
Predictor2test_gini<- cbind.data.frame(Predictor2test, Gini)
Predictor2test_gini
ggplot(data = Predictor2test_gini, aes(x = Predictor2test, y = Gini)) +
  geom_line() 

# B) training a classification tree using rpart 分类树
library(rpart)
tree_class = rpart(y ~ ., data = df3, method = "class", 
                   control = rpart.control(minsplit = 2))
print(tree_class)
summary(tree_class)
library(rpart.plot)
rpart.plot(tree_class, main = "Classification Tree")

######################################################
## 03- Ensemble Learning for classification and regression
######################################################
# 1) Bagging algorithm 

# A) for regression

library(ggplot2)
library(caret) # evaluation performance

data(mtcars)
set.seed(123)
n <- nrow(mtcars)
B <- 50  # trainig 50 trees

pred_list <- matrix(NA, nrow = n, 
                    ncol = B) # save pred，创建矩阵储存每棵树的预测值
for(i in 1:B) {
  # Bootstrap sampling
  idx <- sample(n, n, replace = TRUE) # 有放回抽样，生成索引,sample(n, n, replace = TRUE) 表示从 1 到 n 的整数中随机抽取 n 个数，允许重复
  tr <- mtcars[idx, ] # 从原始数据中抽取 bootstrap 样本集（训练集）, 使用这个索引向量从 mtcars 中选取行，生成一个新的数据框 tr，其行数与原始数据相同（32 行），但包含重复的观测
  # 用 rpart 训练回归树（不剪枝，minsplit=2, cp=0 会使树完全生长，容易过拟合）
  tree_model <- rpart(mpg ~ ., data = tr, 
                      method = "anova", 
                      control = rpart.control(minsplit = 2, cp = 0))
  
  pred_list[, i] <- predict(tree_model, newdata = mtcars)
}
pred_list

bagging_pred <- rowMeans(pred_list) # 对每行（每个样本）求50棵树的预测均值
#计算 Bagging 预测与真实 mpg 之间的均方误差
mse <- mean((mtcars$mpg - bagging_pred)^2)
cat("Bagging Model MSE:", mse, "\n")

ggplot(mtcars, aes(x = mpg, y = bagging_pred)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlab("Actual MPG") +
  ylab("Predicted MPG") +
  ggtitle("Bagging Model: Actual vs Predicted MPG") +
  theme_minimal()

# B) for classification

# classification for the df2
y <- c(0, 0, 1, 0, 1, 2, 2, 2, 2, 2)  # labels
x1 <- c(0.6, 0.8, 1.2, 1.3, 1.7, 2.3, 2.5, 2.9, 3.1, 3.2)  # feature1
x2 <- c(0.8, 1.8, 2.7, 0.4, 2.2, 0.7, 2.4, 1.6, 2.1, 0.2)  # feature2
df3 <- data.frame(y, x1, x2)

# 
clr <- c("pink", "red", "blue", "yellow", "darkgreen",
         "orange", "brown", "purple", "darkblue")

n <- nrow(df3)
dev.new(width=10, height=10)
# set layout of 3x3 
par(mfrow = c(3, 3)) # 将绘图窗口划分为 3×3 的网格，用于显示 9 棵树的图形
# training 9 trees (B = 9),并且将树绘制到一个窗口
for(i in 1:9) {
  set.seed(123) 
  idx <- sample(n, n, replace = TRUE)  # Bootstrap sampling
  tr <- df3[idx, ]
  
  cart <- rpart(
    y ~ x1 + x2,
    data = tr,  
    method = "class", 
    control = rpart.control(minsplit = 2),
    cp = 0  # unpruned
  )
  
  prp(cart, box.col = clr[i]) #绘制该树，节点背景色取自clr
}
par(mfrow = c(1, 1)) #恢复默认单图布局

# 2) randomforest algorithm

# A) for regression

library(randomForest)
set.seed(123) 
rf_model <- randomForest(mpg ~ ., data = mtcars, ntree = 500)
print(rf_model)

rf_model$mse #每棵树的均方误差
rf_model$rsq #每棵树的R2

rf_pred <- predict(rf_model, newdata = mtcars) #用训练好的随机森林对原始数据mtcars进行预测
ggplot(mtcars, aes(x = mpg, y = rf_pred)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlab("Actual MPG") +
  ylab("Predicted MPG") +
  ggtitle("Random Forest: Actual vs Predicted MPG") +
  theme_minimal()

# for classification

# 3) the "boosting tree" for regression
# A) run one round by one round to understand the "boosting"

library(tree) # calculating residuals in decision tree 
library(caret) # calculating mean squared error
library(ggplot2) # visualizating
library(randomForest) # comparing two building models

data()
df4 <- mtcars
df4
x_vars1 <- names(df4[2:ncol(df4)]) #第一行是mpg，故而这是除mpg外的所有变量名
x_vars <- paste(x_vars1, collapse = " + ") # for convince
x_vars
# ROUND 1
df4$pred_1 <- mean(df4$mpg) # 所有样本的初始预测值 = 全局均值
df4

df4$resd_1 <- (df4$mpg - df4$pred_1) # 残差 = 真实值 - 当前预测
head(df4)

# ROUND 2
mdl <-eval(
  parse(text = 
          paste0(
            "tree(resd_1~", x_vars, ", data=df4)" #内部字符串连接
          ) # creating string with paste0
  )  # changing to expression with parse解析为 R 的表达式
) # evaluating the expression with eval； eval执行上一步生成的表达式，真正调用 tree() 函数
# 执行的是expression(tree(resd_1 ~ cyl + disp + ..., data = df4))
df4$pred_2 <- predict(mdl, df4)
head(df4)

# df4$resd_2 <- df4$mpg- (df4$pred_1 + df4$pred_2)

df4$pred_1 + (0.1*df4$pred_2) # using LR=0.1 to avoid overfitting
df4$resd_2 <- (df4$mpg- (df4$pred_1 + (0.1*df4$pred_2)))
head(df4)

# ROUND 3
mdl <-eval(parse(text = paste0("tree(resd_2~", x_vars, ", data=df4)")))
df4$pred_3 <- predict(mdl, df4)
df4
LR=0.1
df4$resd_3 <- (df4$mpg- (df4$pred_1 + (LR*df4$pred_2) + (LR*df4$pred_3)))
head(df4)

# B) writing a "for" loop to complete a "boosting" process

library(tree)
library(caret) 
library(ggplot2)
library(randomForest)

LR <- 0.15
nrounds <- 50

df4 <- mtcars
x_vars1 <- names(df4[2:ncol(df4)])
x_vars <- paste(x_vars1, collapse = " + ")

prediction <- NaN #创建非数值变量
df4 <- cbind(df4[1], prediction, df4[2:ncol(df4)])
head(df4)

# ROUND 1
df4$pred_1 <- mean(df4$mpg)
df4
df4$prediction <- df4$pred_1
df4
df4$resd_1 <- (df4$mpg - df4$prediction)
df4

rmse <- RMSE(df4$mpg, df4$prediction) # RMSE() of caret等价于sqrt(mean((df4$mpg - df4$prediction)^2))
results <- data.frame("Round" = c(1), "RMSE" = c(rmse))

# a for loop from ROUND 2

for (i in 2:nrounds){
  mdl <-eval(parse(text = paste0("tree(resd_", i-1, "~", x_vars, ", 
                                 data=df4)")))
  df4[[paste0("pred_", i)]] <- predict(mdl, df4)
  
  df4$prediction <- df4$prediction + # here includes ROUND 1
    (LR*df4[[paste0("pred_", i)]])
  df4[[paste0("resd_", i)]] <- (df4$mpg- df4$prediction) # 计算残差
  
  rmse <- RMSE(df4$mpg, df4$prediction)
  results <- rbind(results, list("Round" = i, "RMSE" = rmse)) # 将本轮结果追加到 results 数据框
}
results

# 4) compare the boosting algorithm to tree and rf models
# tree model
tree_mdl <-eval(parse(text = paste0("tree(mpg~", x_vars, ", 
                                    data=df4)")))
prediction <- predict(tree_mdl, df4)
tree_rmse <- RMSE(df4$mpg, prediction)

# rf model
rf_mdl <-eval(parse(text = paste0("randomForest(mpg~", x_vars, ", 
                                  data=df4)"))) # 默认ntree=500, ntry =p/3
prediction <- predict(rf_mdl, df4)
rf_rmse <- RMSE(df4$mpg, prediction)

ggplot() +
  geom_line(data = results, aes(x=Round, y=RMSE)) +
  geom_hline(yintercept = tree_rmse, color = "red", linetype = "dashed") +
  geom_hline(yintercept = rf_rmse, color = "blue", linetype = "dashed") 


################################################
## 04-build models and optimize their parameters
##    to obtain high performance
################################################
rm(list = ls())

data() 
data("mtcars")
str(mtcars)
?mtcars

# 1) for a tree model between mpg and others

# A) Split data into train and test (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))# 随机抽取70%的行索引
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]
head(test_data)

# B) find the most optimum parameters for a tree model
# https://danstich.github.io/stich/classes/BIOL217/12_cart.html
# https://rpubs.com/mpfoley73/529130

library(rpart)
?rpart
library(rpart.plot)
rpart_full <- rpart(mpg ~ ., data = train_data, 
                  method = "anova",
                  minsplit = 2, minbucket = 1,#叶子节点至少1个样本
                  xval = 5) # 5-fold cross-validation
print(rpart_full)

printcp(rpart_full)
plotcp(rpart_full)
opt_index <- which.min(rpart_full$cptable[,"xerror"])# 找到最小交叉验证误差的行索引
opt_cp <- rpart_full$cptable[opt_index, "CP"] # 获取对应的 cp 值
opt_cp
rpart_pruned <- prune(rpart_full, cp = opt_cp)# 根据最优 cp 剪枝
print(rpart_pruned)
rpart.plot(rpart_pruned)
rpart.plot(rpart_full) #对比未剪枝的完全树

# C) model evaluation on test_data using R2 and RMSE
rpart_pred <- predict(rpart_pruned, test_data, type = "vector") 
library(caret)
rpart_R2 = R2(rpart_pred, test_data$mpg) #决定系数R2
rpart_R2 
rpart_rmse = RMSE(rpart_pred, test_data$mpg)
rpart_rmse 

# 2) for a rf model between mpg and others

# A) Split data for proper evaluation (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]

# B) pre-train a rf model

library(randomForest)
set.seed(123)
rf_model <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = 500,
  mtry = 3,   # 初始值（p=10 → p/3≈3）
  importance = TRUE # 计算变量的重要性
)

print(rf_model) # Mean of squared residuals → OOB MSE
plot(rf_model)

# C) find the most optimum parameters for a rf model
# mtry → ntree → nodesize（using OOB）
# https://www.geeksforgeeks.org/r-machine-learning/how-to-calculate-the-oob-of-random-forest-in-r/

# optimize mtry based on OOB 

mtry_res <- tuneRF(
  x = train_data[, -1],  # 预测变量矩阵（去掉 mpg 列）
  y = train_data$mpg,    # 目标变量
  stepFactor = 1.5,      # 每次尝试 mtry 乘以 1.5 或除以 1.5
  improve = 0.01,        # 要求 OOB 误差改善至少 1% 才继续
  ntreeTry = 500,        # 每轮试验用的树数量
  trace = TRUE,          # 打印进度
  plot = TRUE            # 绘制 mtry 与 OOB 误差的关系图
)

mtry_res

best_mtry <- mtry_res[which.min(mtry_res[,2]), 1] #mtry_res[,2]提取第二列,which.min(mtry_res[, 2])：找到 OOB 误差最小值所在的行索引;在 mtry_res 中取该行的第一列，即对应的 mtry 值
best_mtry

# re-train a rf model with optimal mtry

rf_best <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = 500,
  mtry = best_mtry, #用最优mtry重新训练模型
  importance = TRUE
)

print(rf_best)

# optimize ntree after best_mtry

rf_temp <- randomForest(mpg ~ ., data = train_data, mtry=best_mtry,
                        ntree = 1000)

plot(rf_temp$mse, type = "l", xlab = "Number of Trees", ylab = "OOB MSE")
#训练一个包含 1000 棵树的模型，绘制 OOB 误差随树数量增加的曲线，目的是找到误差趋于稳定所需的最少树数量
mse_vals <- rf_temp$mse # 每棵树的 OOB MSE 向量
diff_mse <- abs(diff(mse_vals)) # 相邻两棵树的 MSE 变化量
threshold <- 1e-4 # 设定阈值
best_ntree <- which(diff_mse < threshold)[1] # 第一个变化小于阈值的位置
best_ntree
# re-train rf with best_mtry and best_ntree

rf_best <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = best_ntree,
  mtry = best_mtry,
  importance = TRUE
)

print(rf_best)

# search optimal nodesize with OOB (叶子节点最小样本数)

nodesize_vals <- c(3, 5, 10) #候选值，回归默认为5
nodesize_res <- data.frame()

for (n in nodesize_vals) {
  rf <- randomForest(
    mpg ~ ., data = train_data,
    ntree = best_ntree,
    mtry = best_mtry,
    nodesize = n
  )
  
  nodesize_res <- rbind(nodesize_res, data.frame(
    nodesize = n,
    OOB_MSE = oob_mse <- rf$mse[rf$ntree] # the final tree,这里就是指所有树集成完成后的最终稿OOB MSE
  ))
}
#rf$mse：随机森林模型对象中存储的 OOB 均方误差向量。向量的长度等于树的数量（ntree），其中第 i 个元素表示前 i 棵树集成的 OOB MSE
nodesize_res

best_nodesize <- nodesize_res$nodesize[which.min(nodesize_res$OOB_MSE)]
best_nodesize

plot(nodesize_res$nodesize, nodesize_res$OOB_MSE, type = "b",
     xlab = "nodesize", ylab = "OOB MSE",
     main = "Nodesize Tuning")

# re-train rf with optimal mtry, ntree, nodesize

rf_final <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = best_ntree,
  mtry = best_mtry,
  nodesize = best_nodesize,
  importance = TRUE
)

# D) evaluation on test data

rf_pred <- predict(rf_final, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$mpg - rf_pred)^2))
rf_R2 <- 1 - sum((test_data$mpg - rf_pred)^2) /
  sum((test_data$mpg - mean(test_data$mpg))^2)

rf_rmse
rf_R2

# E) view the importance of features
importance(rf_final) # 打印两种重要性指标：%IncMSE 和 IncNodePurity
# %IncMSE：将某个变量随机打乱后，OOB 误差增加的百分比，越大表示该变量越重要
# IncNodePurity：该变量在所有分裂中减少的节点不纯度总和（回归时为 MSE 减少量）
varImpPlot(rf_final)

# 3) for a boosting tree between mpg and others

# A) Split data for proper evaluation (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]

# B) find the most optimum parameters for a gbm model
# shrinkage → depth → n.trees（using CV）

library(gbm)
gbm_model <- gbm(
  mpg ~ ., 
  data = train_data,
  distribution = "gaussian",   # for regres回归任务
  n.trees = 2000,              #初始设定较大的树数量
  interaction.depth = 3,       #每棵树的深度
  shrinkage = 0.01,            #学习率
  n.minobsinnode = 3,          #内部节点最小样本数
  cv.folds=5)

# best n.trees
best_iter <- gbm.perf(gbm_model, method = "cv") # 表示基于交叉验证误差的最小值
best_iter

# best depth and nodesize
library(gbm)

shrinkage_vals <- c(0.05, 0.01)
depth_vals <- c(1, 2, 3)
minobs_vals <- c(1, 2, 3)

gbm_res <- data.frame()

set.seed(123)

for (s in shrinkage_vals) {
  for (d in depth_vals) {
    for (m in minobs_vals) {
      
      model <- tryCatch({
        gbm(
          mpg ~ .,
          data = train_data,
          distribution = "gaussian",
          n.trees = ifelse(s == 0.05, 1500, 3000),  # ✅ 根据学习率调整树数，学习率为0.05时为1500棵
          interaction.depth = d,
          shrinkage = s,
          n.minobsinnode = m,
          bag.fraction = 0.8,    # 每轮随机采样 80% 样本（类似 Bagging）
          cv.folds = 3,         
          verbose = FALSE
        )
      }, error = function(e) return(NULL)) #防止循环因某个参数组合的模型训练失败而中断
      
      
      if (is.null(model)) next  
      
      best_iter <- gbm.perf(model, method = "cv", plot.it = FALSE)
      
      gbm_res <- rbind(gbm_res, data.frame(
        shrinkage = s,
        depth = d,
        minobs = m,
        trees = best_iter,
        error = min(model$cv.error) # 记录该参数组合下最小的交叉验证误差
      ))
    }
  }
}

gbm_res

best_params <- gbm_res[which.min(gbm_res$error), ]
best_params

# re-train a gbm model with best parameters
gbm_final <- gbm(
  mpg ~ .,
  data = train_data,
  distribution = "gaussian",
  n.trees = best_params$trees,
  interaction.depth = best_params$depth,
  shrinkage = best_params$shrinkage,
  n.minobsinnode = best_params$minobs
)

# C) evaluation on test data

gbm_pred <- predict(gbm_final, newdata = test_data)
gbm_rmse <- RMSE(test_data$mpg, gbm_pred)
gbm_rmse 
#打印三个模型的 RMSE，直观对比性能
cat("rpart RMSE: ", rpart_rmse, "\n")
cat("gbm RMSE: ", gbm_rmse, "\n")
cat("rf RMSE: ", rf_rmse, "\n")

results <- data.frame(
  Actual = test_data$mpg,
  rf_Pred = rf_pred,
  gbm_Pred = gbm_pred,
  rpart_Pred = rpart_pred
)
results
results_long <- reshape(results, 
                        varying = c("rf_Pred", "gbm_Pred", "rpart_Pred"),  #需要“拉长”的列名向量，即原数据框中代表不同模型预测值的列
                        v.names = "Prediction",  #新数据框中用于存放这些数值的列名
                        timevar = "Model",  #新数据框中用于标识原始列来源的变量名
                        times = c("rf", "gbm", "rpart"), #为每个 varying 列赋予的标签，顺序必须与 varying 一致
                        direction = "long") #将宽格式变为长格式，便于绘图
results_long
ggplot(results_long, aes(x = Actual, y = Prediction, color = Model)) + #不同颜色代表不同的模型
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Model Comparison: Actual vs Predicted MPG",
       x = "Actual MPG", y = "Predicted MPG") +
  theme(legend.position = "top")

#############################################
## 05-build machine learning models by caret
#############################################
# https://r.qcbs.ca/workshop04/book-en/multiple-linear-regression.html
rm(list = ls())

library(caret)

modelnames <- paste(names(getModelInfo()), collapse=',')
modelnames

modelLookup("rpart")
modelLookup("rf")
modelLookup("gbm")

#######################################################
# 1) train reg models between mpg and others of mtcars
#######################################################

# A) load and split data 
data("mtcars")

set.seed(123)
Index <- createDataPartition(mtcars$mpg, p = 0.7, list = FALSE)  #分层抽样，按 mpg 分组
train <- mtcars[Index,]
test <- mtcars[-Index,]
# # B) pre-Processing the data setting for training 预处理
##########################################################
# skipping the whole B) step and running the C) step
##########################################################
# # a. one-hot encoding（categories → numeric）

library(tidyverse)
dmy <- dummyVars(~ ., data = train) #dummyVars 将因子变量转换为虚拟变量
train_dmy <- predict(dmy, train) %>% 
  as.data.frame()
class(train_dmy)
test_dmy  <- predict(dmy, test) %>% 
  as.data.frame()

# b. impute missing and center/scale 

library(skimr) #用于汇总统计
skim(train_dmy)
skim(test_dmy)
# skim() 输出每个变量的：数据类型（numeric, factor 等），缺失值数量，均值、标准差、分位数（最小值、25%、50%、75%、最大值），直方图
preproc <- preProcess(train_dmy,
                  method = c("medianImpute", "center", "scale"))
# medianImpute：用中位数填补每个变量的缺失值（适用于数值型变量）
# center：中心化，即减去均值，使变量均值为 0。
# scale：缩放，即除以变量的标准差，使变量标准差为 1
train_preproced <- predict(preproc, train_dmy) #predict() 将训练好的预处理对象 preproc 应用到训练集 train_dmy 上，实际执行：填补缺失值 → 中心化 → 缩放。
head(train_preproced)
test_preproced  <- predict(preproc, test_dmy)

# c. deleting the variables with zero variance 

nzv_cols <- nearZeroVar(train_preproced) #扫描数据框 train_preproced 的每一列，判断是否为“近零方差”变量
cols_keep <- setdiff(seq_along(train_preproced), nzv_cols) #seq_along(train_preproced) 生成一个整数序列，从 1 到 ncol(train_preproced)，即所有列的位置。
cols_keep
#setdiff(x, y) 返回在 x 中但不在 y 中的元素，
#cols_keep 就是剔除近零方差列之后需要保留的列索引
train_final <- train_preproced[, cols_keep, drop = FALSE]
#从预处理后的训练集 train_preproced 中，只选取 cols_keep 指定的列，生成新的数据框 train_final
#drop = FALSE 表示即使只选中一列，结果仍然是数据框
test_final  <- test_preproced[, cols_keep, drop = FALSE]
dim(train_final)
dim(test_final)
#打印最终训练集和测试集的维度（行数和列数）

# d. self-defining re-sampling process for validation

fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds
                           repeats = 3)    # repeated 3 times
# ml_rpart <- train(...
#                   trControl = fitControl,
#                   ...
#                   ) 

# e. self-defining way for finding hyperparameters 

# the ways include tunelength (automatically),
# tuneGrid (manually) and search = “random”,

##############################################################
# C) training and evaluating models with caret
# a. a tree model

data("mtcars")

set.seed(123)
Index <- createDataPartition(mtcars$mpg, p = 0.7, list = FALSE) 
train <- mtcars[Index,]
test <- mtcars[-Index,]


fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds
                           repeats = 3)    # repeated two times
# Ranger（快速实现决策树/随机森林的替代）
model_ranger <- train(mpg ~ ., data = train,
                     method = "ranger", # the tree algorithm,ranger比rf更快，可以通过tuneLength自动优化mtry等参数
                     trControl = fitControl, ## 重采样控制对象（定义交叉验证方案）
                     preProcess = c('medianImpute', 'nzv','scale', 'center'), #preProcess 直接在训练时对原始 train 数据执行（填补、去零方差、中心化、缩放）
                     tuneLength = 5,# searching five cp，每个参数尝试5个值
                     metric="RMSE")  # 模型选择时的优化指标
# Predict on the test data
pred_ranger <- predict(model_ranger, newdata = test)# 是上面经过caret::train训练好的模型对象
# evaluate regression performance
rmse_ranger <- caret::RMSE(test$mpg, pred_ranger) #caret::RMSE() 是 caret 包中计算均方根误差的函数
R2_ranger <- caret::R2(test$mpg, pred_ranger)
# b. a rf regression

model_rf <- train(mpg ~ ., data = train, 
                  method = "rf",# rf algorithm
                  trControl = fitControl,
                  preProcess = c('medianImpute', 'nzv','scale', 'center'),
                  tuneLength = 5,
                  metric="RMSE") 

pred_rf <- predict(model_rf, newdata = test)
rmse_rf <- caret::RMSE(test$mpg, pred_rf)
R2_rf <- caret::R2(test$mpg, pred_rf)

# c. a boosting regression

model_gbm <- train(mpg ~ ., data = train, 
                  method = "gbm",# gbm algorithm
                  trControl = fitControl,
                  tuneGrid = expand.grid(
                    n.trees = 30,
                    interaction.depth = 1,
                    shrinkage = 0.1,
                    n.minobsinnode = 3),
                  metric="RMSE") 

pred_gbm <- predict(model_gbm, newdata = test)
rmse_gbm <- caret::RMSE(test$mpg, pred_gbm)
R2_gbm <- caret::R2(test$mpg, pred_gbm)

# d. Compare the models' performances for final picking
models_compare <- resamples(list(RANGER=model_ranger, 
                                 RF=model_rf, 
                                 GBM=model_gbm)) #resamples()：从多个 train 对象中提取重采样（resampling）的性能指标
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), 
               y=list(relation="free")) # 每个面板的坐标轴范围自由
bwplot(models_compare, scales=scales) #bwplot 会为每个指标生成一个盒形图面板
#################################################
# 2) building classification models for iris
#################################################
rm(list = ls())

# A) loading and splitting data

data(iris) 
head(iris)

set.seed(123)
index_iris <- createDataPartition(iris$Species, p=0.8, list=FALSE) # 
train_dat <- iris[index_iris,]
test_dat <- iris[-index_iris,]

###############################################
# skipping the whole B) and running the C)
###############################################
# B) training a model with diff optimal mtry
# feature selection 特征分布可视化
featurePlot(x = iris[, 1:4], y = iris$Species, plot = "density",
            scales = list(x = list(relation = "free"), y = list(relation="free")), #控制坐标轴刻度
            pch = "|", #在密度曲线下方添加地毯（rug），用竖线标记每个样本的位置
            layout = c(4, 1), #将四个子图排成 1 行 4 列
            auto.key = list(columns = 3))

# 递归特征消除（RFE）
set.seed(123) 
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x = iris[, 1:4], y = iris$Species, rfeControl = ctrl)
lmProfile

# a. using default trainControl for optimal mtry 使用 caret 包训练一个随机森林分类模型
#利用默认的重采样方法（bootstrap）自动寻找最优的 mtry 参数
# i.e. trainControl(method = "boot", number = 25)
set.seed(123)
rf_fit1 <- train(Species~., 
                 data = train_dat, 
                 method="rf")  
#有显式提供 trControl 参数：此时 train() 会使用默认的重采样方案，即 bootstrap 法（有放回抽样），
#共进行 25 次（method = "boot", number = 25）
# rf_fit1 <- train(Species~., 
#                  data = train_dat, 
#                  method="rf",
#                  trControl = trainControl(method = "boot", 
#                                           number = 25))  

rf_fit1
plot(rf_fit1)

# b. using self-defined trainControl for optimal mtry

fitControl <- trainControl(method = "repeatedcv", number = 5, 
                           repeats=3) 

set.seed(123)
rf_fit2 <- train(Species ~ ., data = train_dat, method = "rf",
                 trControl = fitControl) 

rf_fit2


# c. self-defined tuneLength for optimal mtry
fitControl <- trainControl(method = 'repeatedcv', 
                           number = 5, 
                           repeats =3,
                           savePredictions = 'final', # keep results保存最终模型
                           classProbs = TRUE, # prob values 要求模型为每个样本输出属于各个类别的概率               
                           summaryFunction=multiClassSummary) # metrics 要求模型为每个样本输出属于各个类别的概率
install.packages("MLmetrics")
library(caret)
rf_fit3 <- train(Species ~ ., data = train_dat, method = "rf", 
                 tuneLength = 3, # optimal mtry
                 trControl = fitControl,
                 verbose = FALSE) #不打印过程的详细信息

rf_fit3

# rf_pred <- predict(rf_fit3, test_dat)
# rf_pred
# caret::confusionMatrix(reference = test_dat$Species, 
#                        data = rf_pred, # 用test评估模型
#                        mode = "everything")

# d. self-defined tuneGrid for optimal mtry
fitControl <- trainControl(method = 'repeatedcv', 
                           number = 5, 
                           repeats =3,
                           savePredictions = 'final', # keep results
                           classProbs = TRUE, # prob values                
                           summaryFunction=multiClassSummary) # metrics

tune_grid <- expand.grid(mtry = c(1, 2, 3, 4)) #expand.grid()：创建所有参数组合的数据框
set.seed(123) 
rf_fit4 <- train(Species ~ ., data = train_dat,  method = "rf",
                 tuneGrid = tune_grid,
                 trControl = fitControl,
                 metric = "Accuracy") #指定用 准确率（Accuracy） 作为选择最优 mtry 的指标
rf_fit4
##########################################################

# C) training a model with preProcess, trControl and optimal mtry

fitControl <- trainControl(method = 'repeatedcv', 
                           number = 5, 
                           repeats =3,
                           savePredictions = 'final', # keep results
                           classProbs = TRUE, # prob values                
                           summaryFunction=multiClassSummary) # metrics
#完整的随机森林建模流程，缺失值处理，去噪，标准化，稳健的模型评估，自动超参数调优
set.seed(123)
rf_fit5 <- train(Species ~ .,
                 data = train_dat, 
                 method = "rf",
                 preProcess = c("nzv", "center", "scale", "knnImpute"),#预处理
                 na.action = na.pass,  #允许输入数据包含缺失值
                 trControl = fitControl,
                 tuneLength=5, #自动超参数调优
                 metric = "Accuracy") 
rf_fit5

# D) simultaneously running several algorithms for classification

library(caretEnsemble) #扩展了 caret 的功能

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  savePredictions = "final", 
  classProbs = TRUE # calculating ROC
)

set.seed(123)
multi_models <- caretList(
  Species ~ ., 
  data = train_dat, 
  trControl = fitControl,
  methodList = c('ranger', 'rf',  'gbm'), #指定要训练的模型列表
  preProcess = c("nzv", "center", "scale", "knnImpute"), #预处理：移除近零方差变量 → 中心化 → 缩放 → KNN 填补缺失值
  tuneLength=3,
  metric = "Accuracy"
)

names(multi_models)
multi_models$rf

# pred_rf <- multi_models$rf$pred
# cm_rf <- confusionMatrix(data = pred_rf$pred, 
#                          reference = pred_rf$obs)
# print(cm_rf) 
# 
# pred_ranger <- multi_models$ranger$pred
# cm_ranger <- confusionMatrix(data = pred_ranger$pred, 
#                              reference = pred_ranger$obs)
# print(cm_ranger)

# Resample results
resamples_list <- resamples(multi_models) # 从多个 train 对象中提取重采样指标
summary(resamples_list) #输出每个模型在各指标上的汇总统计

# Plot the results

dotplot(resamples_list, metric = "Accuracy") #绘制每个模型在每次重采样（15 个验证集）上的准确率点图
bwplot(resamples_list) #绘制箱线图，展示准确率（以及其他指标）的分布

##########################################
## 06-visualizing machine learning models
##########################################
data(mtcars)
head(mtcars)
set.seed(123)
idx <- createDataPartition(mtcars$mpg, p=0.8, list = FALSE)
train <- mtcars[idx, ]
test  <- mtcars[-idx, ]
train_control <- trainControl(method="cv", number=3)
# trainControl()：定义重采样方法，用于内部验证
# 1) training and visualizing models

# A) linear regression model

library(caret)
caret_lm_mdl <- train(mpg ~ wt, data = train, method = "lm", #lm是线性回归
                      trControl = train_control)
print(caret_lm_mdl)
pred <- predict(caret_lm_mdl, test)
rmse <- RMSE(test$mpg, pred) # Root Mean Squared Error
R2 <- R2(test$mpg, pred)

plot(mtcars$wt, mtcars$mpg, main="caret_lm Models")
abline(caret_lm_mdl, col="blue")

# B) a decision tree regression
# a. training and retraining with optimal cp
library(caret)
rpart_mdl <- train(
  mpg ~ wt,
  data = train,
  method = "rpart",
  trControl = train_control,
  tuneLength = 10,              
  control = rpart.control(minsplit = 2)
)

print(rpart_mdl)
plot(rpart_mdl)

best_cp <- rpart_mdl$bestTune$cp
best_cp

final_rpart_mdl <- train(
  mpg ~ wt,
  data = train,
  method = "rpart",
  trControl = trainControl(method = "none"),  # not necessary，不进行重采样
  tuneGrid = data.frame(cp = best_cp),       # fix cp
  control = rpart.control(minsplit = 2)
)

# b. visualizing the tree diagram 
library(rpart.plot)
rpart.plot(
  final_rpart_mdl$finalModel,
  type = 2,       
  extra = 101,   #101 表示显示“样本数/均值”
  under = TRUE   #放在下面
)

# c. visualizing the step plot 
wt_grid <- seq(min(train$wt), max(train$wt), length.out = 200) # 200 个等间距的数值
pred <- predict(final_rpart_mdl$finalModel, 
                newdata = data.frame(wt = wt_grid))
#predict() 用训练好的 rpart 模型（final_rpart_mdl$finalModel）对新的 wt 值进行预测
plot(train$wt, train$mpg,
     pch = 16, col = "blue",
     xlab = "wt", ylab = "mpg",
     main = "Regression Tree as Step Function")

lines(wt_grid, pred, type = "s", col = "red", lwd = 2)

# C) random forest 
# a. training a rf model
rf_mdl <- train(mpg ~ wt, data = train, method = "rf")  # mtry
print(rf_mdl)

# b. visualizing individual trees
library(rpart)
library(rpart.plot)  # prp()

n <- nrow(train)
clr <- rainbow(6) # 生成6种颜色
par(mfrow = c(2,3))

# for the front 6 trees
for(i in 1:6) {
  set.seed(123 + i) #每棵树使用不同的随机种子
  idx <- sample(n, n, replace = TRUE)  # Bootstrap sample
  tr <- train[idx, ]

  cart <- rpart(
    mpg ~ wt,          
    data = tr,  
    method = "anova",  # 回归树 
    control = rpart.control(minsplit = 2),
    cp = 0             
  )
  
  prp(cart, box.col = clr[i], main = paste("Tree", i))
}

par(mfrow = c(1,1))

# c. visualizing the smooth curve

wt_grid <- seq(min(train$wt), max(train$wt), length.out = 200)
rf_pred <- predict(rf_mdl, newdata = data.frame(wt = wt_grid))

plot(train$wt, train$mpg, pch = 16)
lines(wt_grid, rf_pred, lwd = 2)

# D) a boosting tree
# a. training and retraining the model
library(caret)
library(gbm)  

tuneGrid = expand.grid(
  interaction.depth = c(1, 2),
  n.trees = c(50, 100),
  shrinkage = c(0.05, 0.1),
  n.minobsinnode = c(2, 3, 5)  
)

set.seed(123)
gbm_mdl <- train(mpg ~ wt, data = train, method = "gbm",
  trControl = train_control,
  tuneGrid = tuneGrid, # 超参数组合
  bag.fraction = 0.8,   # 或 1
  verbose = FALSE
)

print(gbm_mdl)
plot(gbm_mdl)       
gbm_mdl$bestTune

gbm_final <- train(mpg ~ wt, data = train, method = "gbm",
  trControl = trainControl(method = "none"), # 
  tuneGrid = gbm_mdl$bestTune,
  verbose = FALSE)

# b. visualizing the smooth curve 单独绘制 GBM 拟合曲线

wt_grid <- seq(min(train$wt), max(train$wt), length.out = 200)
pred <- predict(gbm_final, newdata = data.frame(wt = wt_grid))
plot(train$wt, train$mpg, pch = 16, col = "blue",
     xlab = "Weight (wt)", ylab = "MPG",
     main = "Boosted Regression Tree (GBM) Fit")
lines(wt_grid, pred, col = "red", lwd = 2)

# drawing all curves on a map 多模型叠加对比
wt_grid <- seq(min(train$wt), max(train$wt), length.out = 200)
newdata <- data.frame(wt = wt_grid)

pred_lm    <- predict(lm_mdl, newdata)
pred_rpart <- predict(rpart_mdl, newdata)
pred_rf    <- predict(rf_mdl, newdata)
pred_gbm   <- predict(gbm_mdl, newdata)

plot(train$wt, train$mpg,
     pch = 16, col = "black",
     xlab = "wt", ylab = "mpg",
     main = "LM vs rpart vs RF vs GBM")

lines(wt_grid, pred_rpart, type = "s",  col = "black",lwd = 2)
lines(wt_grid, pred_rf, col = "blue", lwd = 2, lty = 2)
lines(wt_grid, pred_lm,  col = "darkgreen", lwd = 2, lty = 3)
lines(wt_grid, pred_gbm,  col = "red", lwd = 2, lty = 4)
legend("topright",
       legend = c("Data",
                  "rpart (step)",
                  "Random Forest",
                  "Linear Model",
                  "GBM"),
       col = c("black", "black", "blue", "darkgreen", "red"),
       pch = c(16, NA, NA, NA, NA),
       lty = c(NA, 1, 2, 3, 4),
       lwd = 2)
-