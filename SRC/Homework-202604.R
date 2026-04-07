#NO1
#从建模目标、建模算法和评估方面，阐述机器学习建模与传统统计模型有哪些不同？
#1.建模目标不同：
#2.建模算法不同：
#3.评估方面不同：
library(ade4)
library(dplyr)
data(doubs)
doubs
doubs$fish
doubs_data <- doubs$env
doubs_data$abund <- rowSums(doubs$fish)
library(caret)
set.seed(123)
index <- createDataPartition(doubs_data$abund, p = 0.7, 
                             list = FALSE)
training_data <- doubs_data[index,]
testing_data <- doubs_data[-index,]
model_rf <- train(abund ~ .,  data = training_data,  method = "rf") 
fitControl <- trainControl(method = "repeatedcv",  number = 10, repeats = 5)
model_rf <- train(abund ~ ., data = training_data, method = "rf", trControl =fitControl)
model_rf <- train(abund ~ ., data = training_data,  method = "rf", 
                  preProcess = c('scale', 'center'),
                  trControl =fitControl)
grid <- expand.grid(.mtry = c(1:10))
model_rf <- train(abund ~ ., data = data_train, method = "rf",
                  preProcess = c('scale', 'center'),
                  trControl = fitControl,
                  tuneGrid = grid)
model_rf




