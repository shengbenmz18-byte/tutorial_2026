# --------------------------------------------
# Script Name: Basic R
# Purpose: This section introduces some examples about basic 
#          manipulation on data and self-defined functions.
#          
# Author:  Fanglin Liu
# Email:   flliu315@163.com
# Date:    2026-03-21
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

#################################################
## 01-data manipluation using tidyverse
#################################################
# 1) built-in datasets

data()  # Data sets in package ‘datasets’

data(package = .packages(all.available = TRUE)) # list the data sets in all *available* packages

all_datasets <- data(package = "datasets")$results[, "Item"] # Counting Number of Built-in Datasets
length(all_datasets)

data(package = "ade4") 
library(ade4)
data(doubs) # load the dataset
head(doubs) # Checking the Structure of the Dataset
head(doubs$env)
str(doubs)
colnames(doubs$env) # Checking Column Names

# 2) manipulation with tidyverse 
library("tidyverse")  # load the tidyverse packages, incl. dplyr

# A) select(), filter(), mutate(), and pipe using dplyr

# use pipes for manipulating data
doubs$env %>% 
  select(dfs, alt, oxy) %>%   # select dfs, alt, pen
  filter(alt > 300)  # filter alt > 300 

# use mutate() to create a new column
doubs$env %>% 
  filter(!is.na(oxy)) %>% 
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) %>% 
  head()

# Split-apply-combine data analysis
doubs$env %>% 
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) %>% 
  group_by(oxygen_category) %>% #按oxygen_categoory分组
  summarise(mean_alt = mean(alt), #计算该组的平均海拔
            mean_pH = mean(pH), 
            .groups = "drop") # 汇总完成后取消分组状态

# summary the above steps as follows

doubs$env %>%  
  select(dfs, alt, oxy, pH) %>%  # select dfs, alt, oxy, pH
  filter(alt > 200) %>%   # keep the rows in which alt > 200
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) %>%   
  rename(distance = dfs, oxygen = oxy) %>%    # rename，写法是新名字=旧名字
  arrange(desc(alt)) %>%  # order by alt decrease，desc() 表示 descending，也就是降序，按照 alt 从大到小排序
  group_by(oxygen_category) %>%  # groups by oxy
  summarise(mean_alt = mean(alt), 
            mean_pH = mean(pH), 
            .groups = "drop")  

# B) Reshaping doubs between long and wide format
# a. using with tidyr::gather and spread
#长表更加适合画图，分析
long_env <- doubs$env |> # from wide format to long 
  gather(key = "variable", value = "value", #原来的列名放到variable这一列，数值放到value这一列
         -dfs) # keep dfs column
long_env
head(long_env)


# reverse the gather() operation
wide_env <- long_env |> # Convert back to wide format
  spread(key = "variable", value = "value")#用 variable 这一列里的名字变成新列名,用 value 这一列里的数值填进去
wide_env
head(wide_env)

# b. using pivot_longer() and pivot_wider()

long_env_new <- doubs$env |> 
  pivot_longer(cols = -dfs, 
               names_to = "variable", #把原来的列名放进新列variable
               values_to = "value")

print(long_env_new, n =30)

wide_env_new <- long_env_new |> 
  pivot_wider(names_from = "variable", values_from = "value")
#将variable 这一列里的内容作为新列名

#总结：gather和pivot_longer是宽表转长表，pivot_longer更加常用，spread和pivot_wider适用于长表转宽表
# C) Data visualization with ggplot2 

env <- doubs$env 

# Plotting scatter plot or a line
library(ggplot2)
ggplot(data = env, 
       aes(x = alt, y = oxy)) # define aes
ggplot(data = env, 
       aes(x = alt, y = oxy)) +
  geom_point() # dot plots

ggplot(data = env, 
       aes(x = alt, y = oxy)) +
  geom_line() # draw a line 


# Assign plot to a variable and then add plot
basic_plot1 <- ggplot(data = env, 
                     aes(x = alt, y = oxy, 
                         color = dfs)) + #点的颜色由距离源头的距离 dfs 决定
  geom_point()

basic_plot1

basic_plot2 <- ggplot(data = env, 
                      aes(x = alt, y = oxy, 
                          color = dfs)) +
  geom_point(color = "blue") #  add colors 最终所有的点会都是蓝色

basic_plot2


ggplot(data = env, 
       aes(x = alt, y = oxy, color = dfs)) +
  geom_line() # add colors


# Usually plots with white background
ggplot(data = env, aes(x = alt, y = oxy, color = dfs)) +
  geom_line() +
  theme_bw() + #黑白主题，背景为白色
  theme(panel.grid = element_blank()) #去掉背景线


# Customization with aes and title
my_plot <- 
  ggplot(doubs$env, aes(x = alt, y = oxy, 
                        color = dfs, size = dfs)) +
  geom_point(alpha = 0.8) +  
  scale_color_gradient(low = "blue", high = "red") +  # 颜色渐变
  labs(title = "alt vs oxygen",
       x = "Altitude",
       y = "Oxygen Level",
       color = "Distance from Source",
       size = "dfs") +
  theme_minimal() +  #使用简洁主题
  theme(plot.title = element_text(hjust = 0.5, # middle，标题居中
                                  face = "bold"))#设置标题样式，bold是标题加粗

ggsave("results/name_of_file.png", #保存路径和文件名
       my_plot, width = 15, height = 10)

################################################
## 02- self-defined functions with for and apply
################################################

# 1) using for loop

y1 <- rnorm(10)
y2 <- rnorm(10) + 10
dat <- data.frame(y1, y2)
dat

result <- list() # define an empty list for saving result
for(i in 1:2){
  result[[i]] <- mean(dat[,i]) # calculating the average
}
result

# 2) using apply()

apply(dat, 2, mean)#作用和for循环相似，dat是要处理的数据，2为按列计算，1为按行计算，mean是计算平均值

################################################
## 03- inserting codes in text with rmarkdown
################################################

