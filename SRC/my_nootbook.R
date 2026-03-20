#getwd()#获取当前的工作目录
#setwd()#设置当前的工作目录
#格式为set("路径，比如D:/R yuyan/shujukexue")

#list.files()和dir()都是查看某个文件夹有哪些文件和子文件夹

#R最初是作为一个计算器使用，于是乎：
#+\-\*\/;^2幂;sqrt(2);5%/%2整数;5%%2余数;<-和=向左赋值;->和->>向右赋值

x <- 5#x赋值5
#<小于，>,<=,>=,==等于，!=不等于
#!不或否；&逻辑与和；&&逻辑与和，仅向量第一元素；|逻辑or;||逻辑或，仅向量第一元素
!TRUE#!逻辑not
TRUE & FALSE#逻辑and
TRUE | FALSE#逻辑or

#Double
x <- 5.23#赋值但不打印
(x <- 5.23)#赋值并且打印
is.numeric(x)#选择数据类型，这个是数值型数据
is.integer(x)#整数数据
is.double(x)

class(x)#选择数据类型
typeof(x)
mode(x)#相比typeof更旧，偏向于S语言
#is.numeric();is.double()本质是一样的

#Logical
x <- c(TRUE,TRUE,FALSE)

#Character
x <- "elevated"
x <- "3.14"
is.character(x)
is.numeric(x)

#date object
#https://www.geeksforgeeks.org/r-objects/

#Vectors
c(1,4,7)
Num_variable <- c(1,4,7)#赋值变量
print(Num_variable)
(Num_variable <- c(1,4,7))#赋值并且打印
#Basic information
length(Num_variable)# How many elements
typeof(Num_variable)# of which type
is.vector(Num_variable)# Date structure
is.list(Num_variable)
names(Num_variable)
str(Num_variable)

a <- 100
is.vector(a)
length(a)

#Accessing elements
Num_variable[2]#索引为2的元素
Num_variable[-2]#除了索引为2的元素
which(Num_variable == "4")#know elements for location

#Indexing by subset()
(v <- c(1:2, NA, 4:6, NA,8:10))#NA表示缺失值，若不加最外层括号R 会赋值，但有时不会把结果明显打印出来
v[v>5]#missing/NA values are preserved
subset(v, v > 5)#missing/NA values are lost

#Rectangular objects
(m0 <- matrix(data = 1:9, nrow = 3, byrow = TRUE))#byrow意思是按行填充，()同样代表一边赋值一边打印
x <- 1:3#creating 3 vectors
y <- 4:6
z <- 7:9
(m1 <- rbind(x, y, z))#rbind=row bind就是说要按行合并

#matrix attribution
 mode(m0)
typeof(m0)
length(m0)

is.vector(m0)
is.matrix(m0)
dim(m0)#m0的维度

#indexing matrixs

m0[2,3]#rows or columns of matrices第2行第3列
m0 > 5
sum(m0)
max(m0)
mean(m0)#平均值
colSums(m0)#按列求和
rowSums(m0)#按行求和
t(m0)#把m0做置换

#data frames

name <- c("Adam", "Bertha", "CSERG", "Disheu", "Eve")
gender <- c("male", "female", "female", "male", "male")
age <- c(21, 23, 22, 19, 21)
height <- c(165, 175, 168, 172, 158)

df <- data.frame(name, gender, age, height, stringsAsFactors = TRUE)
#把 name、gender、age、height 这几个对象组合成一个数据框（data frame），并保存到 df 里
#stringsAsFactors = TRUE意思是把字符串类型的数据自动转换成 factor（因子）
df

is.matrix(df)#判断是不是矩阵
is.data.frame(df)#判断是不是数据框
dim(df)#输出行数和列数

rm(list = ls())#表示清除所有的变量
var1 = "Hello Geeks"
print(var1)

#变量名称命名时，大小写是不同的，也可以用xxx_xxx命名
#比如tea_pot instead of tea pot
#避免使用在R中有特定含义的符号：#注释；$列表取值，索引；TRUE；function

var$1 <- 5#$在R中有特殊语法，not permitted
var#1 <- 10 #也是如此
2var_name <- 111 #不能以数字开头
.var_name <- 11 #可以以.开头，但点后不能有数字

l_1 <- list(1,2,3)
l_1
l_2 <- list(1,c(2,3))
l_2
l_3 <- list(1,"B", 3)
l_3

x <- list(1:3)
x[1][3]#第一个位置第三个元素

x <- list(c(1,2,3),2)
x[1][3]
x

df4 <- data.frame(x = 1:5, y = 5:1)#x,y代表列名，行没有名称
df4

df5 <- data.frame(x = 11:15, y = 15:11)
df5

?unlist#不知道一个函数什么意思的时候可以使用？函数，在help中会显示
#unlist(x)把x中的所有成分，呈现的是一个向量
array1 <- array(data = c(unlist(df4), unlist(df5)), 
                       dim = c(5, 2, 2), 
                       dimnames = list(rownames(df4), 
                                       colnames(df4)))
array1
#把里面的元素分成两层，五行两列的三维数组
#data是数据源
#dim的意思是分成5×2×2行，列，层的三维数组
#rownames(df4)第一维度行名是df4的行名
#colnames(df4)第二维度名称：df4的列名（长度需=2）

# Numeric Function
abs(-12)


#installing and loading packages

# from CRAN
install.packages('readr')
install.packages(c('readr', 'ggplot2', 'tidyr'))

installed.packages()

add_three <-function(x){
  y <- x+3
  return(y)
}
add_three(5)
source("add_three.R")#保存在工作目录之后有，表示调用这个函数
source("C:/Users/lx/Documents/tutorial_2026/SRC/add_three.R")#详细路径太长
source("SRC/add_three.R")#用相对路径访问
add_three(7)
getwd()

#Tools-Global options-Code-snippets可以建立快捷的输入
#snippets+名称可以用于快捷

