#NO1
installed.packages()#查看已经有的包
rownames(installed.packages())#提取包的名称
"tidyverse" %in% rownames(installed.packages())
install.packages('tidyverse')#安装包
library(tidyverse)#加载包
ls("package:tidyverse")#查看tidyverse里面有哪些函数
lsf.str("package:tidyverse")#查看包中所有函数及其简要说明
data(package = "tidyverse")
packageVersion("tidyverse")
packageDescription("tidyverse")
help(package = "tidyverse")#查看包的帮助文档

#NO2
x <- rnorm(n = 10, mean= 35, sd = 10)
check_threshold <- function(vec, threshold = 35){
  result <- logical(length(vec))
  for(i in 1:length(vec)){
    result[i] <- vec[i] > threshold
  }
  return(result)
}
print(x)
check_threshold(x)
