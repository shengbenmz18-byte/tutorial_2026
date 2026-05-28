# --------------------------------------------
# Script Name: Data retriever and databases
# Purpose: My class mainly focuses on exploring the Doubs River 
#          dataset. This script is to show how to get data from 
#          free public databases and save a database of SQlite 
#          or postgresql.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-01
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

#####################################################
# 01- Get data from an URL or a repository
#从 URL 或数据库仓库获取数据
#####################################################

# A) using R packages to visit databases for data
# rgbif
# 使用 R 包访问数据库：GBIF 数据，是全球生物多样性数据库
library(rgbif)
name_backbone(name = "Lepus saxatilis") # obtaining a species key

key <- 2436775

dat <- occ_search( # searching and downloading data 从 GBIF 下载物种出现记录
  taxonKey = key,
  # country = "JP",        
  # year = "2000,2020",    
  hasCoordinate = TRUE, #只下载有经纬度坐标的数据
  limit = 2000 #最多2000条记录
)

head(dat$data) # viewing the data
nrow(dat$data)
ncol(dat$data)
colnames(dat$data)

library(ggplot2)
library(maps)

ggplot() +
  borders("world", colour = "gray70", fill = "gray90") +  # world map,colour是边界线颜色，fill是国家区域填充颜色
  geom_point(data = dat$data,
             aes(x = decimalLongitude, y = decimalLatitude), #x为经度，y为纬度
             color = "red", size = 1) +
  theme_minimal()

write.csv(dat$data, "data/lepus.csv", row.names = FALSE)
lepus_data <- read.csv("data/lepus.csv")

# using rdataretriever to download data from databases

# # Install rdataretriever in python environment 
# # https://github.com/ropensci/rdataretriever
# # https://rstudio.github.io/reticulate/
# 
install.packages('reticulate') # interface to Python
library(reticulate) # run in virtual env!!! reticulate 是 R 和 Python 之间的接口
reticulate::install_miniconda()
reticulate::conda_list()
use_condaenv("myenv", required = TRUE)
reticulate::reply_python()
py_config() # 查看当前 R 连接到的是哪个 Python 环境
pip install retriever
reticulate::py_run_string("import sys; print(sys.executable)")
reticulate::py_install("retriever", pip = TRUE)
py_install("retriever", pip = TRUE)
# Install retriever package
install.packages('rdataretriever') 

library(rdataretriever)
check_retriever_availability()
get_updates() # Update the available datasets
datasets() # List the datasets available via the Retriever
install_csv('portal') # Install csv portal, i.e. 219 dataset
# portal 是一个经典生态学数据集，常用于群落生态、种群动态等分析
download('portal', 'data/portal') # Download the portal dataset
portal = fetch('portal') # Install and load a dataset下载并直接读入 R
names(portal)
head(portal$plot)

plot <- read.csv("data/portal/3299474")
species <- read.csv("data/portal/3299483")
main <- read.csv("data/portal/5603981")

library(tidyverse)
glimpse(main) #快速查看 main 数据表结构
# 注释部分Harvard Forest 数据下载示例
# download('harvard-forest', 'data') # vector [162]
# unzip("data/hf110-01-gis.zip")
# 
# library(sf)
# sf <- st_read(unzip("data/hf110-01-gis.zip", #read .shp into R
#                     "Harvard_Forest_Properties_GIS_Layers/stands_1937.shp"))

# B) get data from web APIs

library(httr2)
response <- request("https://api.gbif.org/v1/occurrence/search") %>%
  req_url_query(
    scientificName = "Lepus saxatilis",
    hasCoordinate = TRUE,
    limit = 100
  ) %>%
  req_perform()
# 向 GBIF API 发送请求

library(jsonlite) # jsonlite 用来把 JSON 转换为 R 里的数据框或列表

lepus_data <- fromJSON(
  resp_body_string(response), # 从网络响应中提取正文内容
  flatten = TRUE
) #解析 API 返回的数据

df <- lepus_data$results
df_key <- df[, c(
  "decimalLongitude",
  "decimalLatitude",
  "eventDate",
  "country",
  "basisOfRecord" #记录来源
)]

str(df_key)

# # C) download.file() from websites
# # https://www.davidzeleny.net/anadat-r/doku.php/en:data:doubs
# 
# # Set the base URL for the datasets 设置基础网址
# 
# base_url <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/"
# 
# datasets <- c("DoubsSpe.csv","DoubsEnv.csv","DoubsSpa.csv")  # List of datasets 
# 
# # Download each dataset
# 
# for(dataset in datasets) {
#   full_url <- paste0(base_url, dataset) # full URL of files
#   dest_file <- file.path("data/", dataset) # the destination
#   download.file(full_url, destfile = dest_file, mode = "wb") # Download
#   cat("Downloaded:", dataset, "\n") # Print a message for complete
# }依次下载三个csv
# 
# # if getting an error, check DNS (sudo vim /etc/resolv.conf)

#####################################################
# 02- loading and saving data from local computer从本地电脑读取和保存数据
#####################################################
data(doubs, package = 'ade4')
Env <- doubs$env
Spe <- doubs$fish

write.csv(Env, "data/Env.csv", row.names = FALSE)
write.csv(Spe, "data/Spe.csv", row.names = FALSE)
Env_csv <- read.csv("data/Env.csv") #重新读取

saveRDS(Env, "data/Env.rds")
saveRDS(Spe, "data/Spe.rds") #把 Env 和 Spe 保存为 RDS 文件，RDS文件是 R 自己的单对象保存格式
Env_rds <- readRDS("data/Env.rds")
Spe_rds <- readRDS("data/Spe.rds")
#CSV适合和别人共享，R更加稳定适合自己保存
#####################################################
# 03-Working on the SQLite with R
#####################################################
# 1) Installing SQLite and DB Browser 用DB Browser 手动导入和导出 CSV
# to check if SQLite is installed, installing on Ubuntu
# by reference to https://www.jianshu.com/p/54261f6105a0

# sudo apt-get install sqlite3
# sudo apt-get install libsqlite3-dev 
# sudo apt-get install sqlitebrowser

# A) working on data with a sqlite db by DB brower

# importing Env and Spe into a sqlite
# a. open DB browser
# b. creating a sqlite such as doubs.sqlite
# c. File → Import → Table from CSV

# exporting Env and Spe from the sqlite
# a. open DB brower
# b. File → Export → Table(s) as CSV
# c. specifying the folder and file name

# B) working on data with a sqlite db
# https://caltechlibrary.github.io/data-carpentry-R-ecology-lesson/05-r-and-databases.html

file.remove("data/DOUBS.sqlite") #删除旧的 SQLite 数据库文件

# a. connecting or creating db with RSQlite

library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), 
                 "data/DOUBS.sqlite") # connecting or creating,如果data/DOUBS已经存在就连接，不存在就创建新的数据库文件
dbListTables(con) #查看有哪些表
DBI::dbDisconnect(con) #断开连接

# b. connecting to RStudio using rstdudio pane

# https://www.youtube.com/watch?v=id0GX4sXnyI
# https://www.youtube.com/watch?v=0euy9b3CjuY
# https://staff.washington.edu/phurvitz/r_sql/
# https://solutions.posit.co/connections/db/best-practices/drivers/

# // setup sqlite for rstudio's connections
# apt install unixodbc 
# apt install sqliteodbd

# // two files used to set up the DSN information
# vim /etc/odbcinst.ini
# vim /etc/odbc.ini

# c. manipulating data with a sqlite db using R code 使用 dplyr 操作 SQLite 数据

dbListTables(con) 
dbWriteTable(con, "Env_sqlite", Env, overwrite = TRUE) # importing,把 R 中的 Env 数据表写入 SQLite 数据库
# 表名为Env_splite, overwrite=TRUE表示如果表已经存在就覆盖
Env_sqlite <- dbReadTable(con, "Env_sqlite") # for a small dataset ，把 SQLite 中的 Env_sqlite 表重新读取回 R
library(tidyverse) # working with dplyr for big data
Env_tbl <- tbl(con, "Env_sqlite") # 创建一个远程表
Env_tbl

Env_tbl_sel <- Env_tbl %>% 
  filter(dfs > 200) %>%
  select(dfs, alt) %>%
  head()

Env_local <- collect(Env_tbl_sel) # loading data into R,把数据库中的查询结果真正下载到 R 本地内存中
Env_local

DBI::dbDisconnect(con)
con

#################################################
# 04-Working on PostgreSQL with R
#################################################
# ## A) Installing PostgreSQL and pgAdmin4 
# 
# 数据库	特点
# SQLite	一个文件就是数据库，轻量，本地使用方便
# PostgreSQL	需要数据库服务器，功能强大，适合多人协作、大型项目
# # Install and configure postgresql by following
# # https://www.youtube.com/watch?v=OxIQ_xJ-yzI
# 
# # For ubuntu 22.04, install a default postgresql version by following the site 
# # https://www.rosehosting.com/blog/how-to-install-postgresql-on-ubuntu-22-04/
# # 
# # Verify the installation
# # $ dpkg --status postgresql
# # $ whereis postgresql
# # $ which psql # psql is an interactive PostgreSQL client
# # $ ll /usr/bin/psql
# # $ psql -V # check postgresql version
# 
# # Configure the postgresql
# # Including client authentication methods,connecting to 
# # PostgreSQL server, authenticating with users, etc. see
# # https://ubuntu.com/server/docs/databases-postgresql
# 
# # Create a database and enable PostGIS extension
# # https://staff.washington.edu/phurvitz/r_sql/
# 

# B) creating a database and some schemas with psql 

# // create the user for the database
# create user doubs with encrypted password 'doubs';

# // default high level privileges for this user
# alter default privileges grant all on schemas to doubs;
# alter default privileges grant all on tables to doubs;
# alter default privileges grant all on sequences to doubs;
# alter default privileges grant all on functions to doubs;

# // create a few schemas
# create schema DOUBS; --for doubsEnv, doubsSpe, doubsSpa
# create schema postgis;

# // extension
# create extension postgis with schema postgis;

# C) connecting to a database of PostgreSQL via DBI
library(DBI)
library(RPostgreSQL)
conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connect
                          dbname = 'myclasses',
                          host = 'localhost',
                          port = 5432,
                          user = 'postgres',
                          password = 'root')

conn

# importing doubs data into postgresql
data(doubs, package = 'ade4')
Env <- doubs$env
Spe <- doubs$fish
Spa <- doubs$xy
# 把 Env 写入 PostgreSQL，表名为 doubs_env
dbWriteTable(conn = conn, #前面创建的是conn
             name = "doubs_env", 
             value = Env,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(conn = conn,
             name = "doubs_spe",
             value = Spe,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(conn = conn,
             name = "doubs_spa",
             value = Spa,
             row.names = FALSE,
             overwrite = TRUE)

dbListTables(conn)  #查看当前 PostgreSQL 数据库中有哪些表
dbListFields(conn, "doubs_Spa") # List fields of the table

dbDisconnect(conn) # 断开 PostgreSQL 数据库连接
dbGetInfo(conn) #查看连接信息，在断开前查看

# Connect to PostgreSQL via Rstudio connection pane
# https://www.youtube.com/watch?v=0euy9b3CjuY&t=551s

# //open ubuntu terminal to edit /etc/odbc.ini like this
# [doubs]
# Driver = CData ODBC Driver for PostgreSQL
# Description = My Description
# User = doubs
# Password = xxxx
# Database = doubs
# Server = 127.0.0.1
# Port = 5432

# # saving doubs data into postgresql
# ?dbWriteTable # from RPostgreSQL package
# dbWriteTable(con, "doubs_env", Env, overwrite = TRUE)
# dbWriteTable(con, "doubs_spe", Spe, overwrite = TRUE)

