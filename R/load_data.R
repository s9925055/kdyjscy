library(jiebaR)
library(stringr)
library(magrittr)
library(dplyr)
library(text2vec)
library(ggplot2)
library(plotly)
library(data.table)

brandname = c('iwill', 'holiland', 'efdc', 'gb', 'cfxb')
filepath = 'C:\\work\\crawler\\output\\iwill\\'
output_path = 'C:\\work\\crawler\\output\\iwill\\'
i=1

# 开始处理 ----
# 读取comment
load(paste0(filepath, brandname[i], '_comment.RData'))
comment = get(paste0(brandname[i], '_comment'))
comment %<>% select(., -c(12:14))
comment %<>% filter(., year >= 2015)
comment %<>% .[!is.na(.$Taste), ]
comment$yearmonth = str_sub(comment$Date, 1, 5) %>% paste0('20', .)

# 读取shop
load(paste0(filepath, brandname[i], '_shop.RData'))
shop = get(paste0(brandname[i], '_shop'))
shop$name %<>% str_extract_all(., '\\(.*\\)') %>% str_replace_all(., '\\(|\\)', '')
shop = shop[shop$name != 'character0', ]

# 读取情绪词库
envir_word = fread('.\\data\\envir_word.csv', encoding = 'UTF-8')

# 读取产品库
product = read.csv('.\\data\\product.csv', encoding = 'GBK', stringsAsFactors = F)
product = product[,-4]
colnames(product) = c("ID", 'name', 'price')

# 将原始的comment加上情绪分数
source('.\\R\\sentiScore.R', encoding = 'UTF-8')
sentiScore(comment, shop, brandname[i], output_path)
# ----
i=i+1

# load('C:\\work\\crawler\\output\\iwill\\iwill_comment_sen.RData')
