
brandname = c('iwill', 'holiland', 'efdc', 'gb', 'cfxb')
filepath = 'C:\\work\\crawler\\output\\iwill\\'
output_path = '.\\output\\'
i=1

load('C:\\work\\liutongbu\\iwill cake\\data\\iwill_product.RData')
load(paste0(filepath, brandname[i], '_comment_sen.RData'))
comment = get(paste0(brandname[i], '_comment'))

# data----
# bytime
all_inf = group_by(comment, yearmonth) %>% summarise(., count = n(),
                                                             Taste = mean(Taste),
                                                             Envir = mean(Envir),
                                                             Service = mean(Service),
                                                             senti = sum(score<0) / (count+50) * 100)
save(all_inf, file = paste0(output_path, brandname[i], '_all_inf.RData'))
# byshop
shop_inf = group_by(comment, name) %>% summarise(., count = n(),
                                                         taste = mean(Taste),
                                                         envir = mean(Envir),
                                                         service = mean(Service),
                                                         senti = sum(score<0) / (count+50) * 100)
colnames(shop_inf)[1] = 'shop'
save(shop_inf, file = paste0(output_path, brandname[i], '_shop_inf.RData'))
# byproduct
# product = kdyjscy_product$name %>% str_replace_all(., '（.*）|\\(.*\\)', '') %>% unique(.) %>% c(., '酸菜鱼')
pLEN = length(product)
product_inf = NULL
for(j in 1:pLEN){
  temp_data = comment %>% .[str_detect(.$Comment, product[j])|str_detect(.$Foods, product[j]), ]
  temp_result = c(count = nrow(temp_data),
                  taste = mean(temp_data$Taste),
                  envir = mean(temp_data$Envir),
                  service = mean(temp_data$Service),
                  senti = sum(temp_data$score<0) / (nrow(temp_data)+50) * 100 # 每100则提到负面评论数
                  # senti = sum(temp_data$score < 0) # 负面次数
  )
  product_inf = rbind(product_inf, temp_result)
}
product_inf %<>% as.data.frame(.)
product_inf$product = product
save(product_inf, file = paste0(output_path, brandname[i], '_product_inf.RData'))
# ----
i=i+1


# 总体----
# 品牌趋势----
# _三项指标的趋势图
all_inf1 = select(all_inf, yearmonth, Taste:Service) %>% melt(., id = 'yearmonth')
a = ggplot(all_inf1 , aes(x=yearmonth, y=value, group=variable, colour=variable)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))
ggplotly(a, width = 1200, height = 600)
# _情绪分数的趋势图
ggplot(all_inf , aes(x=yearmonth, y=senti, group=1, colour=1)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))
# 排序----
# 产品----
# 提到次数最多的前20项产品
ggplot(product_inf %>% arrange(., desc(count)) %>% .[2:21, ],
       aes(x = reorder(product, desc(count)), y = count)) + 
  geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90))
# 味道分数最低的20项产品
ggplot(product_inf %>% arrange(., taste) %>% .[1:20, ],
       aes(x = reorder(product, taste), y = taste, fill = count)) + 
  geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90))
# 负面情绪最多的前20项产品
ggplot(product_inf %>% arrange(., desc(senti)) %>% .[1:20, ],
       aes(x = reorder(product, desc(senti)), y = senti)) + 
  geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90))
# 门店----
# 提到次数最多的20间门店
ggplot(shop_inf %>% arrange(., desc(count)) %>% .[1:20, ],
       aes(x = reorder(shop, desc(count)), y = count)) + 
  geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90))
# 服务分数最低的20间门店
temp = shop_inf %>% arrange(., service) %>% .[1:20, ]
ggplot(temp, aes(x = reorder(shop, service), y = service)) + geom_bar(stat = 'identity')
# 负面情绪最多的20间门店
ggplot(shop_inf %>% arrange(., desc(senti)) %>% .[1:20, ],
       aes(x = reorder(shop, desc(senti)), y = senti)) + 
  geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90))
# ----

# 个体----
# 单一门店----
target = '月亮湾店'
trend_shop = comment %>% filter(., name == target)
# 趋势
trend_shop %<>% group_by(., yearmonth) %>% summarise(., count = n(),
                                                     taste = mean(Taste),
                                                     envir = mean(Envir),
                                                     service = mean(Service),
                                                     senti = sum(score<0) / (count+50) * 100)
# _三项指标
trend_shop2 = melt(trend_shop[,c(1,3,4,5)], id = 'yearmonth')
ggplot(trend_shop2 , aes(x=yearmonth, y=value, group=variable, color = variable)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))
# _负面情绪多寡
ggplot(trend_shop , aes(x=yearmonth, y=senti, group=1)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))

# 单一产品----
target = '酸汤鱼'
trend_product = comment %>% .[str_detect(.$Comment, target), ]
# 趋势
trend_product %<>% group_by(., yearmonth) %>% summarise(., count = n(),
                                                        taste = mean(Taste),
                                                        envir = mean(Envir),
                                                        service = mean(Service),
                                                        senti = sum(score<0) / (count+50) * 100)
# _三项指标
trend_product2 = melt(trend_product[,c(1,3,4,5)], id = 'yearmonth')
ggplot(trend_product2 , aes(x=yearmonth, y=value, group=variable, color = variable)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))
# _负面情绪多寡
ggplot(trend_product , aes(x=yearmonth, y=senti, group=1)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))
# ----

# 竞争对手热度----
test = str_extract_all(comment$Comment, '[跟与和][^，。、,.~!！]*比')
test = str_extract_all(comment$Comment, '比[^，。、,.~!！ 较]*难吃')
test %<>% .[lengths(.) > 0]
# ----

save.image(file = '.\\data\\workspace.RData')
