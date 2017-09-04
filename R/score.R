source('.\\R\\load_data.R', encoding = 'UTF-8')

kd = kdyjscy_comment[, c(2,5,7,8,9,10)] %>% na.omit(.)
kd$Date %<>% strptime(., '%y-%m-%d') %>% as.Date()
kd %<>% filter(., Date >= '2016-01-01')
kd$yearmonth = str_sub(kd$Date, 1, 7)
kd_shop = group_by(kd, StoreID) %>% summarise(., Taste = mean(Taste),
                                               Envir = mean(Envir),
                                               Service = mean(Service))

# 时间分布, 总体分数----
kd_all = group_by(kd, yearmonth) %>% summarise(., Taste = mean(Taste),
                                               Envir = mean(Envir),
                                               Service = mean(Service))
kd_all %<>% melt(., id = 'yearmonth')
a = ggplot(kd_all , aes(x=yearmonth, y=value, group=variable, colour=variable)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))
ggplotly(a, width = 1200, height = 600)
#----

temp2016 = group_by(kd, StoreID, yearmonth) %>% summarise(., count = n(), 
                                                          Taste = mean(Taste),
                                                          Envir = mean(Envir),
                                                          Service = mean(Service))
temp2016  %<>% inner_join(., kdyjscy_shop %>% select(., ID, name), by = c('StoreID'='ID'))

# 时间分布(月), by shop----
kdf = group_by(temp2016, name, yearmonth) %>% summarise(., count = n(),
                                                    Taste = mean(Taste),
                                                    Envir = mean(Envir),
                                                    Service = mean(Service))
# 口味 
# 选出最低的10个
taste10_temp = kd_shop %>% arrange(., Taste) %>% .[1:10, 1] %>% inner_join(., kdyjscy_shop %>% select(., ID, name), by = c('StoreID'='ID'))
taste10 = unlist(taste10_temp$name) %>% paste0(., collapse = '|')
t = ggplot(kdf %>% .[str_detect(.$name, taste10), ], aes(x=yearmonth, y=Taste, group=name, colour=name)) + geom_line() + theme(axis.text.x = element_text(angle = 45))
ggplotly(t, width = 1200, height = 600)

# 环境
# 选出最低的10个
envir10_temp = kd_shop %>% arrange(., Envir) %>% .[1:10, 1] %>% inner_join(., kdyjscy_shop %>% select(., ID, name), by = c('StoreID'='ID'))
envir10 = unlist(envir10_temp$name) %>% paste0(., collapse = '|')
e = ggplot(kdf %>% .[str_detect(.$name, envir10), ], aes(x=yearmonth, y=Envir, group=name, colour=name)) + geom_line() + theme(axis.text.x = element_text(angle = 45))
ggplotly(e, width = 1200, height = 600)

# 服务
# 选出最低的10个
service10_temp = kd_shop %>% arrange(., Service) %>% .[1:10, 1] %>% inner_join(., kdyjscy_shop %>% select(., ID, name), by = c('StoreID'='ID'))
service10 = unlist(service10_temp$name) %>% paste0(., collapse = '|')
s = ggplot(kdf %>% .[str_detect(.$name, service10), ], aes(x=yearmonth, y=Service, group=name, colour=name)) + geom_line() + theme(axis.text.x = element_text(angle = 45))
ggplotly(s, width = 1200, height = 600)
# ----

# 排序----
sort_shop = group_by(temp2016, name) %>% summarise(., )

# ----

# 时间分布, by product
product = kdyjscy_product$name %>% str_replace_all(., '（.*）|\\(.*\\)', '') %>% unique(.) %>% c(., '酸菜鱼')
pLEN = length(product)
count = NULL
for(i in 1:pLEN){
  temp = kdyjscy_comment %>% .[str_detect(.$Comment, product[i]), ]
  taste
  ncount = temp %>% sum(.)
  count = c(count, temp)
}
heat_product = data.frame(product = product, count = count) %>% arrange(., desc(count))
heat_product[2:11, ]
target = '酸菜鱼'
temp_product = kd[str_detect(kd$Comment, target), ]

product_all = group_by(temp_product, yearmonth) %>% summarise(., Taste = mean(Taste),
                                               Envir = mean(Envir),
                                               Service = mean(Service))
product_all %<>% melt(., id = 'yearmonth')
a = ggplot(product_all , aes(x=yearmonth, y=value, group=variable, colour=variable)) +
  geom_line() + theme(axis.text.x = element_text(angle = 45))
ggplotly(a, width = 1200, height = 600)
