library(jiebaR)
library(cidian)

sys = load_sys_dict('.\\dict\\jieba.dict.utf8')
user = load_user_dict('.\\dict\\user.dict.utf8')

# 手工加注的人工标签(包含EST与情绪分数)
i1 <- read.csv("D:\\work\\liutongbu\\kdyjscy\\dict\\word_tag2.csv",header = T, stringsAsFactors = F)
# 有家酸菜鱼的产品
kdyjscy_product = read.csv('.\\data\\product.csv', encoding = 'GBK', stringsAsFactors = F)
kdyjscy_product = kdyjscy_product[,-4]
colnames(kdyjscy_product) = c("ID", 'name', 'price')
product = kdyjscy_product$name %>% str_replace_all(., '（.*）|\\(.*\\)', '') %>% unique(.) %>% c(., '酸菜鱼')
tag = rep('kdy', length(product))
# 有家酸菜鱼的分店
shop = str_extract_all(kdyjscy_shop$name, '\\(.*\\)') %>% str_replace_all(., '\\(|\\)', '') %>% unique(.) %>% .[str_count(.) < 10]
tag = rep('kdy_shop', length(shop))

# 如果sys里面已经有了，修改sys的词性
# 如果sys里面没有，增加到user里面
word = shop
new_tag = tag
for(i in 1:length(word)){
  if(length(sys[word[i]][[1]][1]) < 1){
    user = add_user_words(user, enc2utf8(word[i]), new_tag[i])
  }else{
    sys[word[i]][[1]][1] = new_tag[i]
  }
}

write_dict(sys, output = '.\\dict\\jieba.dict.utf8')
write_dict(user, output = '.\\dict\\user.dict.utf8')

# 增加单一词到user
user = add_user_words(user, enc2utf8('有家酸菜鱼'), 'kdy')

# 查看指定字在sys当中的词性
sys['一般般'][[1]][1]
segment('偶尔', tager)
# 单项更改
sys['一般般'][[1]][1] = 'a'




