library(wordcloud2)

brandname = c('iwill', 'holiland', 'efdc', 'gb', 'cfxb')
filepath = 'C:\\work\\crawler\\output\\iwill\\'
output_path = '.\\output\\'
i=1

filter_word = c('酸汤', '没有', '有点', '感觉', '两个', '比较', '这家', '下次', '一直', '非常', '鱼片', '鱼肉', '每次', '酸菜', '里面', '一条', '真的', '觉得', '一个', '有家', '生意', '以后', '以前', '时间', '一家', '上菜', '个人', '总体', '知道', '吃', '鱼', '点', '店', '没', '会', '说', '菜', '汤', '斤', '完', '吃鱼', '吃酸菜鱼', '想', '送', '多次', '吃完','两个吃', '量', '一点', '太', '吃吃', '已经', '一起', '一次', '超级', '态度', '地方', '做', '三个', '最后', '微', '家', '小时', '这家店', '蛮', '现在', '今天', '可能', '少', '位置', '之前', '久', '吃味道', '喝', '不用', '不会', '酸菜鱼味道', '店里', '希望', '好多', '带', '需要', '起来', '一下', '不能', '不够', '爱', '没吃', '选', '东西', '左右', '一份', '鱼味道', '速度', '基本', '真心', '找', '适合', '不好', '人点', '吃饭', '过来', '里', '后来', '挑', '点酸汤', '建议', '更', '吃几次', '一条鱼', '应该', '选择', '菜品', '上来', '涮', '锅', '鱼吃', '一楼', '分钟', '加', '他家', '不要', '开', '够', '排', '斤鱼', '味', '继续', '煮', '好像', '一锅', '四个', '不到', '看到', '高', '这家酸菜鱼', '确实', '那家', '中', '只能', '消费', '本来', '大众', '没什么', '号', '发现', '负', '一家店', '几个', '吃酸汤', '其实', '吃一次', '斤黑鱼', '进去', '涮菜', '广场', '先', '半个', '一瓶', '还点', '换', '斤两', '大概', '负一楼', '估计', '过去', '一定', '不吃', '人', '元', '两个人点', '居然', '条', '一条斤', '半个小时', '分量', '机会', '坐', '刚', '走', '三斤鱼', '最好', '石路', '店家', '真是', '是不是', '问', '吃一条', '点菜', '上桌', '两次', '挺', '吃', '点', '卡', '位于', '人民商场负', '星座', '圆融', '地下', '国际', '大洋', '逛', '品种', '印象', '本来想', '鱼酸汤', '商场', '好找', '小哥', '空间', '约', '弯', '办', '楼下', '边上', '拌', '会员', '有家酸菜鱼吃', '吃有点', '好吃好吃', '感觉没有', '好好', '难得', '好吃吃', '不错价格', '特意', '决定')
mixer <- worker("mix", dict = '.\\dict\\jieba.dict.utf8', user = '.\\dict\\user.dict.utf8',
                user_weight = 'max', stop_word = '.\\dict\\stop_words.utf8')
word_cloud_fun = function(text, min_num){
  seg_comment <- lapply(text, segment, jiebar = mixer)
  seg_comment %<>% filter_segment(., '团购|点评|.{1}')
  
  t.token <- itoken(seg_comment)
  t.vocab <- create_vocabulary(t.token, ngram = c(1,2))
  pruned_vocab = prune_vocabulary(t.vocab, term_count_min = min_num)
  Encoding(pruned_vocab$term) = 'UTF-8'
  word_cloud <- as.data.frame(pruned_vocab)
  word_cloud %<>% subset(., .$doc_count > 1)
  word_cloud = word_cloud[,-3]
  word_cloud$term %<>% str_replace_all(., '_', '')
  word_cloud %<>% group_by(., term) %>% summarise(., term_count = max(term_count))
  for(word in filter_word){
    index = which(word_cloud$term == word)
    if(length(index) == 0) next
    word_cloud %<>% .[-c(index), ]
  }
  word_cloud$rank = rank(word_cloud$term_count)
  word_cloud$rank = max(word_cloud$rank) - word_cloud$rank + 1
  word_cloud %<>% arrange(., desc(term_count))
  return(word_cloud)
}
# start ----
load(paste0(filepath, brandname[i], '_comment_sen.RData'))
comment = get(paste0(brandname[i], '_comment'))


# 总体词云----
wc_all = word_cloud_fun(comment$Comment, 2)
save(wc_all, file = paste0(output_path, brandname[i], '_wc_all.RData'))
# ----

# 各年词云变动
comment_2015 = subset(comment, year == 2015)
wc_2015 = word_cloud_fun(comment_2015$Comment, 7)
comment_2016 = subset(comment, year == 2016)
wc_2016 = word_cloud_fun(comment_2016$Comment, 7)
comment_2017 = subset(comment, year == 2017)
wc_2017 = word_cloud_fun(comment_2017$Comment, 5)

merge1 = left_join(wc_2017[1:700, ], wc_2016, by = c("term" = 'term'))
merge2 = left_join(merge1,wc_2015, by = "term",all.x = T)
merge2 = select(merge2, term, contains("rank"))
merge2 %<>% na.omit(.)
# merge2 = merge2[-c(1:3), ]

merge2$rank.x[is.na(merge2$rank.x)] = max(wc_2017$rank)+1
merge2$rank.y[is.na(merge2$rank.y)] = max(wc_2016$rank)+1
merge2$rank[is.na(merge2$rank)] = max(wc_2015$rank)+1

merge2$rank.x <- 1-(merge2$rank.x/(max(wc_2017$rank)+1))
merge2$rank.y <- 1-(merge2$rank.y/(max(wc_2016$rank)+1))
merge2$rank <- 1-(merge2$rank/(max(wc_2015$rank)+1))
colnames(merge2) = c("terms","rank_2017","rank_2016","rank_2015")

all_rank = cbind(merge2, merge2$rank_2016-merge2$rank_2015, merge2$rank_2017-merge2$rank_2016)
colnames(all_rank) = c("terms","rank_2017","rank_2016","rank_2015","diff16_15","diff17_16")
all_rank$absDiff = abs(all_rank$diff16_15) + abs(all_rank$diff17_16)
all_rank$trueDiff = all_rank$diff16_15 + all_rank$diff17_16
save(all_rank, file = paste0(output_path, brandname[i], '_all_rank.RData'))
# ----
i = i+1
# # 变动幅度排序(无论正负)
# all_rank1 = arrange(all_rank, desc(absDiff))[1:20, ]
# ggplot(all_rank1, aes(x=reorder(terms, desc(absDiff)), y=absDiff)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(face = "bold",angle = 90))
# # 上升最多
# all_rank1 = arrange(all_rank, desc(trueDiff))[1:20, ]
# ggplot(all_rank1, aes(x=reorder(terms, desc(trueDiff)), y=trueDiff)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(face = "bold",angle = 90))
# # 下降最多
# all_rank1 = arrange(all_rank, trueDiff)[1:20, ]
# ggplot(all_rank1, aes(x=reorder(terms, desc(abs(trueDiff))), y=abs(trueDiff))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(face = "bold",angle = 90))