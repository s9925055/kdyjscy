
sentiScore = function(raw_comment, shop, brandname, output_path){
  # 找出环境后面的词语
  text = str_extract(raw_comment$Comment, '(?=环境)[^。!～！务,，味]{0,20}')
  text %<>% str_replace_all(., '[:punct:a-zA-Z0-9]', '')
  # 断词
  tager <- worker("tag", dict = '.\\dict\\jieba.dict.utf8', user = '.\\dict\\user.dict.utf8', user_weight = 'max')
  tag_comment <- lapply(text, segment, jiebar = tager)
  # 筛选词性
  tag_comment %<>% lapply(., function(x){
    x[names(x) == 'a'|names(x) == 'x'|names(x) == 'n'|names(x) == 'v'|names(x) == 'd'|names(x) == 'c']
  })
  
  # 情緒分數----
  sentence_score = function(target){
    # target = 输入的句子(被筛选词性了)
    if(length(target) == 0){
      # 如果筛选后没剩下东西，代表没有情绪的词，分数=0
      sentence = ''
      names(sentence) = 0
    }else {
      # 如果有情绪词
      score = 0
      sentence = ''
      # 对每个target打上情绪分数
      # 每个target里面有数个sentence, ex. target = c('A', 'B')
      for(i in 1:length(target)){
        # 找到该词的对应于情绪词库(envir_word)的位置
        # (envir_word是利用筛出来的字，人工进行标注的词库)
        index = which(envir_word$terms == target[i])
        # 如果在envir_word找不到，就跳过
        if(length(index) == 0) next
        # 如果有找到，就记录下词性跟情绪分数
        temp_score = envir_word$sentiment[index]
        temp_tag = envir_word$tags[index]
        # 我只要留e = enviroment, g = general = 一般情绪,两种情绪
        if(temp_tag == 'e' | temp_tag == 'g'){
          sentence = paste0(sentence, envir_word$terms[index])
          # 如果情绪分数!=0, 就乘上情绪分数, 其中正面情绪1~3, 负面情绪-1~-3
          if(temp_score != 0){
            # 为了避免0*1 = 0, 若是有需要乘上情绪分数, 首先要让情绪分数=1, 再做运算
            if(score == 0) score = score + 1
            score = score * temp_score
          }
        }
      }
      names(sentence) = score
    }
    return(sentence)
  }
  # 计算每句的情绪分数
  tag_score <- sapply(tag_comment, sentence_score)
  # 加到comment的frame里面
  raw_comment$score = names(tag_score) %>% as.integer(.)
  raw_comment$tag = tag_score
  # comment没有shopID, 从shop那边合进来
  raw_comment %<>% inner_join(., shop %>% select(., ID, name), by = c('StoreID' = 'ID'))
  # 除了以上的情绪分析做法, 同时考虑点评里的三项指标
  # 所以先找出情绪分数目前为正, 但三项指标有一个低于2的
  index = which(raw_comment$score >= 0 &
                  (raw_comment$Taste < 2 | raw_comment$Envir < 2 | raw_comment$Service < 2))
  for(i in index){
    # 将上述的score>0, 三项指标有一个<2的
    # 分数修正为 最低分-2, ex. taste = 1 -> 1-2 = -1
    raw_comment$score[i] = min(raw_comment$Taste[i]-2,
                                   raw_comment$Envir[i]-2,
                                   raw_comment$Service[i]-2)
  }
  # result = filter(raw_comment, Envir < 2 | score < 0)
  assign(paste0(brandname, '_comment'), raw_comment)
  save(list = paste0(brandname, '_comment'), file = paste0(output_path, brandname, '_comment_sen.RData'))
}
