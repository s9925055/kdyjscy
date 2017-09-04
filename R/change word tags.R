
comment<-word_bind

length(comment)
tag<-names(comment[[1]])
word<-comment[[1]]

x<-cbind(word,tag)

xx<-list()
for(i in 1:length(comment)){
  
  tag<-names(comment[[i]]) 
  word<-comment[[i]]
  x<-cbind(word,tag)
  xx<-rbind(xx,x)
}

yy<-unique(xx)

write.csv(yy,"D:\\work\\dianping\\word_tag1.csv")


sys = load_sys_dict(jiebaR::DICTPATH)
i1<-read.csv("D:\\work\\word\\i.csv",header = T)
word<-as.character(i1$word)

for(i in 1:dim(i1)[1]){
sys[word[i]][[1]][1] = 'a'
}
write_dict(sys, jiebaR::DICTPATH)