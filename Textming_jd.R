library(tm)
library(Rwordseg)
library("jiebaR")
par(family='STXihei')
library(grDevices)
#setwd("~/Documents/Project/Job-Market Analysis")
data = read.csv("job_test_440.csv",stringsAsFactors=F)

####1.Pick up the jd####
jd_text = data[,8]

jd = c()
for(word in jd_text){
  jd = paste(jd,word)
}



####2.process the text####
#(1)delete all number and br
# gsub("[0-9０１２３４５６７８９]","",jd[2]) 
# gsub("[b,r,p]","",jd[2]) 
cl_word = function(text){
  for(i in 1:length(text)){
    text[i] = gsub("[0-9０１２３４５６７８９]","",text[i]) 
    text[i] = gsub("[b,r,p,n,li,strong]","",text[i])
    text[i] = gsub(pattern = "[[:punct:]]", replacement = "", text[i]) #去除标点符号
    text[i]<-gsub("[0-9a-zA-Z]+?","",text[i])###去除数字和英文
  }
  return (text)
}

jd1 = cl_word(jd)

####3.Cut the words####
cutter = worker()
jd1 = cutter[jd1]
####4.Filter the words####

filter_words  = c("岗位","职责","工作","负责","业务","职位","岗位职责",'分析师',"数据","数据分析")
wordsegment<- function(x) {
  filter_segment(x,filter_words)
}

jd1 <- unlist(lapply(jd1, wordsegment))
####4.delete stopwords####
#(1)Load the stopwords

stopwords<-unlist (read.table("stopwords_CN.txt",stringsAsFactors=F))###读取停止词

#(2)make a function  
removeStopWords = function(x,words) {  
  ret = character(0)
  index <- 1
  it_max <- length(x)
  while (index <= it_max) {
    if (length(words[words==x[index]]) <1) ret <- c(ret,x[index])
    index <- index +1
  }
  ret
}

jd1 <- unlist(lapply(jd1, removeStopWords, stopwords))


####5.delete number and letters####
library(stringr)#加载stringr包
jd1<-str_trim(jd1)#去除空格


####6.构建词频表#####
library(plyr)
tableWord<-count(jd1)##形成词频表,tableWord是数据框格式
tableWord = tableWord[order(tableWord[,2],decreasing=TRUE),]
head(tableWord,10)
####7.PLot wordcloud####
library(wordcloud)
wordcloud(tableWord[,1],tableWord[,2],random.order=F,col= rainbow(length(tableWord[,2])))##参数应该能看懂吧




