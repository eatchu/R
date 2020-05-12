library(httr)
library(XML)

url <- "https://news.naver.com/main/main.nhn?mode=LSD&mid=shm&sid1=100"
web <- GET(url)
web
html <- htmlTreeParse(web, 
                      useInternalNode=TRUE , trim=TRUE, #노트삽입, 공백제거
                      encoding="UTF-8")
html
head(html)
root_node <- xmlRoot(html)

news <- xpathSApply(root_node,
                    c("//a[@class='nclicks(rig.secteco)']",
                    "//a[@class='cluster_text_headline nclicks(cls_pol.clsart)']",
                    "//a[@class='cluster_text_headline']",
                    "//a[@class='nclicks(rig.rankwor)']",
                    "//a[@class='nclicks(rig.ranksci)']",
                    "//a[@class='nclicks(rig.entopic)']"),
                    xmlValue)
news
news_sent = gsub('[[:punct:]]', '', news) #문장부호 제거 '[[:punct:]]'
news_sent = gsub('[\n\r\t]', '', news_sent) #이스케이프 제거
news_sent = gsub('[[:cntrl:]]', '', news_sent) #특수문자 제거 '[[:cntrl:]]'
news_sent = gsub('[a-z]', '', news_sent) #영문제거
news_sent = gsub('[A-Z]', '', news_sent) #대문자제거
news_sent = gsub('\\s+',' ',news_sent) #두개 이상의 공백 제거
news_sent

str(news_sent)

user_dic = data.frame(term=c('펜데믹','코로나19','타다'),tag='ncn')
buildDictionary(ext_dic='Sejong', user_dic = user_dic)

news_word <- sapply(news_sent,exNouns)
news_word <- gsub('[0-9]',' ',news_word)

str(news_word)
write.csv(news_word,'newstopic.csv',row.names=TRUE,quote=F)
news2 <- read.csv('newstopic.csv')
news2
str(news2)


news_cor <- Corpus(VectorSource(news_word))
news_term <- TermDocumentMatrix(news_cor,control=list(wordLengths=c(4,Inf)))
news_df <- as.data.frame(as.matrix(news_term)) 

newsResult <- sort(rowSums(news_df), decreasing=TRUE)
newsResult [1:10]
newsname <- names(newsResult)
newsfinal <- data.frame(word=newsname,freq=newsResult)
newsfinal


pal <- brewer.pal(12,"Paired")
windowsFonts(malgun=windowsFont("맑은 고딕"))  
x11()

jpeg('navernewstopic.jpg')
wordcloud(newsfinal$word, newsfinal$freq,
          scale=c(6,0.5), min.freq=2, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")
dev.off()

newsfinal
newspie <- newsResult[1:20]
str(newspie)

jpeg('navernewstopic2.jpg')
pie(newspie,labels=lab,
    main='네이버 뉴스토픽에 자주 출현하는 단어 빈도수',
    xlab='0318 오후7시 기준')
dev.off()


pct <- round(newspie/sum(newspie)*100,2)
lab <- paste(names(newspie),'\n',pct,'%')





