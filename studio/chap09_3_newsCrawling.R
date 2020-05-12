#chap09_3_newsCrawling

# http://media.daum.net
# <a href="url"> 기사내용 </a>


# 1. 관련 패키지 설치
# 웹문서의 뉴스기사를 가져오는 패키지
install.packages('httr') #url을 요청하는 패키지
library(httr)
install.packages('XML') #태그 속의 문서를 가져오는 패키지
library(XML)

# 2. URL 요청
url <- "http://media.daum.net"
web <- GET(url)
web # status : 200

# 3. HTML 파싱 : text -> html 문서 변형
help("htmlTreeParse")
html <- htmlTreeParse(web, 
                     useInternalNode=TRUE , trim=TRUE, #노트삽입, 공백제거
                     encoding="UTF-8")
html
root_node <- xmlRoot(html) # 시작 지점을 찾아주는 함수

# 4. tag 자료수집 : "//tag[@속성='값']"
news <- xpathSApply(root_node,"//a[@class='link_txt']", xmlValue)
news[1]
news
str(news)
news2 <- news[1:59]

# 5. news 전처리
news_sent = gsub('[[:punct:]]', '', news2) #문장부호 제거 '[[:punct:]]'
news_sent = gsub('[\n\r\t]', '', news_sent) #이스케이프 제거
news_sent = gsub('[[:cntrl:]]', '', news_sent) #특수문자 제거 '[[:cntrl:]]'
news_sent = gsub('[a-z]', '', news_sent) #영문제거
news_sent = gsub('[A-Z]', '', news_sent) #대문자제거
news_sent = gsub('\\s++','',news_sent) #두개 이상의 공백 제거
news_sent


# 6. file save
setwd("C:/IITT/2_Rwork/output")
write.csv(news_sent,'news_data.csv',
          row.names=TRUE,quote=F) #행이름저장, 따움표제거 

news_data <- read.csv('news_data.csv')
head(news_data)
colnames(news_data) <- c('no','news_txt')
news_data

# 7. 토픽분석

#신규단어 등록
user_dic = data.frame(term=c('펜데믹','코로나19','타다'),tag='ncn')
buildDictionary(ext_dic='Sejong', user_dic = user_dic)

#명사추출하고 단어구름 만들기 
news_txt <- news_data$news_txt
news_txt <- sapply(news_txt,exNouns)
news_cor <- Corpus(VectorSource(news_txt)) 
inspect(news_cor)

news_cor <- tm_map(news_cor,removeWords,stopwords("전술불발→제2비례당"))

#TDM
news_term <- TermDocumentMatrix(news_cor,
                                control=list(wordLengths=c(4,Inf)))
inspect(news_term)


#평서문 변환
news_df <- as.data.frame(as.matrix(news_term)) 


#단어 빈도수 내림차순 정렬
newsResult <- sort(rowSums(news_df), decreasing=TRUE)
newsResult [1:10]
newsname <- names(newsResult)
newsfinal <- data.frame(word=newsname,freq=newsResult)
newsfinal

#차트 생성
pal <- brewer.pal(12,"Paired")
windowsFonts(malgun=windowsFont("맑은 고딕"))  
x11()
wordcloud(newsfinal$word, newsfinal$freq,
          scale=c(6.5,1), min.freq=2, random.order=F, 
          rot.per=.5, colors=pal, family="malgun")


pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]
wordcloud(newsfinal$word,newsfinal$freq,
          c(5,.1),2,,FALSE,,.15,pal)

wordcloud(newsfinal$word,newsfinal$freq,c(8,.5),2,,FALSE,TRUE,.1)


jpeg('newstopic2.jpg')
wordcloud(newsfinal$word, newsfinal$freq,
          scale=c(6.5,1), min.freq=2, random.order=F, 
          rot.per=.5, colors=pal, family="malgun")
dev.off()


