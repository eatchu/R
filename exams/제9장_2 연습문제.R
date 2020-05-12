#################################
## <제9장-2 연습문제>
################################# 


# 01. 트럼프 연설문(trump.txt)과 오바마 연설문(obama.txt)을 대상으로 빈도수가 2회 이상 단어를 대상으로 단어구름 시각화하시오.
trump <- file(file.choose())
trump <- readLines(trump)
trump[24] #텍스트파일 잘 가져왔는지 확인
length(trump) #24

# 영문시 문자 추출 불필요함
# fuc <- function(x){paste(extractNoun(as.character(x)),collapse = " ")}
# trump_wd <- sapply(trump,fuc)
# length(trump_wd[24]) #1
# str(trump_wd)

trump_cor <- Corpus(VectorSource(trump_wd))# 벡터 소스 생성 -> 코퍼스 생성 
inspect(trump_cor[24])
length(trump_cor[24]) #1

trump_update <- tm_map(trump_cor,removePunctuation)
trump_update <- tm_map(trump_update,removeNumbers)
trump_update <- tm_map(trump_update,tolower)
trump_update <- tm_map(trump_update,removeWords,stopwords('english'))
inspect(trump_update[24])

trump_term <- TermDocumentMatrix(trump_update, control=list(wordLengths=c(2,Inf)))
str(trump_term)

trump_df <- as.data.frame(as.matrix(trump_term)) 
str(trump_df)
trump_df[1]
trump_df[1:20,24]

trump_result <- sort(rowSums(trump_df), decreasing=TRUE)
str(trump_result)
trump_result[1:20]

trump_update <- tm_map(trump_update,removeWords,
                       c(stopwords('english'),'will','make'))
trump_term <- TermDocumentMatrix(trump_update, control=list(wordLengths=c(2,16)))
trump_df <- as.data.frame(as.matrix(trump_term)) 
trump_result <- sort(rowSums(trump_df), decreasing=TRUE)
trump_result[1:20]

tname <- names(trump_result)
word_df <- data.frame(word=tname,freq=trump_result)

pal <- brewer.pal(12,"Paired")
windowsFonts(malgun=windowsFont("맑은 고딕"))
wordcloud(word_df$word, word_df$freq,
          scale=c(5,1), min.freq=1, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")


################################################################

obama <- file(file.choose(),encoding='UTF-8')
obama_read <- readLines(obama)
str(obama_read)
obama_read[496]


obama_cor <- Corpus(VectorSource(obama_read))
inspect(obama_cor[494])
obama_update <- tm_map(obama_cor,removePunctuation)
obama_update <- tm_map(obama_update,removeNumbers)
obama_update <- tm_map(obama_update,tolower)
obama_update <- tm_map(obama_update,removeWords,stopwords('english'))
inspect(obama_update[494])
str(obama_update)
obama_tdm <- TermDocumentMatrix(obama_update,control=list(wordLengths=c(2,16)))
obama_result <- as.data.frame(as.matrix(obama_tdm))
obama_sort <- sort(rowSums(obama_result),decreasing = TRUE)
obama_sort[1:10]

obama_update <- tm_map(obama_update,removeWords,
                       c(stopwords('english'),'just','thats','will'))
obama_tdm <- TermDocumentMatrix(obama_update,control=list(wordLengths=c(2,16)))
obama_result <- as.data.frame(as.matrix(obama_tdm))
obama_sort <- sort(rowSums(obama_result),decreasing = TRUE)
obama_sort[1:10]
oname<-names(obama_sort)
obama_df <- data.frame(word=oname,freq=obama_sort)

?brewer.pal


pal <- brewer.pal(12,"Accent")
pal <- brewer.pal(10,"BrBG")
pal <- brewer.pal(1,"Greens")
windowsFonts(malgun=windowsFont("맑은 고딕"))
wordcloud(obama_df$word, obama_df$freq,
          scale=c(5,1), min.freq=2, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")




# 02. 공공데이터 사이트에서 관심분야 데이터 셋을 다운로드 받아서 빈도수가 5회 이상 단어를 이용하여 
#      단어 구름으로 시각화 하시오.
# 공공데이터 사이트 : www.data.go.kr

data <- read.csv(file.choose())
head(data)
str(data)
data_df <- data.frame(word=data$키워드,freq=data$빈도수)
str(data_df)
range(data_df$freq)




pal <- brewer.pal(12,"BrBG")
windowsFonts(malgun=windowsFont("맑은 돋음"))
wordcloud(data_df$word, data_df$freq,
          scale=c(5,1), min.freq=100, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")



crime <- read.csv(file.choose())
head(crime)
str(crime)
crime_new <- crime[-c(1,2)]
str(crime_new)
crime_sum <- sort(colSums(crime_new),decreasing = TRUE)
head(crime_sum)
str(crime_sum) # 88개
range(crime_sum) # 87-355341
cname <- names(crime_new)
crime_sum

crime_df <- data.frame(word=cname,freq=crime_sum)


pal <- brewer.pal(10,"BrBG")
windowsFonts(malgun=windowsFont("맑은 고딕"))

jpeg("crimeplace.jpg",width=720, height=480,quality=150)
wordcloud(crime_df$word, crime_df$freq,
          scale=c(7,1), min.freq=3000, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")
dev.off()


piecrime <- crime_sum[1:15]
pal <- brewer.pal(12,"Set1")
layer <- pie(crime_sum[1:15],colors=pal,radius=1,
    main='범죄 발생 빈도수에 따른 지역별 현황',
    labels=lab)
pct <- round(piecrime/sum(piecrime)*100, 1) #백분율 
lab <- paste(names(piecrime), "\n", pct, "%")

jpeg('crime.jpg')
layer <- pie(crime_sum[1:15],colors=pal,radius=1,
             main='범죄 발생 빈도수에 따른 지역별 현황',
             labels=lab)
dev.off()




