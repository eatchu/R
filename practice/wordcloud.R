wordcloud("Many years ago the great British explorer George Mallory, who was to die on Mount Everest, was asked why did he want to climb 
it. He said, \"Because it is there.\"
Well, space is there, and we're going to climb it, and the 
moon and the planets are there, and new hopes for knowledge 
and peace are there. And, therefore, as we set sail we ask 
God's blessing on the most hazardous and dangerous and greatest 
adventure on which man has ever embarked.",
          ,random.order=FALSE)
data(crude)
crude

crude <- tm_map(crude, removePunctuation)
crude <- tm_map(crude, function(x)removeWords(x,stopwords()))
wordcloud(crude)
crude
str(crude)
inspect(crude)

tdm <- TermDocumentMatrix(crude)
str(tdm)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d
wordcloud(d$word,d$freq)
wordcloud(d$word,d$freq,c(8,.3),2)
wordcloud(d$word,d$freq,c(8,.5),2,,FALSE,.1)

pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(d$word,d$freq,c(8,.3),2,,FALSE,,.15,pal)


pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]
wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,,.15,pal)

wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,TRUE,.15,pal)

wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,,.15,pal,
          vfont=c("gothic english","plain"))


wordcloud(d$word,d$freq,c(8,.3),2,100,TRUE,,.15,
          pal,vfont=c("script","plain"))


wordcloud(d$word,d$freq,c(8,.3),2,100,TRUE,,.15,pal,
          vfont=c("serif","plain"))





