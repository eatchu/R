data <- read.csv(file.choose())
head(data)

data_df <- data.frame(호선명=data$호선명,승객수=data$승차총승객수,하차총승객수=data$하차총승객수)
head(data_df)
library(dplyr)
data_df_lname <- group_by(data_df,호선명=data_df$호선명)
data_sum <- summarise(data_df_lname,
                      승차총승객수=sum(승객수),
                      하차총승객수=sum(하차총승객수))
data_sum
str(data_sum)
data_sum <- as.data.frame(data_sum)
head(data_sum)

data_sum$tot_people <- data_sum[,2]+data_sum[,3]
names(data_sum$tot_people) <- 'tot_people'
str(data_sum)
data_tot <- data_sum[c(1,4)]
data_tot
str(data_tot)


data_sort <- arrange(data_tot,desc(data_tot$tot_people))
data_sort

table(data_sort$호선명,data_sort$tot_people)
melt(data_sort,id='호선명')


data_vec <- data_sort$tot_people
data_name <- data_sort$호선명
data_vec
str(data_vec)
str(data_name)
data_name <- as.character(data_name)
data_vec <- names(data_sort$호선명)
names(data_vec) <- data_name
data_vec

data_pie <- data_vec[1:10]

co <- brewer.pal(10,'Paired')
pie(data_pie,col=co,radius=1,
    labels=lab) 

pct <- round(data_pie/sum(data_pie)*100, 1)
lab <- paste(names(data_pie), "\n", pct, "%")


jpeg('subway.jpg')
pie(data_pie,col=co,radius=1,
    labels=lab,
    main='지하철 승하차 승객수 빈도수 현황')
dev.off()


data_vec
data_name <- names(data_vec)
data_df <- data.frame(word=data_name,freq=data_vec)

windowsFonts(malgun=windowsFont("맑은 고딕")) 
wordcloud(data_df$word, data_df$freq,
           scale=c(5,1), min.freq=0, random.order=F, 
           rot.per=.1, colors=pal, family="malgun")

jpeg('subway2.jpg')
wordcloud(data_df$word,data_df$freq,c(8,.3),2,,FALSE,,.15,pal)
dev.off()





wordcloud(data_df$word,data_df$freq,c(8,.3),2,100,TRUE,,.15,pal,
          vfont=c("serif","plain"))







