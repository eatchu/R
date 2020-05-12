# 패키지 모음

# 1. dplyr 패키지 : dataframe 기준
# tbl_df() : 데이터셋 화면창 크기 만큼만 데이터 제공
# filter() : 지정한 조건식에 맞는 데이터 추출- subset()
# select() : 열의 추출 - data[, c(“Year”, “Month”)]
# mutate() : 열 추가 - transform()
# arrange() : 정렬 - order(), sort()
# summarise() : 집계

# 2. reshape2 패키지
# 1) dcast() : long -> wide
# 형식) dcast(dataset,row~col,func)
# 2) melt() : wide -> long
# 형식) melt(dataset, id='row(colname)')
# 3) acast()
# 형식) acast(dataset,행~열~면)

# 3. stringr 패키지
# 1) str_extract_all()
# 메타문자 : 패턴지정 특수기호 , list 값으로 나옴
# 2) str_length()
# 3) str_locate() / str_locate_all()
# 4) str_replace() / str_replace_all()
# 5) str_sub()
# 6) str_split() :\\s+ : 공백 정규식, +(1개 이상)
# 7) str_join : paste(name,collapse=',')
# 8) str_c : str_c(x,no), outer(x,y,str_c)

# 4. lattice 패키지
# 1)histogram(~x축, dataframe) : 막대차트-변수를 대상으로 백분율 적용
#            (~x축 | 조건, dataframe)
# 2)densityplot(~x축 | 조건, dataframe, groups=변수,plot.points=T, auto.key = T) 
#              : 선형차트-밀도 적용
# 3)barchart(y~x | 조건, dataframe, layout) : 막대차트
# 4)dotplot(y~x | 조건 , dataframe, layout) : 점차트
#   dotplot(y ~ x, dataframe, groups=변수, type="포인트모양", 
#         auto.key=list(space="공간배치", points=T, lines=T)) 
# 5)xyplot(y축~x축| 조건, dataframe or list, pch="", layout) : 점차트
# 6)coplot(y~x | a, data, overlap=0.5, number=6, row=2) : 막대,점차트
#         a조건 하에서 x에 대한 y 그래프를 그린다.
# 7)cloud(위도, 경도* 깊이) 산점도 그래프 플로팅


# 5. ggplot2 패키지
# 1) ggplot()
# 2) ggplot()
# 3) ggsave()함수 


# 6. ggmap 패키지
# 1) get_stamenmap(경도-위도변수, zoom=15,  maptype='terrain'or'watercolor')
# 2) ggmap()




# 데이터 자료 패키지들
# MASS
# RSADBE
# UsingR
# ggplot2
# mlmRev
# datasets






# 데이터베이스 연동시 필요한 패키지
#library("rJava")
#library("DBI")
# 1 dbConnect
# 2 dbGetQuery
# 3 dbSendUpdate
# 4 dbDisconnect
# 5 dbWriteTable
#library("RJDBC")
# 1 JDBC





# library(rJava)
# library(KoNLP)
# KoNLP패키지에 의존 패키지들 :
# 'Sejong' 'hash' 'tau' 'RSQLite' 'devtools'
# 6. 패키지 KoNLP
# 1 buildDictionary(ext_dic='sejong', user_dic = 값) : 패키지에 새로운 명사가 추가됨
# 2 extractNoun("나는 김진성이고, 한국 사람이다.") #명사들만 출력하는 함수
# 3 is.hangul


# wordcloud 설치    
# install.packages("wordcloud") 


# tm 설치 
# install.packages("tm")
# Corpus()
# VectorSource()  # 벡터 소스 생성 -> 코퍼스 생성 
# removePunctuation
# removeNumbers
# removeWords
# stopwords
# tm_map
# inspect
# TermDocumentMatrix(myCorpusPrepro,control=list(wordLengths=c(4,16))) 
# 단어 문서 행렬을 만들어줌 



# install.packages("arules")
# library(arules) 
# - as(), apriori(), inspect()


