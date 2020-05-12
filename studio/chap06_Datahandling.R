#chap06_Datahandling
 
# 관련함수:plyr, dplyr, reshape, reshape2

# 1. dplyr 패키지 : dataframe 기준
# tbl_df() : 데이터셋 화면창 크기 만큼만 데이터 제공
# filter() : 지정한 조건식에 맞는 데이터 추출- subset()
# select() : 열의 추출 - data[, c(“Year”, “Month”)]
# mutate() : 열 추가 - transform()
# arrange() : 정렬 - order(), sort()
# summarise() : 집계

install.packages('dplyr')
library(dplyr)
library(help='dplyr')

# 1) 파이프 연산자 : %>% (주로 행열 구조를 가진 df을 토대로 사용함)
# 형식 ) df%>%func1()%>%func2() : 여러개의 함수 붙여쓸 수 있음
#        df가 func1에 의해 처리되고 그 결과가 다시 func2에 의해 처리됨
#head(iris) & filter(x,조건) 같이 실행하기
str(iris) # 150x5 data.frame
iris %>% head() %>% filter(Sepal.Length>=5.0)


# 2) tbl_df() : 콘솔의 사이즈에 맞춰 데이터를 잘라서 변수에 입력해줌
install.packages('hflights')
library(hflights)
str(hflights) # 227496x21

hflights_df <- tbl_df(hflights) #테이블 데이터프레임


# 3) filter() : 조건에 만족하는 행 출력 (데이터프레임에 사용)
# 형식) df %>% filter(조건식)
names(iris)
iris%>%filter(Species=='setosa')%>%head()
iris%>%filter(Sepal.Width>3.0)%>%head(10)
iris_df <- iris%>%filter(Sepal.Width>3.0)
str(iris_df)
# 형식) filter(df,조건식)
filter(hflights_df,Month==1 & DayofMonth==1)
filter(hflights_df,Month==1 | Month==2)



# 4) arrange() : 특정 컬럼의 행 정렬 함수 
# 형식) df %>% arrange(칼럼명) : 칼럼을 기준으로 오름차순 정렬 
iris %>% arrange(Sepal.Width) %>% head() #오름차순
iris %>% arrange(desc(Sepal.Width)) %>% head() #내림차순
# 형식) arrange(df, 칼럼명)
arrange(hflights_df,Month,ArrTime)



# 5) select() : 특정 칼럼 출력
# 형식) df %>% select()
iris %>% select(Sepal.Length,Petal.Length,Species) %>% head()
# 형식) select(df,col1,col2,,,)
select(hflights_df,DepTime,ArrTime,TailNum,AirTime)
select(hflights_df,Year:DayOfWeek)
select(hflights_df, -(Year:DayOfWeek))

# 문제) Month기준으로 내림차순 정렬하고, Year, Month, AirTime 칼럼 선택
# 방법 두가지 모두 확인하기
hflights_df %>% arrange(desc(Month)) %>% select(Year, Month, AirTime)
select(arrange(hflights_df,desc(Month)),Year, Month, AirTime)



# 6) mutate() : 파생 변수 생성 : 실제 데이터에 반영되지는 않음
# 형식) df %>% mutate(변수=함수or수식)
iris %>% mutate(diff=Sepal.Length-Sepal.Width) %>% select(diff) %>% 
  head(10) %>% arrange(diff)
# 형식) mutate(df,변수=함수or수식)
select(mutate(hflights_df,diff_delay=ArrDelay-DepDelay),
       ArrDelay,DepDelay,diff_delay)

select(mutate(hflights_df,diff_delay=ArrDelay-DepDelay),
       ArrDelay,DepDelay,diff_delay)


# 7) summarise() : 통계구하기
# n(),sum(), mean(), sd(), var(), median() 등의 함수 사용 - 기초 통계량
# 형식) df %>% summarise(변수=통계함수())
iris %>% summarise(col1_avg=mean(Sepal.Length),
                   col2_sd=sd(Sepal.Width))
#   col1_avg   col2_sd
# 1 5.843333 0.4358663

# 형식) summarise(df,변수=통계함수())
summarise(hflights_df,
          delay=mean(DepDelay,na.rm=T),
          delay_tot=sum(DepDelay,na.rm=T))
#     delay delay_tot
#     <dbl>     <int>
#   1  9.44   2121251

iris %>% summarise(col1_avg=mean(Sepal.Length),
                   col2_num=n()) #row갯수 출력함수



# 8) group_by(dataset, 집단변수) 
# 형식) df %>% group_by(집단변수)

names(iris) # 여기서 집단변수의 역할을 해주는 컬럼은 Species
table(iris$Species) # 3개의 집단별로 통계를 구할 수 있음

grp <- iris %>% group_by(Species)
grp #여기서 집단통계를 확인할 수는 없음

summarise(grp,mean(Sepal.Length))
#   Species    `mean(Sepal.Length)`
#   <fct>                     <dbl>
# 1 setosa                     5.01
# 2 versicolor                 5.94
# 3 virginica                  6.59
summarise(grp,mean(Sepal.Length,Sepal.Width)) #두개 칼럼 전체의 평균
summarise(grp,mean(Sepal.Length),mean(Sepal.Width)) #각각 평균 따로나옴

# [실습]
install.packages('ggplot2')
library(ggplot2)
data('mtcars')
str(mtcars) #32x11
head(mtcars) #mpg : 연산이 가능한 연속변수
table(mtcars$cyl) #3개의 domain을 가지고 있는 집단변수 4,6,8
table(mtcars$gear)#3,4,5로 구성된 집단변수

grp_cyl <- group_by(mtcars,cyl)
grp_cyl
summarise(grp_cyl,mean_avg=mean(mpg),
          mean_sd=sd(mpg))
grp_gear <- group_by(mtcars,gear)
summarise(grp_gear,mean(wt),sd(wt),var(wt))

#두 집단변수 그룹화
grp2 <- group_by(mtcars,cyl,gear) #cyl:1차, gear:2차
summarise(grp2,mean(mpg),sd(mpg))


# 예제) 각 항공기별(TailNum) 비행편수가 40편 이상
#       평균 비행거리가 2000마일 이상인 경우의
#       평균 도착 지연시간
#(1) 항공기별 그룹화
str(hflights_df)
plane <- hflights_df %>% group_by(TailNum)
#(2) 항공기별 요약 통계
planes <- summarise(plane,count=n(),dist_avg=mean(Distance,na.rm=T),
                          delay_avg=mean(ArrDelay,na.rm=T))
#(3) 항공기별 요약 통계 필터링
result <- filter(planes,count>=40 & dist_avg>=2000)







# 2. reshape2 패키지 활용
install.packages('reshape2') 
library(reshape2)

data <- read.csv(file.choose())

# 1) dcast() : long -> wide
# 형식) dcast(dataset,row~col,func)

data
#Data : 구매일자 (col)
#ID : 고객 구분자 (row)
#Buy : 구매수량 (sum)
table(data$Date)
dcast(data, Customer_ID ~ Date, sum)
wide <- dcast(data, Customer_ID ~ Date, sum)

library(ggplot2)
data(mpg)
str(mpg) # 234 11
#data.frame형태로 바꿔줌
mpg_df <- as.data.frame(mpg)

# 교차셀에 hwy 합계
# 원하는 컬럼만 선택
mpg_df <- select(mpg_df,cyl,drv,hwy)
dcast(mpg_df,cyl~drv,sum)

# 교차셀에 hwy 출현 건수 : 두가지 방법
# 교차분할표 1 : dcast
dcast(mpg_df,cyl~drv,length)
# 교차분할표 2 : table
# table(행집단변수,열집단변수)
table(mpg_df$cyl,mpg_df$drv)

unique(mpg_df$cyl)
unique(mpg_df$drv)



# 2) melt() : wide -> long

long <- melt(wide, id='Customer_ID')
long
#Customer_ID : 기준이 되는 칼럼
#variable : 열 이름 (자동생성)
#value : 교차 셀의 값 (자동생성)
str(long)
arrange(long,Customer_ID)

names(long) <- c('User_ID','Date','Buy')
long

# 예제)
data('smiths')
smiths #wide 구조
long <- melt(smiths,id='subject')
melt(smiths,id=1:2)
melt(smiths,id=1:5)
melt(smiths,id=2:2)

#long -> wide
wide <- dcast(long,subject~...) #...은 나머지 컬럼을 의미하는 약속




# 3) acast()
# 형식) acast(dataset,행~열~면)
data('airquality')
str(airquality) # 153 6
unique(airquality$Month) # 5 6 7 8 9
table(airquality$Month)
table(airquality$Day)

# wide -> long
air_melt <- melt(airquality,id=c('Month','Day'),
                 na.rm = T) #월단위-일단위로 컬럼이 집계됨
dim(air_melt)
table(air_melt$variable)

# [일,월,컬럼]
air_acast <- acast(air_melt,Day~Month~...)
dim(air_acast) # 31 5 4
class(air_acast) #array
str(air_acast)



#########추가내용###########
# 3. URL 만들기 : http://www.naver.com?name='홍길동'
# 1) base URL 만들기
baseurl <- 'http://www.sbus.or.kr/2018/lost/lost_02.htm'
# 2) page query 추가
#http://www.sbus.or.kr/2018/lost/lost_02.htm?Page=1
no <- 1:5
library(stringr)
page <- str_c('?Page=',no)
page #"?Page=1" "?Page=2" "?Page=3" "?Page=4" "?Page=5"

# outer(x(1),y(n),func) x를 기준으로 y를 결합해줌
# 1:n 형태로 문자열을 결합해주는 함수
page_url <- outer(baseurl,page,str_c) #list값으로 나옴
dim(page_url)


# reshape : 2d -> 1d
page_url <- sort(as.vector(page_url)) #변수 백터로 바꾸고 오름차순 정렬
page_url


# 3) sear query 추기
no <- 1:3
sear <- str_c('&sear=',no)
page_url <- outer(page_url,sear,str_c)
str(page_url)
class(page_url) #matrix
final_url <- sort(as.vector(page_url))




