#chap07_EDA_Preprocessing 


# 1. 탐색적 데이터 조회

# 실습 데이터 읽어오기
setwd("C:/IITT/2_Rwork/Part-II")
dataset <- read.csv("dataset.csv", header=TRUE) # 헤더가 있는 경우
# dataset.csv - 칼럼과 척도 관계 
dataset


# 1) 데이터 조회
# - 탐색적 데이터 분석을 위한 데이터 조회 

# (1) 데이터 셋 구조
names(dataset) # 변수명(컬럼)
attributes(dataset) # names(), class, row.names
str(dataset) # 데이터 구조보기
dim(dataset) # 차원보기 : 300 7
nrow(dataset) # 관측치 수 : 300
length(dataset) # 칼럼수 : 7 
length(dataset$resident) # 300

# (2) 데이터 셋 조회
# 전체 데이터 보기
dataset # print(dataset) 
View(dataset) # 뷰어창 출력

# 칼럼명 포함 간단 보기 
head(dataset)
head(dataset, 10) 
tail(dataset) 

# (3) 칼럼 조회 
# 형식) dataframe$칼럼명   
dataset$resident
length(dataset$age) # data 수-300개 

# 형식) dataframe["칼럼명"] 
dataset["gender"] 
dataset["price"]

# $ vs index 
res <- dataset$resident #vector :  int [1:300]
res2 <- dataset['resident'] #data.frame 
res3 <- dataset[1] %>% head()  #data.frame
res4 <- dataset[,1] %>% head() #vector :num [1:6]
str(res) 
str(res2)
str(res3)
str(res4) 


# 형식) dataframe[색인] : 색인(index)으로 원소 위치 지정 
dataset[2] # 두번째 컬럼 
dataset[6] # 여섯번째 컬럼
dataset[3,] # 3번째 관찰치(행) 전체
dataset[,3] # 3번째 변수(열) 전체


# dataset에서 2개 이상 칼럼 조회 : data.frame형태
dataset[c("job", "price")]
dataset[c(2,6)] 
dataset[c(1,2,3)] 
dataset[c("job":"price")] # error 
names(dataset) #[1] "resident" "gender" "job" "ageposition" "price" "survey"  
dataset[3:6]
dataset[c(1:3)] 
dataset[c(2,4:6,3,1)] 
dataset[-c(2)] # dataset[c(1,3:7)] 
# dataset에서 2개 이상 칼럼 조회 : vector형태
x <- dataset$gender
y <- dataset$price
x;y %>% head()




# 2. 결측치(NA) 발견과 처리 
# 99999999 - NA : R에서 상수로 인식하는 값

# 1) 결측치 확인
summary(dataset$price) #특정 컬럼 조회
summary(dataset) #전체 컬럼 조회

table(is.na(dataset$price)) #특정 컬럼 조회
# FALSE  TRUE  T값이 결측치
# 270    30 
table(is.na(dataset)) #전체 컬럼 조회 (합계값)

sum(dataset$price) # NA가 존재 유무확인 : NA값이 나오면 결측지 존재


# 2) 결측치 제거
price2 <- na.omit(dataset$price) #특정 컬럼 제거
length(price2) #300-30=270

dataset2 <- na.omit(dataset) #전체 컬럼 제거
                             #주의 : 결측치를 가지고 있는 행 자체가 전부 사라짐
dim(dataset2) #209 7

# 특정 칼럼 기준 결측치 제거 : subset() 생성
stock <- read.csv(file.choose())
str(stock) #data.frame 6706 69
#Market.Cap : 시가총액

library(dplyr)
stock_df <- stock %>% filter(!is.na(Market.Cap))
dim(stock_df) # 5028  69 :1678이 결측치


# 3) 결측치 처리 
#(1) 0으로 대체
x <- dataset$price
dataset$price2 <- ifelse(is.na(x),0,x)

#(2) 평균으로 대체
dataset$price3 <- ifelse(is.na(x),mean(x,na.rm=T),x)

dim(dataset) #300 9 : 컬럼 2개 추가됨(price2,price3)
head(dataset[c('price','price2','price3')],30)


# 4) 통계적 방법의 결측치 처리
# ex) 1~4 : age 결측치 -> 각 학년별 평균으로 대체
age <- round(runif(12,min=20,max=25))
grade <- rep(1:4, 3)#round(runif(12,min=0.5,max=4))

age[5] <- NA
age[8] <- NA

DF <- data.frame(age,grade)
DF

# age 칼럼 대상 결측치 -> age2 생성
age1 <- ifelse(is.na(DF$age),round(mean(DF$age,na.rm=T)),DF$age)
age2 <- data.frame(age1,grade)
age2


# 각 학년별 나이 합계 
n <- nrow(DF) #12

g1=0;g2=0;g3=0;g4=0

# 결측치에 평균을 넣어 합계 구하기
for(i in 1:n){
  if(DF$grade[i]==1){
    if(!is.na(DF$age[i])){
      g1=g1+DF$age[i]
    }else{
      g1=g1+mean(DF$age,na.rm=T)
    }
  }else if(DF$grade[i]==2){
    if(!is.na(DF$age[i])){
      g2=g2+DF$age[i]
    }else{
      g2=g2+mean(DF$age,na.rm=T)
    }
  }else if(DF$grade[i]==3){
    if(!is.na(DF$age[i])){
      g3=g3+DF$age[i]
    }else{
      g3=g3+mean(DF$age,na.rm=T)
    }
  }else if(DF$grade[i]==4){
    if(!is.na(DF$age[i])){
      g4=g4+DF$age[i]
    }else{
      g4=g4+mean(DF$age,na.rm=T)
    }
  }
}
g1;g2;g3;g4


#결측치를 제거하고 합계 구하기
for(i in 1:n){ # i = index
  if(DF$grade[i]==1 & !is.na(DF$age[i])){
    g1 = g1 + DF$age[i]
  }else if(DF$grade[i]==2 & !is.na(DF$age[i])){
    g2 = g2 + DF$age[i]
  }else if(DF$grade[i]==3 & !is.na(DF$age[i])){
    g3 = g3 + DF$age[i]
  }else if(DF$grade[i]==4 & !is.na(DF$age[i])){
    g4 = g4 + DF$age[i]
  }
}  

g1;g2;g3;g4


#결측치에만 각 학년 평균값을 넣기
tab <- table(DF$grade)
age3 <- age
for(i in 1:n){
  if(is.na(DF$age[i]) & DF$grade[i]==1)
    age3[i] <- g1/2
  if(is.na(DF$age[i]) & DF$grade[i]==2)
    age3[i] <- g2/2
  if(is.na(DF$age[i]) & DF$grade[i]==3)
    age3[i] <- g3/2
  if(is.na(DF$age[i]) & DF$grade[i]==4)
    age3[i] <- g4/2
}
age3
DF1 <- DF
DF1$age3 <- round(age3)
DF1





# 3. 이상치(outlier) 발견과 정제 
# - 정상 범주에서 크게 벗어난 값


# 실습 데이터 읽어오기
setwd("C:/IITT/2_Rwork/Part-II")
dataset <- read.csv("dataset.csv", header=TRUE) # 헤더가 있는 경우
# dataset.csv - 칼럼과 척도 관계 
str(dataset)


# 1) 범주형(집단)변수 
#   이상치 발견 : table(), 차트
gender <- dataset$gender
gender
table(gender)


# 0   1   2   5  (0,5값이 이상치)
# 2  173 124  1 
pie(table(gender))

#   이상치 정제
dataset <- subset(dataset,gender==1 | gender==2)
pie(table(dataset$gender))

dataset <- filter(dataset,gender==1 | gender==2)
table(dataset$gender)

dataset$gender <- ifelse(dataset$gender==1|dataset$gender==2,
                  dataset$gender,NA)
table(dataset$gender)


# 2) 연속형 변수
#    이상치 발견
price <- dataset$price # 연속형 데이터
length(price) 
plot(price)
summary(price)
boxplot(price)$stats
#   2~10 정상 범주만 출력
dataset2 <- subset(dataset,price>=2&price<=10)
dim(dataset) # 297
dim(dataset2) # 248 : 49개가 정제됨
plot(dataset2$price)
summary(dataset2$price)
boxplot(dataset2$price)
stem(dataset2$price) 
#   dataset2 : age(20-69)
dataset2 <- subset(dataset2,age>=20&age<=69)
boxplot(dataset2$age)


# 3) 이상치 발견이 어려운 경우
# boxplot : 정상범주에서 상하위 0.3%에 해당하는 값을 알려줌 
boxplot(dataset$price)
boxplot(dataset$price)$stats #정상범주내의 상/하한값을 보여줌 : 굵은선

dataset3<-subset(dataset,price>=2.1&price<=7.9)
boxplot(dataset3$price)


# [실습] 
library(ggplot2)
# 이상치 발견
str(mpg) # 234 11
hwy <- mpg$hwy
summary(hwy)
plot(hwy)
# 발견한 이상치 처리
boxplot(hwy)$stats
mpg2 <- subset(mpg,hwy>=12&hwy<=37)
length(mpg2$hwy) # 231 : 3개 정제됨
boxplot(mpg2$hwy)



# 이상치 정제
# 방법(1) 이상치 값을 제거 : 같은 행의 다른컬럼 값도 제거됨
mpg2 <- subset(mpg,hwy>=12&hwy<=37)
length(mpg2$hwy) # 231 : 3개 정제됨
boxplot(mpg2$hwy)
# 방법(2) 이상치 값을 결측치로 대체
hwy2 <- ifelse(mpg$hwy<12|mpg$hwy>37,NA,mpg$hwy)
mpg$hwy2 <- hwy2
mpg_df <- as.data.frame((mpg))
mpg_df[c('hwy','hwy2')]






# 4. 코딩 변경
# - 데이터 가독성, 척도 변경(ex연속형->범주형), 최초 코딩 내용 변경

# 1) 데이터 가독성 
# - 범주형 변수일 경우 해당하는 숫자가 무엇을 의미하는지 알기 어려움
# 형식) dataset$추가칼럼[조건식] <- 값
# 정제된 컬럼인지 확인하기
table(dataset$gender) 
# 각 숫자의 의미를 인식하기 위해 새로운 컬럼을 추가해 값을 부여하기
# 성별 식별
dataset$gender2[dataset$gender==1] <- '남자'
dataset$gender2[dataset$gender==2] <- '여자'
head(dataset,20)
# 주소 식별
dataset2$resident2[dataset2$resident==1] <- '1.서울특별시'
dataset2$resident2[dataset2$resident==2] <- '2.인천광역시'
dataset2$resident2[dataset2$resident==3] <- '3.대전광역시'
dataset2$resident2[dataset2$resident==4] <- '4.대구광역시'
dataset2$resident2[dataset2$resident==5] <- '5.시구군'
head(dataset2,20)

# 이렇게 식별하는 이유 : 데이터를 편하게 보기 위하여

#    1  2
# 1 67 43 
# 2 26 20
# 3 16 10
# 4  6  9
# 5 19 15

#               남자 여자
# 1.서울특별시   67   43
# 2.인천광역시   26   20
# 3.대전광역시   16   10
# 4.대구광역시    6    9
# 5.시구군       19   15




# 2) 척도 변경 : 연속형 -> 범주형
range(dataset2$age) # 20 69 : 정제를 해야 값이 나옴
#                           : 결측치가 들어있으면 안나옴
# 나이식별 (집단변수)
dataset2$age2[dataset2$age<=30] <- '청년층' 
dataset2$age2[dataset2$age>30&dataset2$age<=55] <- '중년층'
dataset2$age2[dataset2$age>55] <- '장년층'
head(dataset2,20)

# 3) 역코딩 ; 1->5, 5->1
table(dataset2$survey)
#  1  2  3  4  5 
# 15 91 93 27  6 
survey <- dataset2$survey 
# 12345->54321로 만들기
csurvey <- 6-survey 
dataset2$survey <- csurvey
table(dataset2$survey)
# 1  2  3  4  5 
# 6 27 93 91 15 



# 5. 탐색적 분석을 위한 시각화
# - 변수 간의 관계분석

setwd('C:/IITT/2_Rwork/Part-II')
new_data <- read.csv('new_data.csv',header=T)
dim(new_data) # 231 15
str(new_data)

# 1) 범주형(명목/서열) vs 범주형(명목/서열)
# - 방법 : 교차테이블, barplot, mosaicplot, ggplot, geom_bar

# 거주지역(5) vs 성별(2) : 거주지역에 따른 성별분표
tab1 <- table(new_data$resident2, new_data$gender2)
tab1
#               남자 여자
# 1.서울특별시   67   43
# 2.인천광역시   26   20
# 3.대전광역시   16   10
# 4.대구광역시    6    9
# 5.시구군       19   15

table(new_data$resident2, new_data$gender2)
dcast(new_data,resident2~gender2,length)

par(mfrow=c(1,1))
# 막대 차트
barplot(tab1,col=rainbow(5),
        beside=T,
        main='성별에 따른 거주지역 분포 현황',
        legend=row.names(tab1))

tab2 <- table(new_data$gender2,new_data$resident2)

barplot(tab2,col=rainbow(2),
        ylim=c(0,100),
        beside=T,
        main='거주지역에 따른 성별의 분포 현황',
        legend=row.names(tab2))

# 정사각형 기준 차트
mosaicplot(tab1,col=rainbow(5),
           main='성별에 따른 거주지역 분포 현황')
mosaicplot(tab2,col=rainbow(2),
           main='거주지역에 따른 성별의 분포 현황')


# 고급 시각화 
# 직업유형(범주형) vs 나이(범주형)
library(ggplot2)

# 미적 객체 생성
obj <- ggplot(data=new_data,
       aes(x=job2,fill=age2))
# 막대 차트 추가
obj + geom_bar()
# 밀도 1 기준으로 막대 차트 추가
obj + geom_bar(position='fill')
# 결측치 값까지 확인 가능한 교차 테이블
table(new_data$job2,new_data$age2,useNA='ifany') 


# 2) 숫자형(비율/등간) vs 범주형(명목/서열)
# - 방법 : boxplot, 카테고리별 통계
install.packages('lattice')
library(lattice) # 격자

# 나이(비율) vs 직업유형(명목)
densityplot(~ age,groups=job2, #격자내에 그룹을 설정
            data=new_data, #선택할 데이터 자료
            plot.points=T, 
            auto.key=T) #범례 추가


# 3) 숫자형(비율) vs 범주형(명목) vs 범주형(명목)
# - 기준컬럼은 숫자형이 되어야함
# 형식) densityplot(~x(기준 숫자형)|factor(범주형),groups=그래프범주형)
#(1) 구매금액을 성별과 직급으로 분류
densityplot(~price|factor(gender2), #기준이 되는 컬럼|범주의 수만큼 격자(칸)생성
            groups=position2, #각 격자내에 그룹 생성
            data=new_data,
            auto.key = T)
#(2) 구매금액을 직급과 성별로 분류
densityplot(~price|factor(position2), 
            groups=gender2,
            data=new_data,
            auto.key = T)



# 4) 숫자형 vs 숫자형 or 숫자형 vs 숫자형 vs 범주형
# - 방법 : 상관계수, 산점도, 산점도 행렬

#(1) 숫자형(age) vs 숫자형(price)
# 두개의 숫자형 컬럼의 관계 알아보기
cor(new_data$age,new_data$price) #결측치 있으면 값이 안나옴
new_data2 <- na.omit(new_data) #결측치 제거
cor(new_data2$age,new_data2$price) #0.0881251
# 0.0881251 : +-0.3~0.4 이상
plot(new_data2$age,new_data2$price)

#(2) 숫자형 vs 숫자형 vs 범주형
#형식) xyplot(y(숫자형) ~ x(숫자형|factor(범주형))
xyplot(price~age|factor(gender2),
       data=new_data)

xyplot


# 6. 파생변수 생성
# - 기존 변수 -> 새로운 변수
# 1) 사칙연산
# 2) 1:1 방식 : 기존컬럼1 -> 새로운 컬럼1
# 3) 1:n 방식 : 기준변수1 -> 새로운 컬럼n

user <- read.csv('user_data.csv',header=T)
str(user)

#(1) 1:1 기존칼럼 -> 새로운 칼럼
#    더미변수 : 1,2->1(주택) 3,4->2(아파트)
user$house_type2 <- ifelse(user$house_type==1|user$house_type==2,
                           1,2)
table(user$house_type2) # 1=79 2=321

#(2) 1:n 기준변수 -> 새로운 칼럼
#    지불정보 테이블
pay_data <- read.csv('pay_data.csv',header=T)
str(pay_data) #3개의 범주형 칼럼과 1개의 숫자형 칼럼
names(pay_data)

# dcast 함수 이용 long -> wide
library(reshape2) 
product_price <- dcast(pay_data,user_id~product_type,sum)
dim(product_price) #303   6
names(product_price) <- c('user_id','식료품(1)','생필품(2)','의류(3)',
                          '잡화(4)','기타(5)')
product_price


#(3) 파생변수 추가 (join)
library(dplyr)
# 형식) left_join(df1,df2,by='기준컬럼')
user_pay_data <- left_join(user,product_price,by='user_id')
dim(user_pay_data) # 400 11
head(user_pay_data)


#(4) 사칙 연산 : 총 구매 금액
names(user_pay_data) #각 컬럼의 인덱스 값 확인
user_pay_data$tot_price <- user_pay_data[7]+user_pay_data[8]+
  user_pay_data[9]+user_pay_data[10]+user_pay_data[11]
head(user_pay_data,20)
dim(user_pay_data) # 400 12
names(user_pay_data$tot_price) <- 'tot_price'


tot_price <- user_pay_data[,7]+user_pay_data[,8]+
  user_pay_data[,9]+user_pay_data[,10]+user_pay_data[,11]
user_pay_data$tot_price2 <- tot_price
head(user_pay_data,20)
dim(user_pay_data) # 400 12

user_pay_data <- user_pay_data[-13]






