# chap11_Descriptive Statistics



# 기술통계(Descriptive Statistics) 
# 대푯값 : 평균(Mean), 합계(Sum), 중위수(Median), 최빈수(mode), 사분위수(quartile) 등
# 산포도 : 분산(Variance), 표준편차(Standard deviation), 최소값(Minimum), 최대값(Maximum), 범위(Range) 등 
# 비대칭도 : 왜도(Skewness), 첨도(Kurtosis)



# 실습파일 가져오기
setwd("C:/IITT/2_Rwork/Part-III")
data <- read.csv("descriptive.csv", header=TRUE)
head(data) # 데이터셋 확인
str(data) # 300x8

# 1. 척도별 기술통계량
#  - 데이터 특성 보기(전체 데이터 대상)
dim(data) # 행(300)과 열(8) 정보 - 차원보기
length(data) # 열(8) 길이
length(data$survey) #survey 컬럼의 관찰치 - 행(300) 

str(data) # 데이터 구조보기 -> 데이터 종류,행/열,data
str(data$survey) 

# 데이터 특성(최소,최대,평균,분위수,노이즈-NA) 제공
summary(data) 

# 1) 명목/서열 척도 변수의 기술통계량
# - 명목상 의미없는 수치로 표현된 변수 - 성별(gender)     
length(data$gender)
summary(data$gender) # 최소,최대,중위수,평균-의미없음
table(data$gender) # 각 성별 빈도수 - outlier 확인-> 0, 5

data <- subset(data,data$gender == 1 | data$gender == 2) # 성별 outlier 제거
x <- table(data$gender) # 성별에 대한 빈도수 저장
x # outlier 제거 확인
barplot(x) # 범주형(명목/서열척도) 시각화 -> 막대차트

prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)

lab <- paste(round(y*100, 2),"%")
barplot(x,
        names.arg=c('남자','여자'),
        legend.text=lab,
        col=rainbow(2))


# 2) 등간/비율 척도 변수의 기술통계량
# - 비율척도 : 수치로 직접 입력한 변수(cost)  
length(data$cost)
summary(data$cost) # 요약통계량 - 의미있음(mean) - 8.784
mean(data$cost) # NA
data$cost

# 데이터 정제 - 결측치 제거 및 outlier 제거
plot(data$cost)
data <- subset(data,data$cost >= 2 & data$cost <= 10) # 총점기준
data
x <- data$cost
x

# 2. 대푯값 : cost 변수  이용
mean(x) # 평균 : 5.354
# 평균이 극단치에 영향을 받는 경우 - 중위수(median) 대체
median(x) # 5.4 : 값을 정렬해서 중간에 있는 값 
min(x)
max(x)
range(x) # min ~ max
length(x)

sort(x) # 오름차순 
sort(x, decreasing=TRUE) # 내림차순  

#정렬 : 특정컬럼 기준(cost)
data[sort(data$cost),] #vector : index정렬 
# order vs arrange : 행번호가 다르게 정렬됨
data[order(data$cost),] #data.frame : 내용정렬
arrange(data,cost) #data.frame


# 최빈수(Mode) : 빈도수가 가장 높은 값
mode(data)
install.packages('prettyR')
library(prettyR)
Mode(x) #5


mean(x) #5.354
median(x) #5.4
Mode(x) #5
#최빈수=중위수=평균 : 좌우대칭
#최빈수<중위수,평균 : 오른쪽 기울어짐
#최빈수>중위수,평균 : 왼쪽 기울어짐

hist(x)
?hist
table(x)

# 3. 산포도 : cost 변수 이용 
var(x) # 분산
sd(x) # 표준편차는 분t산의 양의 제곱근

quantile(x, 1/4) # 1 사분위수
quantile(x, 3/4) # 3 사분위수 

min(x) # 최소값
max(x) # 최대값
range(x) # 범위(min ~ max)


# 4. 비대칭도 :  패키지 이용
install.packages("moments")  # 왜도/첨도 위한 패키지 설치   
library(moments)
cost <- data$cost # 정제된 data
cost
# 왜도 - 평균을 중심으로 기울어진 정도
skewness(cost)  #-0.297234
# 음수 : 오른쪽으로 기울어짐
# 양수 : 왼쪽으로 기울어짐


# 첨도 - 표준정규분포와 비교하여 얼마나 뽀족한가 측정 지표
kurtosis(cost) #3기준 2.674163   


# 기본 히스토그램 
hist(cost,freq=F)

# 히스토그램 확률밀도/표준정규분포 곡선 

hist(cost, freq = F)

# 확률밀도 분포 곡선 : 히스토그램의 밀도 추정 
lines(density(cost), col='blue')

# 표준정규분포 곡선 
x <- seq(0, 8, 0.1)
curve( dnorm(x, mean(cost), sd(cost)), col='red', add = TRUE)
?curve

# 정규성 검정
shapiro.test(cost)
# W = 0.98187, p-value = 0.002959





# 5. 기술통계량 보고서 작성법

# 1) 거주지역 
data$resident2[data$resident == 1] <-"특별시"
data$resident2[data$resident >=2 & data$resident <=4] <-"광역시"
data$resident2[data$resident == 5] <-"시구군"

x<- table(data$resident2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#광역시 시구군 특별시 
#37.66  14.72  47.62

# 2) 성별
data$gender2[data$gender== 1] <-"남자"
data$gender2[data$gender== 2] <-"여자"
x<- table(data$gender2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#남자  여자 
#58.87 41.13

# 3) 나이
summary(data$age)# 40 ~ 69
data$age2[data$age <= 45] <-"중년층"
data$age2[data$age >=46 & data$age <=59] <-"장년층"
data$age2[data$age >= 60] <-"노년층"
x<- table(data$age2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#노년층 장년층 중년층 
#24.60  68.15   7.26

# 4) 학력수준
data$level2[data$level== 1] <-"고졸"
data$level2[data$level== 2] <-"대졸"
data$level2[data$level== 3] <-"대학원졸"

x<- table(data$level2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#고졸     대졸 대학원졸 
#39.41    36.44    24.15

# 5) 합격여부
data$pass2[data$pass== 1] <-"합격"
data$pass2[data$pass== 2] <-"실패"
x<- table(data$pass2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#실패  합격 
#40.85 59.15

head(data) # data Mart


# <논문/보고서에서 표본의 인구통계적특성 결과 제시 방법>
















