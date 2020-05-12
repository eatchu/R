# chap13_Ttest_Anova(연습문제)

#############################################
# 추론통계분석 - 1-1. 단일집단 비율차이 검정
#############################################

# 01. 중소기업에서 생산한 HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 
# 기존 구매비율 15% 보다 향상되었는지를 각 단계별로 분석을 수행하여 검정하시오.


#연구가설(H1) : 기존 구매비율과 차이가 있다.
#귀무가설(H0) : 기존 구매비율과 차이가 없다.

#조건) 구매여부 변수 : buy (1: 구매하지 않음, 2: 구매)

#(1) 데이터셋 가져오기
setwd("C:/ITWILL/2_Rwork/Part-III")
hdtv <- read.csv("hdtv.csv", header=TRUE)

# (2) 빈도수와 비율 계산
head(hdtv)
summary(hdtv)
hdtv$buy2[hdtv$buy==1]<-'1구매하지않음'
hdtv$buy2[hdtv$buy==2]<-'2구매'
prop.table(table(hdtv$buy2)) #비구매 80% 구매 20% 


# (3)가설검정
# 1. 양측 검정 : 구매 기준
binom.test(c(10,40), p=0.15) #p-value = 0.321 : 귀무가설 채택
binom.test(10,50, p=0.15) #p-value = 0.321
#귀무가설(H0) : 기존 구매비율과 차이가 없다.





#################################################
# 추론통계학 분석 - 1-2. 단일집단 평균차이 검정
#################################################

# 02. 우리나라 전체 중학교 2학년 여학생 평균 키가 148.5cm로 알려져 있는 상태에서 
# A중학교 2학년 전체 500명을 대상으로 10%인 50명을 표본으로 선정된 데이터 셋을 이용하여
# 모집단의 평균과 차이가 있는지를 각 단계별로 분석을 수행하여 검정하시오.

#(1) 데이터셋 가져오기
sheight<- read.csv("student_height.csv", header=TRUE)

# (2) 기술통계량 평균 계산
head(sheight)
height<-sheight$height
summary(sheight)
mean(height) #149.4

# (3) 정규성 검정
shapiro.test(height) # p-value = 0.0001853 : 정규분포x 비모수검정 사용
hist(height,freq=T)

# (4) 가설검정 
wilcox.test(height, mu=148.5) #p-value = 0.067 : 귀무가설 채택
# A중학교 2학년 여학생키의 평균과 전체 중학교 2학년 여학생키의 평균은 차이가 없다



#################################################
# 추론통계학 분석 - 2-1. 두집단 비율 차이 검정
#################################################

# 03. 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 
# 대해서 만족도에 차이가 있는가를 검정하시오.

# 힌트) 두 집단 비율 차이 검정
#  조건) 파일명 : two_sample.csv, 변수명 : gender(1,2), survey(0,1)
# gender : 남학생(1), 여학생(2)
# survey : 불만(0), 만족(1)
# prop.test('성공횟수', '시행횟수')

data <- read.csv('two_sample.csv')
data
summary(data)
data$gender2[data$gender==1] <- '1남학생'
data$gender2[data$gender==2] <- '2여학생'
data$survey2[data$survey==0] <- '0불만'
data$survey2[data$survey==1] <- '1만족'
length(data$gender2)
length(data$survey2)
table(data$gender2,data$survey2)
table(data$gender2)
prop.test(c(138,107),c(174,126)) #p-value = 0.2765 : 귀무가설 채택
# [해설] 진학한 대학에 대해서 남학생과 여학생은 만족도에 차이가 없다



##################################################
# 추론통계학 분석 - 2-2. 두집단 평균 차이 검정
##################################################

# 04. 교육방법에 따라 시험성적에 차이가 있는지 검정하시오.

#힌트) 두 집단 평균 차이 검정
#조건1) 파일 : twomethod.csv
#조건2) 변수 : method : 교육방법, score : 시험성적
#조건3) 모델 : 교육방법(명목)  ->  시험성적(비율)
#조건4) 전처리 : 결측치 제거 : 평균으로 대체 

data <- read.csv('twomethod.csv')
summary(data)
str(data)
data <- data[c(2,3)]
data

data$score <- ifelse(is.na(data$score),round(mean(data$score,na.rm=TRUE)),data$score)
data


method1 <- subset(data,method==1)
method2 <- subset(data,method==2)
length(method2$score) #39
length(method1$score) #24
mean(method1$score) #17.04167
mean(method2$score) #28.69231

var.test(method1$score, method2$score) #p-value = 0.7487 : 동질성분포 -> 모수검정
t.test(method1$score,method2$score) #p-value = 1.829e-06 : 귀무가설기각 
t.test(method1$score,method2$score,alter="greater", conf.int=TRUE, conf.level=0.95) 
t.test(method1$score,method2$score,alter="less", conf.int=TRUE, conf.level=0.95) 

# [해석] 2교육방법의 평균이 1교육방법의 평균보다 좋다








############################################
# 추론통계분석 - 3-2. 두 집단 이상 평균차이 검정
############################################



# 05. iris 데이터셋을 이용하여 다음과 같이 분산분석(aov)을 수행하시오
# 독립변수 : Species(집단변수)
# 종속변수 : 1 칼럼 ~ 4 칼럼 (전제조건 만족하는 변수 선택)
# 분산분석 실행 후 해석-> 사후검정 실행 후 해석
data <- iris
data1 <- iris[c(1,5)]
data2 <- iris[c(2,5)]
data3 <- iris[c(3,5)]
data4 <- iris[c(4,5)]

summary(data) #NA없음
boxplot(data$Sepal.Width) #이상치 존재
# data2 이상치 정제
boxplot(data2$Sepal.Width)$stats
data2 <- subset(data2,data2$Sepal.Width<=4.0&data2$Sepal.Width>=2.2)

# 동질한 경우 선택
bartlett.test(Sepal.Length ~ Species, data=data1) #p-value = 0.0003345
bartlett.test(Sepal.Width ~ Species, data=data2) #p-value = 0.77 : 동질
bartlett.test(Petal.Length ~ Species, data=data3) #p-value = 9.229e-13
bartlett.test(Petal.Width ~ Species, data=data4) #p-value = 3.055e-09

# 분산검정
result <- aov(Sepal.Width ~ Species, data=data2)
summary(result) # p-value 1.51e-15 ***
# [해설] 유의미한 수준에서 집단별 값의 차이를 가짐

# 사후검정
plot(TukeyHSD(result))
TukeyHSD(result)
#                           diff        lwr        upr    p adj
# versicolor-setosa    -0.5908815 -0.7434736 -0.4382893 0.000000
# virginica-setosa     -0.4025957 -0.5544390 -0.2507525 0.000000
# virginica-versicolor  0.1882857  0.0380483  0.3385231 0.009778
# [해설] 세집단 모두 차이를 보인다


methods(plot)
library(dplyr)
data2 %>% group_by(Species) %>% summarise(age=mean(Sepal.Width))
2.79-3.38 #-0.59






barchart()








