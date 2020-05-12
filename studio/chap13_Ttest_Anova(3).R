# chap13_Ttest_Anova(3)


############################################
# 추론통계분석 - 3-1. 두 집단 이상 비율차이 검정
############################################
# - 두 집단 이상 비율차이 검정

# 1. 파일가져오기 
data <- read.csv("three_sample.csv", header=TRUE)
data

# 2. 두 집단 이상 subset 작성(데이터 정제,전처리) 
method <- data$method 
survey<- data$survey
method
survey 

# 3.기술통계량(빈도분석)
table(method, useNA="ifany") # 50 50 50 -> 3그룹 모두 관찰치 50개
table(method, survey, useNA="ifany") # 그룹별 클릭수 : 1-43, 2-34, 3-37


# 4. 두 집단 이상 비율차이 검정
# prop.test(그룹별 빈도, 그룹수) -> 집단이 늘어나도 동일한 함수 사용-땡큐
prop.test(c(34,37,39), c(50,50,50)) # p-value = 0.1165 -> 귀무가설 채택


############################################
# 추론통계분석 - 3-2. 두 집단 이상 평균차이 검정
############################################
# 두 집단 이상 평균차이 검정 
# 독립변수 : 집단변수(범주형)
# 종속변수 : 숫자변수(연속형)

#################################
##1.일원배치 분산분석
#종속변수 ~ 독립변수
#################################


# 1. 파일 가져오기
data <- read.csv("three_sample.csv")

# 2. 데이터 정제/전처리 - NA, outline 제거
data <- subset(data, !is.na(score), c(method, score)) 
data # method, score

# (1) 차트이용 - ontlier 보기(데이터 분포 현황 분석)
plot(data$score) # 차트로 outlier 확인 : 50이상과 음수값
boxplot(data$score)$stats # 2~8.5

# (2) outlier 제거 - 평균(14) 이상 제거
length(data$score)#91
data2 <- subset(data, score <= 8.5) # 14이상 제거
length(data2$score) #88(3개 제거)

# (3) 정제된 데이터 보기 
x <- data2$score
boxplot(x)
plot(x)

# 3. 집단별 subset 작성
# method: 1:방법1, 2:방법2, 3:방법3
data2$method2[data2$method==1] <- "방법1" 
data2$method2[data2$method==2] <- "방법2"
data2$method2[data2$method==3] <- "방법3"

table(data2$method2) # 교육방법 별 빈도수 

# 4. 동질성 검정 - 정규성 검정
# bartlett.test(종속변수 ~ 독립변수) # 독립변수(세 집단)
bartlett.test(score ~ method2, data=data2) #p-value = 0.1905

# 귀무가설 : 집단 간 분포의 모양이 동질적이다.
# 해설 : 유의수준 0.05보다 크기 때문에 귀무가설을 기각할 수 없다. 

# 동질한 경우 : aov() - Analysis of Variance(분산분석)
# 동질하지 않은 경우 - kruskal.test()

# 5. 분산검정(집단이 2개 이상인 경우 분산분석이라고 함)
# aov(종속변수 ~ 독립변수, data=data set)
# 귀무가설 : 집단 간 평균에 차이가 없다.
result <- aov(score ~ method2, data=data2)

# aov()의 결과값은 summary()함수를 사용해야 p-value 확인 
summary(result) 
#              Df Sum Sq Mean Sq  F value   Pr(>F)    
# method2      2  99.37   49.68   43.58  9.39e-14 ***
# p-value : 0에 가까운 값으로 귀무가설 기각
# [해설] 매우 유의미한 수준으로 적어도 한 집단에서 평균에 차이를 보임



# 사후검정
TukeyHSD(result)
#                 diff        lwr        upr     p adj
# 방법2-방법1  2.612903  1.9424342  3.2833723 0.0000000 <0.05
# 방법3-방법1  1.422903  0.7705979  2.0752085 0.0000040 <0.05
# 방법3-방법2 -1.190000 -1.8656509 -0.5143491 0.0001911 <0.05

# 세 집단 모두 유의미한 수준에서 점수의 차이를 보인다

plot(TukeyHSD(result))
# 0을 걸친 그래프가 나오면 집단간에 차이가 없는걸로 간주할수있음




#################################
##2.이원배치 분산분석
#종속변수 ~ 독립변수1 + 독립변수2
#################################

# 쇼핑몰 고객의 연령대별(20대~50대), 시간대별(오전/오후) 구매현황 분석
# - 독립변수 : 연령대, 시간대
# - 종속변수 : 구매현황


# 1. dataset 생성
age <- round(runif(100,min=20,max=59))
time <- round(runif(100,min=0,max=1))
buy <- round(runif(100,min=1,max=10))

# 2. 집단변수 생성
shop_df <- data.frame(age,time,buy)
shop_df$age2[shop_df$age>=20&shop_df$age<30] <- '20대'
shop_df$age2[shop_df$age>=30&shop_df$age<40] <- '30대'
shop_df$age2[shop_df$age>=40&shop_df$age<50] <- '40대'
shop_df$age2[shop_df$age>=50&shop_df$age<60] <- '50대'

shop_df$time2[shop_df$time==0] <- '0오전'
shop_df$time2[shop_df$time==1] <- '1오후'

shop_df

# 3. 동질성검정
bartlett.test(buy~age2,data=shop_df) #p-value =0.5806 : 동질
bartlett.test(buy~time2,data=shop_df) #p-value = 0.6379 : 동질

# 4. 분석검정 : 모수검정
# 귀무가설 : 집단간(연령대별/시간대별) 평균의 차이가 없다
# 대립가설 : 적어도 한 집단에 평균의 차이가 있다
result <- aov(formula=buy~age2+time2,data=shop_df)
summary(result)
#              Df Sum Sq Mean Sq F value Pr(>F)  
# age2         3   35.8  11.929   1.956 0.1258  
# time2        1   18.5  18.460   3.027 0.0851 

# [해설] 귀무가설 채택 : 집단간의 구매평균에는 큰 차이가 없다


# 5. 사후검정 : 집단별 분산차이
library(dplyr)
shop_df %>% group_by(age2) %>% summarise(buy_age=mean(buy))
shop_df %>% group_by(time2) %>% summarise(time_age=mean(buy))





library(lattice)
densityplot(~buy|time2,groups = age2,data=shop_df,
            auto.key = TRUE)






