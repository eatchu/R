#################################
## <제11장 연습문제>
################################# 

#01. descriptive.csv 데이터 셋을 대상으로 다음 조건에 맞게 빈도분석 및 기술통계량 분석을 수행하시오

setwd("C:/IITT/2_Rwork/Part-III")
data <- read.csv("descriptive.csv", header=TRUE)
head(data) # 데이터셋 확인

# 조건1) 명목척도 변수인 학교유형(type), 합격여부(pass) 변수에 대해 빈도분석을 수행하고 
# 결과를 막대그래프와 파이차트로 시각화 
type <- table(data$type)
pass <- table(data$pass)
names(pass) <- c('합격','실패')
pass
pass_y <- prop.table(pass)
pass_y <- paste(names(pass),round(pass_y*100,2),'%')
barplot(pass,legend.text = pass_y,col=pass)
pie(pass, labels = pass_y)


# 조건2) 비율척도 변수인 나이 변수에 대해 요약치(평균,표준편차)와 비대칭도(왜도와 첨도)
# 통계량을 구하고, 히스토그램으로 비대칭도 설명
age <- data$age
summary(age)
mean(age) #53.88
sd(age) #6.813247
skewness(age) #0.3804892 : 양수 왼쪽으로 기울어짐
kurtosis(age) #1.866623 : 정규분포에 비해 완만햔 형태
hist(age,freq=F)


# 조건3) 나이 변수에 대한 밀도분포곡선과 정규분포 곡선으로 정규분포 검정
lines(density(age), col='blue')
x <- seq(0, 8, 0.1)
curve( dnorm(x, mean(age), sd(age)), col='red', add = TRUE)
shapiro.test(age)
# W = 0.92312, p-value = 2.633e-11






