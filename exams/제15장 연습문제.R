#################################
## <제15장 연습문제>
################################# 

###################################
## 선형 회귀분석 연습문제 
###################################

# 01. ggplot2패키지에서 제공하는 diamonds 데이터 셋을 대상으로 
# carat, table, depth 변수 중 다이아몬드의 가격(price)에 영향을 
# 미치는 관계를 다음과 같은 단계로 다중회귀분석을 수행하시오.

library(ggplot2)
data(diamonds)
str(diamonds)
y <- diamonds$price
x1 <- diamonds$carat
x2 <- diamonds$depth
x3 <- diamonds$table

data <- data.frame(y,x1,x2,x3)


# 단계1 : 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
lm_ <- lm(fomula=y~.,data=data)
summary(lm(fomula=y~x1,data=data))
summary(lm(fomula=y~x2,data=data))
summary(lm(fomula=y~x3,data=data))
summary(lm_)
#carat이 영향을 가장 크게 미치는 변수
head(data)
cor(data)
summary(lm_)
# 85.4%의 예측력을 가진 유의미한 값


# 단계2 : 다중회귀 분석 결과를 정(+)과 부(-) 관계로 해설
# carat(+) depth(-)  table(-) 


# 02. mtcars 데이터셋을 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.

library(datasets)
str(mtcars) # 연비 효율 data set 
y <- mtcars$mpg
x1 <- mtcars$hp
x2 <- mtcars$wt

data2 <- data.frame(y,x1,x2)

# 단계1 : 연비(mpg)는 마력(hp), 무게(wt) 변수와 어떤 상관관계를 갖는가? 
cor(data2)
lm_2 <- lm(fomula=y~.,data=data2)
head(data2)
summary(lm_2)
cor(data2)
# 82.7%의 유의미한 값을 가짐 


# 단계2 : 마력(hp)과 무게(wt)는 연비(mpg)에 어떤 영향을 미치는가? 
# 마력과 무게가 커질수록 연비는 작아진다 반비례 관계에 있음
# 특히 무게가 연비에 더 큰 영향을 끼침 


# 단계3 : hp = 90, wt = 2.5t일 때 회귀모델의 예측치는?
predict(lm_2, data.frame(x1=90,x2=2.5)) #24.67313 



# 03. product.csv 파일의 데이터를 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.
setwd("C:/IITT/2_Rwork/Part-IV")
product <- read.csv("product.csv", header=TRUE)

#  단계1 : 학습데이터(train),검정데이터(test)를 7 : 3 비율로 샘플링
str(product)
s <- sample(1:nrow(product),0.7*nrow(product))
train <- product[s,]
test <- product[-s,]

#  단계2 : 학습데이터 이용 회귀모델 생성 
#        변수 모델링) y변수 : 제품_만족도, x변수 : 제품_적절성, 제품_친밀도
promodel <- lm(제품_만족도~.,data=train)
promodel #절편 0.5825  x1기울기 0.1051  x2 기울기 0.7056  
summary(promodel) # 64.8%의 확률로 유의미한 값을 가짐


#  단계3 : 검정데이터 이용 모델 예측치 생성 
premodel<-predict(promodel,test)
length(premodel) #80개
error <- premedel - test$제품_만족도
mse <- mean(error**2)
mse #0.1209616


#  단계4 : 모델 평가 : cor()함수 이용  
r <- cor(premodel,test$제품_만족도)
r #0.7043361



###################################
## 로지스틱 회귀분석 연습문제 
###################################
# 04.  admit 객체를 대상으로 다음과 같이 로지스틱 회귀분석을 수행하시오.
# <조건1> 변수 모델링 : y변수 : admit, x변수 : gre, gpa, rank 
# <조건2> 7:3비율로 데이터셋을 구성하여 모델과 예측치 생성 
# <조건3> 분류 정확도 구하기 

# 파일 불러오기
admit <- read.csv('admit.csv')
str(admit) # 'data.frame':	400 obs. of  4 variables:
#$ admit: 입학여부 - int  0 1 1 1 0 1 1 0 1 0 ...
#$ gre  : 시험점수 - int  380 660 800 640 520 760 560 400 540 700 ...
#$ gpa  : 시험점수 - num  3.61 3.67 4 3.19 2.93 3 2.98 3.08 3.39 3.92 ...
#$ rank : 학교등급 - int  3 3 1 4 4 2 1 2 3 2 ...

# 1. data set 구성 
idx <- sample(1:nrow(admit), nrow(admit)*0.7)
train <- admit[idx, ]
test <- admit[-idx, ]

# 2. model 생성 
linearm <- glm(admit~.,data=train,family= 'binomial')
summary(linearm)


# 3. predict 생성 
linearp <- predict(linearm,test)
linearp <- ifelse(linearp>=0.5,1,0)
table(linearp)
# linearp
#  0   1 
# 110  10
lineart <- test$admit
table(lineart)
# lineart
# 0  1 
# 83 37 
tab <- table(lineart,linearp)
#        linearp
# lineart  0  1
#       0 77  1
#       1 40  2


# 4. 모델 평가(분류정확도) : 혼돈 matrix 이용/ROC Curve 이용
# 정분류
acc <- (tab[1,1]+tab[2,2])/nrow(test)
acc #0.6583333
# 오분류
nacc <- (tab[1,2]+tab[2,1])/nrow(test)
nacc #0.3416667
# 특이도 
tab[1,1]/(tab[1,1]+tab[1,2]) #0.9871795
# 민감도 (재현율) : 관측치
recall <- tab[2,2]/(tab[2,1]+tab[2,2]) #0.04761905
# 정확율 : 예측치
precision <- tab[2,2]/(tab[1,2]+tab[2,2]) #0.6666667
# 불균형 비율
F1_score <- 2*((recall*precision)/(recall+precision))
F1_score #0.08888889

pr <- prediction(linearp, lineart)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)





