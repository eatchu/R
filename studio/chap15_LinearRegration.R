# Chap15_1_Regration

######################################################
# 회귀분석(Regression Analysis)
######################################################
# - 특정 변수(독립변수:설명변수)가 다른 변수(종속변수:반응변수)에 어떠한 영향을 미치는가 분석

###################################
## 1. 단순회귀분석 
###################################
# - 독립변수와 종속변수가 1개인 경우

# 단순선형회귀 모델 생성  

product <- read.csv("C:/IITT/2_Rwork/Part-IV/product.csv", header=TRUE)
head(product) # 친밀도 적절성 만족도(등간척도 - 5점 척도)

str(product) # 'data.frame':  264 obs. of  3 variables:
y = product$제품_만족도 # 종속변수
x = product$제품_적절성 # 독립변수
df <- data.frame(x, y)

# 회귀모델 생성 
# 형식) lm(formula= y ~ x 변수, data) 
result.lm <- lm(formula=y ~ x, data=df)
result.lm # 회귀계수 

# 회귀방정식y=ax+b (a:기울기 b:절편)
head(df)

X <- 4 # 입력변수
Y <- 3 # 정답
a <- 0.7393 # 기울기
b <- 0.7789 # 절편

y <- a*X+b
err <- y-Y

names(result.lm)
#"coefficients" : 회귀계수
#"residuals" : 오차(잔차)
#"fitted.values" : 적합치(예측치)

result.lm$coefficients
result.lm$residuals
result.lm$fitted.values
?lm

# 회귀모델 예측 
predict(result.lm, data.frame(x=5) ) 

# (2) 선형회귀 분석 결과 보기
summary(result.lm)
#<회귀모델 해석 순서>
# 1 F-statistic : p-value < 0.05 값이 나와야 통계적으로 유의
# 2 Residual R-squared : 1에 가까울수록 예측력이 정학해짐
# 3 x의 유의성 검정 : t value(-1.96~+1.96), p value  0.05


# 상관계수
cor(df) #0.7668527
r <- 0.7668527
r_squared <- r**2
r_squared # 예측력 58.8%



# (3) 단순선형회귀 시각화
# x,y 산점도 그리기 
plot(formula=y ~ x, data=df, col='red',
     xlim=c(0,5),ylim=c(0,5))
# 회귀분석
result.lm <- lm(formula=y ~ x, data=df)
# 회귀선 
l <- abline(result.lm, col='blue')


x <- product$제품_적절성
y <- product$제품_만족도

# x 기울기 = Covxy / Sxx
Covxy = mean((x - mean(x)) * (y - mean(y)))
Sxx = mean((x-mean(x))**2)
a <- Covxy / Sxx
a # 0.7392762

# y 절편 
b <- mean(y) - a * mean(x)
b # 0.7788583



###################################
## 2. 다중회귀분석
###################################
# - 여러 개의 독립변수 -> 종속변수에 미치는 영향 분석
# 가설 : 음료수 제품의 적절성(x1)과 친밀도(x2)는 제품 만족도(y)에 정의 영향을 미친다.

product <- read.csv("C:/Rwork-I/Part-IV/product.csv", header=TRUE)
head(product) # 친밀도 적절성 만족도(등간척도 - 5점 척도)


#(1) 적절성 + 친밀도 -> 만족도  
y = product$'제품_만족도' # 종속변수
x1 = product$'제품_친밀도' # 독립변수1
x2 = product$'제품_적절성' # 독립변수2

df <- data.frame(x1, x2, y)

result.lm <- lm(formula=y ~ x1 + x2, data=df)
#result.lm <- lm(formula=y ~ ., data=df) 나머지를 나타낼때 . 사용



# 계수 확인 
result.lm # y= 0.09593x1 + 0.68522x2 + 0.66731
b <- 0.66731
a1 <- 0.09593
a2 <- 0.68522
head(df)
X1 <- 3
X2 <- 4
Y <- 3
# 다중회귀방정식
y = a1*X1+a2*X2+b
err <- y-Y
abs(err) #0.69598
cor(df)
# 분석결과 확인
summary(result.lm)
# F-statistic : p-value: < 2.2e-16
# R-squared : 0.5975
# x 유의성 검정 :
# x1  2.478   0.0138 *  -> 친밀도
# x2  15.684  2e-16 *** -> 적절성 : 만족도에 더 많은 영향을 미침

plot(result.lm)


install.packages('car')
library(car)
str(Prestige)
# education : 교육수준 (x1)
# income : 수입 (y)
# women : 여성비율 (x2)
# prestige : 평판 (x3)
# census : 직업수
# type : factor
row.names(Prestige) #106가지의 직업들

df <- Prestige[c(1,2,3,4)]

model <- lm(income~.,data=df)
model
plot(df)
summary(model)
# education  0.944    0.347   : 교육은 수입에 영향을 크게 미치지 않음
# women     -5.948 4.19e-08 ***
# prestige   4.729 7.58e-06 ***

cor(df)

res <- model$residuals
length(res)

# MSE : mean Squared error 평균제곱오차
# 모델을 평가하는 대표적인 도구 - 표준화를 시켜줘야함
# (1) 표준화 (scale)
res_scale <- scale(res) #mean=0, sd=1
# (2) mse 
mse <- mean(res_scale**2)
mse #0.9901961
shapiro.test(res) #p-value = 1.816e-11



#######################################
### 3. x변수 선택
#######################################

new_data <- Prestige[,c(1:5)]
dim(new_data) # 102 5
library(MASS)

model2 <- lm(income~.,data=new_data)
step <- stepAIC(model2,direction='both')
# stepAIC : AIC 지수값으로 알맞는 x변수선택을 해줌
#           AIC가 가장 낮은 변수를 선택하는것이 타당


model3 <- lm(income~women+prestige,data=new_data)
summary(model3)




###################################
# 4. 다중공선성(Multicolinearity)
###################################
# - 독립변수 간의 강한 상관관계로 인해서 회귀분석의 결과를 신뢰할 수 없는 현상
# - 생년월일과 나이를 독립변수로 갖는 경우
# - 해결방안 : 강한 상관관계를 갖는 독립변수 제거

# (1) 다중공선성 문제 확인
library(car)
fit <- lm(formula=Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width, data=iris)

vif(fit) #분산팽창요인 정보 제공 함수
# Sepal.Width Petal.Length  Petal.Width -> 값이 크면 상관성이 높다고 판단할수있다
# 1.270815     15.097572    14.234335 

sqrt(vif(fit))>2 # root(VIF)가 2 이상인 것은 다중공선성 문제 의심 



# (2) iris 변수 간의 상관계수 구하기
cor(iris[,-5]) # 변수간의 상관계수 보기(Species 제외) 
#x변수 들끼 계수값이 높을 수도 있다. -> 해당 변수 제거(모형 수정) <- Petal.Width


#[해석] 즉 서로 강한 상관관계를 보이는 Petal.Length  Petal.Width 칼럼 중에 하나는 제거해아함


# (3) 학습데이터와 검정데이터 분류
x <- sample(1:nrow(iris), 0.7*nrow(iris)) # 전체중 70%만 추출
train <- iris[x, ] # 학습데이터 추출 70% - 모델을 만듬
test <- iris[-x, ] # 검정데이터 추출 30% - 검정을 함
dim(train) # 105 5 model학습용
dim(test) # 45 5 model검정용


# (4) model 생성 : Petal.Width 변수를 제거한 후 회귀분석 
iris_model <- lm(formula=Sepal.Length ~ Sepal.Width + Petal.Length, data=train)
iris_model
summary(iris_model)


# (5) model 예측치 : test set(x) -> y prediction
y_pred <- predict(iris_model, test)
y_pred
length(y_pred)
y_true <- test$Sepal.Length


# (6) model 평가 

# 1. MSE 표준화
error <- y_true - y_pred
mse <- mean(error**2)
mse #0.1209616

# 2. 상관계수 r
r <- cor(y_true, y_pred)
r #0.8822798

# 3. scale
a <- iris_model$residuals
res <- scale(a) 
mse2 <- mean(res**2)
mse2 #0.9904762



# 시각화 평가
plot(y_true,col='blue',type='l',label='y true')
points(y_pred,col='red',type='l',label='y pred')
legend('topleft',cex=2,legend=c('y true','y pred'), col=c('blue','red'),pch='--')



##########################################
##  5. 선형회귀분석 잔차검정과 모형진단
##########################################

# 1. 변수 모델링  
# 2. 회귀모델 생성 
# 3. 모형의 잔차검정 
#   1) 잔차의 등분산성 검정 - 오차와 실제값
#   2) 잔차의 정규성 검정 - 정답과 예측치
#   3) 잔차의 독립성(자기상관) 검정 
# 4. 다중공선성 검사 
# 5. 회귀모델 생성/ 평가 
# 잔차 : 관측값과 계산값의 차


names(iris)

# 1. 변수 모델링 : y:Sepal.Length <- x:Sepal.Width,Petal.Length,Petal.Width
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width


# 2. 회귀모델 생성 
model <- lm(formula = formula,  data=iris)
model
names(model)


# 3. 모형의 잔차검정
plot(model)
#Hit <Return> to see next plot: 잔차 vs 적합값 -> 등분산성 - 패턴없이 무작위 분포(포물선 분포 좋지않은 적합) 
#Hit <Return> to see next plot: Normal Q-Q -> 정규분포 : 대각선이면 잔차의 정규성 
#Hit <Return> to see next plot: 척도 vs 위치 -> 중심을 기준으로 고루 분포 
#Hit <Return> to see next plot: 잔차 vs 지렛대값 -> 중심을 기준으로 고루 분포 

# (1) 등분산성 검정 
plot(model, which =  1) 
methods('plot') # plot()에서 제공되는 객체 보기 

# (2) 잔차 정규성 검정
attributes(model) # coefficients(계수), residuals(잔차), fitted.values(적합값)
res <- residuals(model) # 잔차 추출 
res <- model$residuals
shapiro.test(res) # 정규성 검정 - p-value = 0.9349 >= 0.05
# 귀무가설 : 정규성과 차이가 없다.

# 정규성 시각화  
hist(res, freq = F) 
qqnorm(res)

# (3) 잔차의 독립성(자기상관 검정 : Durbin-Watson) 
install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값
# DW = 2.0604(2~4), p-value = 0.6013>=0.05


# (4) 다중공선성 검사 
library(car)
sqrt(vif(model)) > 2 # TRUE 

# (5) 모델 생성/평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length 
model <- lm(formula = formula,  data=iris)
summary(model) # 모델 평가




###################################
### 6. 범주형 변수 사용
###################################

# 범주형(gender) 변수 -> 더미변수(0,1) 생성
# 범주형 변수 기울기 영향 없음 (절편에만 영향 미침)
# 범주형 범주가 n개이면 더미변수 수 : n-1
# ex) 혈액형 (AB,A,B,O) - 3개의 변수가 필요
#    x1  x2  x3
#  A  1   0   0
#  B  0   1   0
#  O  0   0   1
# AB  0   0   0 (base기준)
# Factor : 범주형 -> 더미변수

setwd('C:/IITT/2_Rwork/Part-IV')
insurance <- read.csv('insurance.csv')
str(insurance) # 1338  7
# 'data.frame':	1338 obs. of  7 variables:
# $ age     : 나이 int  19 18 28 33 32 31 46 37 37 60 ...
# $ sex     : 성별 Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...
# $ bmi     : 비만지수 num  27.9 33.8 33 22.7 28.9 ...
# $ children: 자녀수 int  0 1 3 0 0 0 1 3 2 0 ...
# $ smoker  : 흡연여부 Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...
# $ region  : 지역 Factor w/ 4 levels "northeast","northwest",..: 4 3 3 2 2 3 3 2 1 2 ...
# $ charges : 의료비(y) num  16885 1726 4449 21984 3867 ...

# 범주형 변수 : sex(2), smoker(2), region(4)
# 기준 (base) : level1(base)=0, level2=1

# 회귀모델 생성
insurance2 <- insurance[,-c(5,6)] #흡연유뮤, 지역 제거거
head(insurance2)
ins_lm <- lm(charges~.,data=insurance2)
ins_lm
# intercept = -7460.0 
# sexmale = 1321.7 : female=0 male=1
# [해석] 여성에 비해서 남성의 의료비가 더 비쌈



x <- c('male','female')
insurance2$sex <- factor(insurance2$sex, levels=x)
insurance2$sex

ins_lm <- lm (charges~. , data=insurance2)
ins_lm
# intercept = -6138.2 
# sexfemale = -1321.7 : female=1 male=0
# [해석] 여성이 남성에 비해서 의료비 절감

male <- subset(insurance2,sex=='male')
female <- subset(insurance2,sex=='female')

mean(male$charges) #13956.75
mean(female$charges) #12569.58



## dummy 변수 vs 절편
insurance3 <- insurance[,-6]
head(insurance3)
smok_lm <- lm(charges~smoker,data=insurance3)
smok_lm
summary(smok_lm)
# (Intercept)    smokeryes  
#   8434          23616  
# base : smoker no =0  smoker yes = 1
# [해석] 흡연자가 비흡연자에 비해서 23616 의료비 증가


no <- subset(insurance3,smoker=='no')
mean(no$charges) #8434.268
yes <- subset(insurance3, smoker=='yes')
mean(yes$charges) #32050.23


# 4개 범주 -> 3개 더미변수 생성
insurance4 <- insurance




###############################################
# 15_2. 로지스틱 회귀분석(Logistic Regression) 
###############################################

# 목적 : 일반 회귀분석과 동일하게 종속변수와 독립변수 간의 관계를 나타내어 
# 향후 예측 모델을 생성하는데 있다.

# 차이점 : 종속변수가 범주형 데이터를 대상으로 하며 입력 데이터가 주어졌을 때
# 해당 데이터의결과가 특정 분류로 나눠지기 때문에 분류분석 방법으로 분류된다.
# 유형 : 이항형(종속변수가 2개 범주-Yes/No), 다항형(종속변수가 3개 이상 범주-iris 꽃 종류)
# 다항형 로지스틱 회귀분석 : nnet, rpart 패키지 이용 
# a : 0.6,  b:0.3,  c:0.1 -> a 분류 

# 분야 : 의료, 통신, 기타 데이터마이닝

# 선형회귀분석 vs 로지스틱 회귀분석 
# 1. 로지스틱 회귀분석 결과는 0과 1로 나타난다.(이항형)
# 2. 정규분포 대신에 이항분포를 따른다.
# 3. 로직스틱 모형 적용 : 변수[-무한대, +무한대] -> 변수[0,1]사이에 있도록 하는 모형 
#    -> 로짓변환 : 출력범위를 [0,1]로 조정
# 4. 종속변수가 2개 이상인 경우 더미변수(dummy variable)로 변환하여 0과 1를 갖도록한다.
#    예) 혈액형 AB인 경우 -> [1,0,0,0] AB(1) -> A,B,O(0)


# 단계1. 데이터 가져오기
weather = read.csv("C:/IITT/2_Rwork/Part-IV/weather.csv", stringsAsFactors = F) 
# Stringsasfactors=F 순수한 문자형으로 가져오기
dim(weather)  # 366  15
head(weather)
str(weather)

# chr 칼럼, Date, RainToday 칼럼 제거 : 범주형을 y변수로 선택함
weather_df <- weather[, c(-1, -6, -8, -14)]
str(weather_df)

# RainTomorrow 칼럼 -> 로지스틱 회귀분석 결과(0,1)에 맞게 더미변수 생성      
weather_df$RainTomorrow[weather_df$RainTomorrow=='Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow=='No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

# y 빈도수
table(weather_df$RainTomorrow)
#  0   1 
# 300  66  -> 비가 안올경우의 수가 더 많음
prop.table(table(weather_df$RainTomorrow))
#     0         1 
# 0.8196721 0.1803279 -> 82% , 18%



#  단계2.  데이터 셈플링 (7:3)
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx, ] #학습데이터 256
test <- weather_df[-idx, ] #검정데이터 110


#  단계3.  로지스틱  회귀모델 생성 : 학습데이터 
#  GLM : generalized linear models
weater_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial')
# family = binomial y변수가 이항이라는 뜻
weater_model 
summary(weater_model) 


# 단계4. 로지스틱  회귀모델 예측치 생성 : 검정데이터 
# newdata=test : 새로운 데이터 셋, type="response" : 0~1 확률값으로 예측 
pred <- predict(weater_model, newdata=test, type="response")  
pred #y가 나올 수 있는 확률값으로 나옴 : 0.5이상 비가온다, 0.5미만 비가안온다
range(pred,na.rm=TRUE) #0.002605013 0.989237906 
summary(pred)
str(pred)

# cutoff=0.5
cpred <- ifelse(pred>=0.5,1,0)
prop.table(table(cpred))
table(cpred)

y_true <- test$RainTomorrow
prop.table(table(y_true)) 
table(y_true)


tab <- table(y_true,cpred)
#         cpred
# y_true   0  1
#      0  88  7
#      1   6  7

#정분류
acc <- (tab[1,1]+tab[2,2])/nrow(test)
acc # 0.8636364 : 86%정도의 분류정확도

# 오분류
nacc <- (tab[1,2]+tab[2,1])/nrow(test)
nacc # 0.1181818

# 특이도
no <- tab[1,1]/(tab[1,1]+tab[1,2])
no # 0.9263158

# 민감도(재현율) - 실제값 대상
yes <- tab[2,2]/(tab[2,1]+tab[2,2])
yes # 0.5384615

# 정확률 - 예측치 대상
precision <- tab[2,2]/(tab[1,2]+tab[2,2])
precision # 0.5


# 불균형비율 (F1_score) : 재현율 & 정확율
recall <- yes
F1_score <- 2*((recall*precision)/(recall+precision))
F1_score # 0.5185185 
 

# [해설] 비가 오지 않을 확률은 93%로 예측도가 높았고 비가 올 확률은 54%로 비교적 예측도가 낮다
#        즉 전체의 분류정확도는 86%정도로 예측도가 꽤 높은편이다




### ROC Curve를 이용한 모형평가(분류정확도)  ####
# Receiver Operating Characteristic

install.packages("ROCR")
library(ROCR)

# ROCR 패키지 제공 함수 : prediction() -> performance
pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)



##############################################
### 다항형 로지스틱 회귀분석 : nnet
##############################################

install.packages('nnet')
library(nnet)

idx <- sample(nrow(iris),nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]

# 활성함수
# 이항 : sigmoid function : 0~1 확률값
# 다항 : softmax function : 0~1 확률값 (sum=1)
# y1=0.1 y2=0.1 y3=0.8

names(iris)
model <- multinom(Species~.,data=train)
names(model)

# fitted.values : 확률로 예측
range(model$fitted.values) # 1.107001e-37 1.000000e+00 : 0-1사이의 확률값
str(model$fitted.values) # 각각의 꽃의 종류의 예측치를 나타내줌
model$fitted.values[2,]
train[2,]

# 예측치 : 범주로 예측 
y_pred <- predict(model, test)
y_pred
y_true <- test$Species

tab <- table(y_true,y_pred)
tab
acc <- (tab[1,1]+tab[2,2]+tab[3,3])/nrow(test)
acc # 0.9777778







