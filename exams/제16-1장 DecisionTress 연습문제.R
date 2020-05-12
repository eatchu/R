#####################################
## 제16-1장 DecisionTress 연습문제 
#####################################

library(rpart) # rpart() : 분류모델 생성
library(rpart.plot) # prp(), rpart.plot() : rpart 시각화
library(rattle) # fancyRpartPlot() : node 번호 시각화 

# 01. Spam 메시지 데이터 셋을 이용하여 DT 분류모델을 생성하고.
# 정분류율, 오분류율, 정확률을 구하시오. 

# 실습 데이터 가져오기
sms_data = read.csv(file.choose()) # sms_spam_tm.csv
dim(sms_data) # [1] 5558(row) 6824(word)
sms_data$sms_type
str(sms_data)
prop.table(table(sms_data$sms_type)) #0.8655991 0.1344009 


# 1. train과 test 데이터 셋 생성 (7:3)
n <- sample(1:nrow(sms_data),nrow(sms_data)*0.7)
train <- sms_data[n,]
test <- sms_data[-n,]
nrow(train) #3890
nrow(test) #1668

# 2. model 생성 - train_sms
train_sms <- rpart(sms_type~., data=train)
prp(train_sms) 
rpart.plot(train_sms) 
fancyRpartPlot(train_sms) 


# 3. 예측치 생성 - test_sms
test_sms <- predict(train_sms, test, type='class')
true_sms <- test$sms_type

# 4. 분류정확도 : 정분류율, 오분류율, 정확률, 재현율 
table(test_sms,true_sms)
table(test_sms)
table(true_sms)
#정분율
acc <- (1408+150)/nrow(test)
acc #0.9340528
#오분율
nacc <- (80+30)/nrow(test)
nacc #0.06594724
#재현률 - 실제값 대상
recall <- 150/(150+80)
recall #0.6521739
#정확률 - 예측치 대상
precision <- 150/(150+30)
precision #0.8333333
#불균형비율
f1 <- 2*((recall*precision)/(recall+precision))
f1 # 0.7317073



# 02. weather 데이터를 이용하여 다음과 같은 단계별로 의사결정 트리 방식으로 분류분석을 수행하시오. 

# 조건1) y변수 : RainTomorrow, x변수 : Date와 RainToday 변수 제외한 나머지 변수로 분류모델 생성 
# 조건2) 모델의 시각화를 통해서 y에 가장 영향을 미치는 x변수 확인 
# 조건3) 비가 올 확률이 50% 이상이면 ‘Yes Rain’, 50% 미만이면 ‘No Rain’으로 범주화

# 단계1 : 데이터 가져오기
library(rpart) # model 생성 
library(rpart.plot) # 분류트리 시각화 

weather = read.csv(file.choose(), header=TRUE) # weather.csv
str(weather)

# 단계2 : 데이터 샘플링
weather.df <- weather[, c(-1,-14)]
idx <- sample(1:nrow(weather.df), nrow(weather.df)*0.7)

weather_train <- weather.df[idx, ]
weather_test <- weather.df[-idx, ]

# 단계3 : 분류모델 생성
weather_model <- rpart(RainTomorrow~.,data=weather_train)
weather_model
# Humidity
# Pressure
# Sunshine
# WindGustDir
# MinTemp


# 단계4 : 분류모델 시각화 - 중요변수 확인 
rpart.plot(weather_model)
fancyRpartPlot(weather_model)


# 단계5 : 예측 확률 범주화('Yes Rain', 'No Rain') 
pred <- predict(weather_model, weather_test)
weather_pred <- ifelse(pred[,2]>=0.5,'yes rain','no rain')
weather_true <- weather_test$RainTomorrow
weather_true <- ifelse(weather_true=='Yes','yes rain','no rain')

# 단계6 : 혼돈 matrix 생성 및 분류 정확도 구하기
table(weather_pred)
table(weather_true)
weather_tab <- table(weather_true,weather_pred)
#                 weather_pred
# weather_true  no rain yes rain
#      no rain     83        7
#     yes rain     10       10
# 분류정확도 : 정분류율, 오분류율, 정확률, 재현율 
# 정분율
acc <- (83+10)/nrow(weather_test)
acc #0.8454545
# 오분율
nacc <- (7+10)/nrow(weather_test)
nacc #0.1545455
# 정확률 - 예측치 대상
precision <- 10/(10+7)
precision #0.5882353
# 재현율 - 실제값 대상
recall <- 10/(10+10)
recall #0.5
# 불균형 비율
f2 <- 2*((recall*precision)/(recall+preci구글sion))
f2 #0.5405405




############################################
### 교차 검정
############################################

# 단계 1 : k겹 교차검정을 위한 샘플링
install.packages('cvTools')
library(cvTools)
?cvFolds
# cvFolds(n : 전체의 길이 , K = 5 : 균등하게 몇등분으로 쪼갤것인지, R = 1,   
#         type = c("random", "consecutive", "interleaved"))

# iris : n=150, k=3(d1=50,d2=50,d3=50)
cross <- cvFolds(n=nrow(iris),K=3,R=1,type='random')
cross # Fold : dataset번호(d1,d2,d3), Index : row번호
str(cross)


# set1
d1 <- cross$subsets[cross$which==1,1]
# set2
d2 <- cross$subsets[cross$which==2,1]
# set3
d3 <- cross$subsets[cross$which==3,1]

length(d1) #50
length(d2) #50
length(d3) #50

library(rpart)

K <- 1:3
R <- 1
ACC <- numeric()
cnt <- 1
data(iris)
  
for(r in R){
  for(k in K){
    idx <- cross$subsets[cross$which==k,r]
    test <- iris[idx,]
    train <- iris[-idx,]
    model <- rpart(Species~.,data=train)
    pred <- predict(model,test,type='class')
    true <- test$Species
    tab <- table(true,pred)
    ACC[cnt] <- (tab[1,1]+tab[2,2]+tab[3,3])/sum(tab)
    cnt <- cnt+1
  }
}

ACC


#############################
## titanic 변수
#############################
# titanic3.csv 변수 설명
#'data.frame': 1309 obs. of 14 variables:
#1.pclass : 1, 2, 3등석 정보를 각각 1, 2, 3으로 저장
#2.survived : 생존 여부. survived(생존=1), dead(사망=0)
#3.name : 이름(제외)
#4.sex : 성별. female(여성), male(남성)
#5.age : 나이
#6.sibsp : 함께 탑승한 형제 또는 배우자의 수
#7.parch : 함께 탑승한 부모 또는 자녀의 수
#8.ticket : 티켓 번호(제외)
#9.fare : 티켓 요금
#10.cabin : 선실 번호(제외)
#11.embarked : 탑승한 곳. C(Cherbourg), Q(Queenstown), S(Southampton)
#12.boat     : (제외)Factor w/ 28 levels "","1","10","11",..: 13 4 1 1 1 14 3 1 28 1 ...
#13.body     : (제외)int  NA NA NA 135 NA NA NA NA NA 22 ...
#14.home.dest: (제외)

titanic <- read.csv(file.choose())
str(titanic)

# 조건1) 6개 변수 제외 -> subset 생성
# 조건2) survived int -> factor (생존=1,사망=0)
# 조건3) train vs test
# 조건4) 생존여부를 결정하는 가장 중요한 변수 확인
# 조건5) 분류정확도 확인 


tita <- titanic[-c(3,8,10,12,13,14)]
str(tita)
tita$survived <- factor(tita$survived)
x <- c('0','1')
tita$survived <- factor(tita$survived, levels=x)

t <- sample(1:nrow(tita),nrow(tita)*0.7)
train <- tita[t,]
test <- tita[-t,]

tita_model <- rpart(survived~.,data=train)
tita_model
rpart.plot(tita_model)
fancyRpartPlot(tita_model)
# 생존여부를 결정하는 가장 중요한 변수 : sex (성별이 남자인지 여자인지)


pred <- predict(tita_model,test,type='class')
true <- test$survived

tab <- table(true,pred)
#         pred
# true   0   1
#     0 227  15
#     1  61  90

acc <- (tab[1,1]+tab[2,2])/sum(tab)
acc #0.8066158



