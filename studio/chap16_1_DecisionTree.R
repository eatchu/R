# chap16_1_DecisionTree



library(rpart) # rpart() : 분류모델 생성 
install.packages("rpart.plot")
library(rpart.plot) # prp(), rpart.plot() : rpart 시각화
install.packages('rattle')
library('rattle') # fancyRpartPlot() : node 번호 시각화 


# 단계1. 실습데이터 생성 
data(iris)
set.seed(415)
idx = sample(1:nrow(iris), 0.7*nrow(iris))
train = iris[idx, ]
test = iris[-idx, ]
dim(train) # 105 5
dim(test) # 45  5

table(train$Species)

# 단계2. 분류모델 생성 
# rpart(y변수 ~ x변수, data)
model = rpart(Species~., data=train) # iris의 꽃의 종류(Species) 분류 
model

#1) root 105 68 setosa : setosa제외하고 68개 존재. setosa가 가장 많은 비율차지
#3) Petal.Length>=2.45 68 33 virginica




# 분류모델 시각화 - rpart.plot 패키지 제공 
prp(model) # 간단한 시각화   
rpart.plot(model) # rpart 모델 tree 출력
fancyRpartPlot(model) # node 번호 출력(rattle 패키지 제공)


# 단계3. 분류모델 평가  
pred <- predict(model, test) # 비율 예측 
pred <- predict(model, test, type="class") # 분류 예측 

# 1) 분류모델로 분류된 y변수 보기 
table(pred)

# 2) 분류모델 성능 평가 
table(pred, test$Species)


acc <- (13+16+12)/nrow(test)
acc #0.9111111


##################################################
# Decision Tree 응용실습 : 암 진단 분류 분석
##################################################
# "wdbc_data.csv" : 유방암 진단결과 데이터 셋 분류

# 1. 데이터셋 가져오기 
setwd('C:/IITT/2_Rwork/Part-IV')
wdbc <- read.csv('wdbc_data.csv', stringsAsFactors = FALSE)
str(wdbc) # 569  32

# 2. 데이터 탐색 및 전처리 
wdbc <- wdbc[-1] # id 칼럼 제외(이상치) 
head(wdbc)
head(wdbc[, c('diagnosis')], 10) # 진단결과 : B -> '양성', M -> '악성'

# 목표변수(y변수)를 factor형으로 변환 
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
wdbc$diagnosis[1:10]

# 3. 정규화  : 서로 다른 특징을 갖는 칼럼값 균등하게 적용 
normalize <- function(x){ # 정규화를 위한 함수 정의 
  return ((x - min(x)) / (max(x) - min(x)))
}

# wdbc[2:31] : x변수에 해당한 칼럼 대상 정규화 수행 
wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize))
wdbc_x
summary(wdbc_x) # 0 ~ 1 사이 정규화 
class(wdbc_x) # [1] "data.frame"
nrow(wdbc_x) # [1] 569

wdbc_df <- data.frame(wdbc$diagnosis, wdbc_x)
dim(wdbc_df) # 569  31
head(wdbc_df)

# 4. 훈련데이터와 검정데이터 생성 : 7 : 3 비율 
idx = sample(nrow(wdbc_df), 0.7*nrow(wdbc_df))
wdbc_train = wdbc_df[idx, ] # 훈련 데이터 
wdbc_test = wdbc_df[-idx, ] # 검정 데이터 

names(wdbc_train)

# 5. rpart 분류모델 생성 
wdbc_model <- rpart(wdbc.diagnosis~. , data=wdbc_train)
wdbc_model
rpart.plot(wdbc_model)
fancyRpartPlot(wdbc_model)



# 6. 분류모델 평가  


wdbc_pred <- predict(wdbc_model,wdbc_test,type='class')
table(wdbc_pred)
wdbc_true <- wdbc_test$wdbc.diagnosis
table(wdbc_true)
tab <- table(wdbc_true,wdbc_pred)

#         wdbc_pred
# wdbc_true  B  M
#         B 99  6
#         M 14 52

# 정분율
acc <- (99+52)/nrow(wdbc_test)
acc #0.8830409
# 오분율
nacc <- (6+14)/nrow(wdbc_test)
nacc #0.1169591
# 특이도 (B / M)
B <- 99/(99+6)
B #0.9428571
M <- 52/(52+14)
M #0.7878788
# 재현율
B #0.9428571
M #0.7878788
# 정확율 - 예측치 기준
BB <- 99/(99+14)
BB #0.8761062
MM <- 52/(52+6)
MM #0.8965517
# 불균형 비율
f1 <- 2*((B*BB)/(B+BB))
f1 #0.9082569
f2 <- 2*((M*MM)/(M+MM))
f2 #0.8387097








