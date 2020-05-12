


################################################
## chap16_3_XGboost
################################################




# xgboost vs randomForest
# - xgboost : boosting 방식 
# - randomForest : bagging 방식 

# 1. package install
install.packages("xgboost")
library(xgboost)
#library(help="xgboost") agaricus.train/test 패키지 자체에서 제공하는 기본데이터

# 2. dataset load/search
data(agaricus.train)
data(agaricus.test)

train <- agaricus.train
test <- agaricus.test

str(train)
train$data@Dim #6513  126
# Java, Python : object.member
# R : object@member
# $data : x변수 : [6513,126] : 2차원 matrix
# $label : y변수 : num[1:6513] : 1차원(vector)


train$data
train$label
table(train$label)
#   0    1 
# 3373 3140 


str(test)
# $data : x변수 : [1611,126] : 2차원 matrix
# $label : y변수 : num[1:1611] : 1차원(vector)



# 3. xgboost matrix 생성 : 객체 정보 확인  
# xgb.DMatrix(data=x, label=y)
dtrain <- xgb.DMatrix(data = train$data, label = train$label) # x:data, y:label
dtrain  #dim: 6513 x 126

?xgboost
#We will train decision tree model using the following parameters:
# •objective = "binary:logistic": we will train a binary classification model ;
# "binary:logistic" : y변수 이항 
# •max_depth = 2: the trees won't be deep, because our case is very simple ;
# tree 구조가 간단한 경우 : 2
# •nthread = 2: the number of cpu threads we are going to use;
# cpu 사용 수 : 2
# •nrounds = 2: there will be two passes on the data, the second one will enhance the model by further reducing the difference between ground truth and prediction.
# 실제값과 예측값의 차이를 줄이기 위한 반복학습 횟수 
# •eta = 1 : eta control the learning rate 
# 학습률을 제어하는 변수(Default: 0.3) 
# 숫자가 낮을 수록 모델의 복잡도가 높아지고, 컴퓨팅 파워가 더많이 소요
# 부스팅 과정을보다 일반화하여 오버 피팅을 방지하는 데 사용
# •verbose = 0 : no message
# 0이면 no message, 1이면 성능에 대한 정보 인쇄, 2이면 몇 가지 추가 정보 인쇄

# 4. model 생성 : xgboost matrix 객체 이용  
xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)


# 5.  학습된 model의 변수(feature) 중요도/영향력 보기 
import <- xgb.importance(colnames(train$data), model = xgb_model)
import #y에 가장 영향력 있는 x변수 확인
xgb.plot.importance(importance_matrix = import)


# 6. 예측치
pred <- predict(xgb_model, test$data)
range(pred)  #0.01072847 0.92392391
y_pred <- ifelse(pred>=0.5,1,0)
y_true <- test$label

tab <- table(y_true,y_pred)
tab
#         y_pred
# y_true    0    1
#       0 813   22
#       1  13  763


# 7. 모델평가
# 1) 분류정확도
acc <- (813+763)/sum(tab)
acc #0.9782744
# 2) 평균 오차
err <- mean(as.numeric(pred>=0.5) != y_true)
err #0.02172564


# 8. model save & load
# 1) model file save
setwd('C:/IITT/2_Rwork/output')
xgb.save(xgb_model,'xgboost.model')

rm(list=ls()) # 메모리 지움

# 2) model load(memory loading)
xgb_model2 <- xgb.load('xgboost.model')
xgb_model2

pred <- predict(xgb_model2, test$data)
range(pred) #0.01072847 0.92392391 앞서 사용한 예측치와 동일








##########################
## iris 적용 : y 이항분류
##########################

iris_df <- iris # 데이터 복제

# 1. y 변수 binary (Species : factor -> number)
iris_df$Species <- ifelse(iris_df$Species == "setosa", 0, 1) # 0=50개, 1=100개
head(iris_df) 
str(iris_df) # $ Species : num 
str(iris) #  $ Species : Factor w/ 3 levels
table(iris_df$Species) 


# 2. data set 생성 
idx <- sample(nrow(iris_df), 0.7*nrow(iris_df))
train <- iris_df[idx, ]
test <- iris_df[-idx, ]

head(train)
head(test)
tail(test)


# 3. xgb.DMatrix 생성
# train : x변수(4개) -> matrix, y변수(Species) -> vector
train_x <- as.matrix(train[-5]) #y변수 제거 -> matrix
train_y <- train$Species #x변수 제거 -> vector

dim(train_x) # 105 4
length(train_y) # 105

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtrain #dim: 105 x 4


# 4. model 생성 : xgboost matrix 객체 이용   
iris_model <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 0)
iris_model # object info

# 5. 학습된 model의 변수(feature) 중요도/영향력 보기 
import <- xgb.importance(colnames(train_x),
                         model = iris_model)
import #Petal.Length 
xgb.plot.importance(importance_matrix = import)



# 6. 예측치
# test : x변수(4개) -> matrix, y변수(Species) -> vector
test_x <- as.matrix(test[-5]) #y변수 제거 -> matrix
test_y <- test$Species #x변수 제거 -> vector
pred <- predict(iris_model, test_x)
range(pred)  #0.06118078 0.94868654
y_pred <- ifelse(pred>=0.5,1,0)
y_true <- test_y

tab <- table(y_true,y_pred)
tab
#         y_pred
# y_true    0    1
#       0  16    0
#       1   0   29


# 7. 모델평가
# 1) 분류정확도
acc <- (16+29)/sum(tab)
acc #1
# 2) 평균 오차
err <- mean(as.numeric(pred>=0.5) != y_true)
err #0




#############################
### iris 적용 : y 다항분류
### objective = 'multi:softmax' : y 다항분류
###-> num_class = n
### -> 첫번째 class = 0(number)
#############################

?xgboost
# objective 속성
# objective = 'reg:squarederror' : 연속형(default)
# objective = 'binary:logistic' : y 이항분류
# objective = 'multi:softmax' : y 다항분류
# -> num_class = n
# -> 첫번째 class = 0(number)


# 1. y 변수 binary (Species : factor -> number)
iris_df <- iris
iris_df$Species <- ifelse(iris_df$Species == "setosa", 0, 
                          ifelse(iris_df$Species=='versicolor',1,2)) 
str(iris_df) # $ Species : num 
str(iris) #  $ Species : Factor w/ 3 levels
table(iris_df$Species) # 50 50 50



# 2. data set 생성 
idx <- sample(nrow(iris_df), 0.8*nrow(iris_df))
sort(idx)
train <- iris_df[idx, ]
test <- iris_df[-idx, ]



# 3. xgb.DMatrix 생성
# train : x변수(4개) -> matrix, y변수(Species) -> vector
train_x <- as.matrix(train[-5]) #y변수 제거 -> matrix
train_y <- train$Species #x변수 제거 -> vector

test_x <- as.matrix(test[-5])
test_y <- test$Species

dim(train_x) # 120 4
length(train_y) # 120

dmtrix <- xgb.DMatrix(data = train_x, label = train_y)


# 4. model 생성 : xgboost matrix 객체 이용   
iris_model2 <- xgboost(data = dmtrix, 
                       max_depth = 2, 
                       eta =0.5,
                       nthread = 2,
                       nrounds = 2,
                       objective = "multi:softmax", num_class=3,
                       verbose = 0)
iris_model2 # object info


# 5. prediction
pred <- predict(iris_model2,test_x)
range(pred)

tab <- table(test_y,pred)
tab
#         pred
# test_y   0  1  2
#       0 13  0  0
#       1  0 10  1
#       2  0  0  6

acc <- (tab[1,1]+tab[2,2]+tab[3,3])/sum(tab)
acc #0.9666667
err <- mean(pred != test_y)
err #0.03333333


# 6. 학습된 model의 변수(feature) 중요도/영향력 보기 
import <- xgb.importance(colnames(train_x),
                         model = iris_model2)
import #Petal.Length 
xgb.plot.importance(importance_matrix = import)


#############################
### iris 적용 : y 연속형
### objective = 'reg:squarederror' : 연속형(default)
#############################

# 1. train/test
idx <- sample(nrow(iris_df), 0.7*nrow(iris_df))
sort(idx)
train <- iris_df[idx, ]
test <- iris_df[-idx, ]

# 2. xgboost model 생성
# y : 1번칼럼
# x : 2~4번칼럼
iris_df <- iris[-5]
train_x <- as.matrix(train[-1])
train_y <- train$Sepal.Length
test_x <- as.matrix(test[-1])
test_y <- test$Sepal.Length

imtrix <- xgb.DMatrix(data=train_x, label=train_y)
imodel <- xgboost(data = imtrix, 
                   max_depth = 2, 
                   eta =1,
                   nthread = 2,
                   nrounds = 3,
                   objective = "reg:squarederror", #기본옵션
                   verbose = 0)
imodel
# iter train_rmse - 반복할수록 오차가 적어짐
# 1   0.527295
# 2   0.405677
# 3   0.367338

# 3. 정확도 구하기
pred <- predict(imodel,test_x)
true <- test_y
range(pred) #4.762721 7.517762

# 표준화 (O)
err <- pred-true
mse <- mean(err**2)
mse # 0.118072 -> 0값에 가까울수록 좋은 모델

# 표준화 (scale)
err_scale <- scale(err) #mean=0, sd=1
# (2) mse 
mse <- mean(err_scale**2)
mse #0.9777778
shapiro.test(pred) #p-value = 0.002372


# 표준화 (X)
cor(true,pred) # 0.9227486



# 4. 중요변수구하기
import <- xgb.importance(colnames(train_x),
                         model = imodel)
import #Petal.Length 
xgb.plot.importance(importance_matrix = import)
# Petal.Length 0.9146464
# Sepal.Width 0.0853536



