################################
## 제16-3장 XGBboost 연습문제 
################################

# 01. UniversalBank.csv 데이터셋을 이용하여 다음과 같이 XGBoost에 적용하여 분류하시오. 
# 조건1> 포물라 : formula <- Personal.Loan ~.
# 조건2> 모델 성능 평가
# 조건3> 중요변수 보기

# 대출 수락 or 거절 prediction
setwd("c:/IITT/2_Rwork/Part-IV")
Data = read.csv('UniversalBank.csv',  stringsAsFactors = F)
str(Data)
Data = Data[c(-1, -5)] # 1, 5번 칼럼 제외 


# Personal.Loan -> y변수(대출여부) 
str(Data)

# 1. data set 생성 
idx <- sample(nrow(Data), nrow(Data)*0.7)
train <- Data[idx, ]
test <- Data[-idx, ]
dim(train) # 3500   12
dim(test) # 1500  12

# 2. xgb.DMatrix 생성 : data(x):matrix, label(y):vecor 
train_mat <- as.matrix(train[-8]) # matrix
train_mat[1,]
test_mat <- as.matrix(test[-8])
test_mat[1,]
# y변수 : vector 
train_y <- train$Personal.Loan
test_y <- test$Personal.Loan

# 3. model 생성 : xgboost matrix 객체 이용   
bank <- xgb.DMatrix(data = train_mat, label = train_y) 
bank_model <- xgboost(data = bank, max_depth = 2, eta = 1, nthread = 2, 
                       nrounds = 2, objective = "binary:logistic", verbose = 0)
bank_model

# 4. prediction 
pred <- predict(bank_model,test_mat)
range(b_pred) #0.04227788 0.96893603


# 5. cut off 적용 
b_pred <- ifelse(pred>=0.5,1,0)
b_true <- test_y


# 6. confusion matrix
tab <- table(b_true,b_pred)
#            b_pred
# b_true     0    1
#       0 1333    8
#       1   19  140


# 7. 모델 성능평가 : Accuracy
acc <- (tab[1,1]+tab[2,2])/sum(tab)
acc #0.982

wonder <- mean(as.numeric(b_pred==0) != b_true)


err <- mean(as.numeric(pred>=0.5) != b_true)
err #0.018
err2 <- (tab[1,2]+tab[2,1])/sum(tab)
err2

table(b_pred)
table(b_true)


# 8. 중요변수 보기(model 비교) 
xport <- xgb.importance(colnames(train_mat), model = bank_model)
xport
xgb.plot.importance(importance_matrix = xport)
#Education
#Income
#Family
#CCAvg







