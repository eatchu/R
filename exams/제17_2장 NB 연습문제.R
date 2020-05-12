##########################
## 제2-2장 NB 연습문제 
##########################

# 문) Spam 메시지 데이터 셋을 이용하여 NB 분류모델을 생성하고,
# 분류정확도와 F 측정치를 구하시오. 
library(e1071)

# 실습 데이터 가져오기(TM에서 전처리한 데이터)
setwd("C:/IITT/2_Rwork/Part-IV")
sms_data <- read.csv('sms_spam_tm.csv')
dim(sms_data) # [1] 5558(row) 6824(word) - 6157
str(sms_data)

colnames(sms_data)[1] <- "sms_type"

sms_data$sms_type[1:5]
length(sms_data$sms_type) #5558
names(sms_data[1])


# X 칼럼 제외 
sms_data.df <- sms_data[-1] # 행번호 제외 
head(sms_data.df)
str(sms_data.df) # 5558 obs. of  6823 variables:

# 1. train과 test 데이터 셋 생성 (7:3)
idx <- sample(1:nrow(sms_data),nrow(sms_data)*0.7)
train <- sms_data[idx,]
test <- sms_data[-idx,]

# 2. model 생성 - train_sms
model <- naiveBayes(train[-1], train$sms_type) 
model

# 3. 예측치 생성 - test_sms
pred <- predict(model,test)
true <- test$sms_type
tab <- table(true,pred)

# 4. 정분류율(Accuracy)
acc <- (tab[1,1]+tab[2,2])/sum(tab)


# 5. F measure(f1 score)










