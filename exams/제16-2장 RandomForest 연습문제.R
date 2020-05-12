####################################
## 제16-2장 RandomForest 연습문제 
####################################

# 01. 400개의 Tree와 4개의 분류변수를 파라미터로 지정하여 모델을 생성하고, 
#       분류정확도를 구하시오.
#  조건> 1,2,22,23 칼럼을 제외하여 데이터 셋 구성 

weatherAUS = read.csv(file.choose()) # weatherAUS.csv 
weatherAUS = weatherAUS[ ,c(-1,-2, -22, -23)]
str(weatherAUS)
# 'data.frame':	36881 obs. of  20 variables:
wr <- randomForest(RainTomorrow~.,data=weatherAUS,ntree=400,mtry=4,
                   na.action=na.omit, importance=TRUE)
wr
varImpPlot(wr)
wr$importance
# error rate: 13.95%
#       No    Yes  class.error
# No  12604   822  0.06122449
# Yes  1602  2350  0.40536437

#Humidity3pm
#Sunshine


# 02. 변수의 중요도 평가를 통해서 가장 중요한 변수를 확인하고, 시각화 하시오. 


t <- sample(1:nrow(weatherAUS),nrow(weatherAUS)*0.7)
train <- weatherAUS[t,]
test <- weatherAUS[-t,]

wr2 <- rpart(RainTomorrow~., data=train)
fancyRpartPlot(wr2)
wr2
#Humidity3pm
#Rainfall

wp <- predict(wr2,test,type='class')
wt <- test$RainTomorrow
tab <- table(wt,wp)
acc <- (7872+1069)/sum(tab)
acc #0.8217831






