
# DecisionTree

library(rpart) # rpart() : 분류모델 생성 
library(rpart.plot) # prp(), rpart.plot() : rpart 시각화
library('rattle') # fancyRpartPlot() : node 번호 시각화 

names(cmw)
cmw3 <- cmw[-2]

model = rpart(QUANTITY~., data=cmw) 
model

# 분류모델 시각화 - rpart.plot 패키지 제공 
prp(model) # 간단한 시각화   
rpart.plot(model) # rpart 모델 tree 출력
fancyRpartPlot(model) # node 번호 출력(rattle 패키지 제공)


# 분류모델 평가  
pred <- predict(model, tmw) # 비율 예측 
pred <- predict(model, tmw, type="class") # 분류 예측 

# 1) 분류모델로 분류된 y변수 보기 
table(pred)

# 2) 분류모델 성능 평가 
table(pred, tmw$QUANTITY)


acc <- (13+16+12)/nrow(test)
acc #0.9111111






