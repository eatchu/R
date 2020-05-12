##########################
## 제17-3장 SVM 연습문제 
##########################

# 문1) 기상데이터를 다음과 같이 SVM에 적용하여 분류하시오. 
# 조건1> 포물라 적용 : RainTomorrow ~ .  
# 조건2> kernel='radial', kernel='linear' 각 model 생성 및 평가 비교 

# 1. 파일 가져오기 
weatherAUS = read.csv(file.choose()) #weatherAUS.csv
weatherAUS = weatherAUS[ ,c(-1,-2, -22, -23)] # 칼럼 제외 
str(weatherAUS)

# 2. 데이터 셋 생성 
set.seed(415)
idx = sample(1:nrow(weatherAUS), 0.7*nrow(weatherAUS))
training_w = weatherAUS[idx, ]
testing_w  = weatherAUS[-idx, ]
length(training_w$RainTomorrow) #25816
length(testing_w$RainTomorrow) #11065

summary(weatherAUS)

training_w <- na.omit(training_w)
testing_w <- na.omit(testing_w)

# 3. 분류모델 생성 : kernel='radial'(cost,gamma), kernel='linear'(cost)
model_r <- svm(RainTomorrow~., data=training_w, na.action=na.omit)
model_l <- svm(RainTomorrow~., data=training_w, kernel='linear', na.action=na.omit)

# 4. 분류모델 평가 
pred_r <- predict(model_r,testing_w)
pred_l <- predict(model_l,testing_w)
length(pred_r)
summary(pred_r)
summary(testing_w$RainTomorrow)
length(testing_w$RainTomorrow)
table(testing_w$RainTomorrow,pred_r)
table(testing_w$RainTomorrow,pred_l)

# 문2) 문1에서 생성한 모델을 tuning하여 최적의 모델을 생성하시오.
tun_w <- tune.svm(RainTomorrow~.,data=training_w,
                  gamma=params,cost=params) 
tun_w

best_model <- svm(RainTomorrow~.,data=training_w,
                  gamma=0.01, cost=10)
pred <- predict(best_model, testing_w)
table(testing_w$RainTomorrow,pred)





