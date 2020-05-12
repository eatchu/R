



# x변수 선택

nrow(c_m_w) #1023473
x<-sample(1:nrow(c_m_w),10000)
cmw <- c_m_w[x,]
length(c_m_w)

library(MASS)
model2 <- lm(QUANTITY~.,data=cmw)
step <- stepAIC(model2,direction='both')
# stepAIC : AIC 지수값으로 알맞는 x변수선택을 해줌
#           AIC가 가장 낮은 변수를 선택하는것이 타당

# 변수 전부 다 사용





# 다중공선성 확인
library(car)
str(cmw)
cmw <- c_m_w
cmw$SELL_DATE <- as.numeric(cmw$SELL_DATE)
cmw$BRAND <- as.numeric(cmw$BRAND)
cmw$MENU <- as.numeric(cmw$MENU)
cmw$GENDER <- as.numeric(cmw$GENDER)

fit <- lm(formula=QUANTITY~.,data=cmw)

vif(fit) #분산팽창요인 정보 제공 함수 : 대략 1
sqrt(vif(fit))>2 #2넘는것 없음 


# 변수 간의 상관계수 구하기
cor(cmw[-6]) # 변수간의 상관계수 보기(종속변수 제외) 

# 가격-브랜드 / 성별-생년월일 정도가 상관관계를 보임 하지만 크진 않음



# 다중 회귀분석
result.lm <- lm(formula=QUANTITY ~ ., data=cmw)
summary(result.lm)
# 유의수준 - 브랜드, 가격, 성별, 나이, 온도
plot(result.lm)
res <- result.lm$residuals
res
# (1) 표준화 (scale)
res_scale <- scale(res) #mean=0, sd=1
# (2) mse 
mse <- mean(res_scale**2)
mse #0.999999
shapiro.test(res) #p-value = 1.816e-11




test_customer <- read.csv(file.choose())
test_meal <- read.csv(file.choose())
test <- merge(test_customer,test_meal,by='CUSTOMER_ID')
str(test)

tmw <- merge(x=test,y=weather,by="SELL_DATE")
str(tmw)


tmw$TEMP <- str_replace_all(tmw$TEMP, "\\[\\'평균기온:", "")
tmw$TEMP <- str_replace_all(tmw$TEMP, "\\'\\]", "")
tmw$PRECIP= str_replace_all(tmw$PRECIP, "\\[\\'일강수량:", "")
tmw$PRECIP= str_replace_all(tmw$PRECIP, "\\'\\]", "")
tmw$PRECIP= str_replace_all(tmw$PRECIP, "\\[\\]", "0")

str(tmw)
tmw$TEMP <- as.numeric(tmw$TEMP)
tmw$PRECIP <- as.numeric(tmw$PRECIP)

tmw$SELL_DATE <- as.numeric(tmw$SELL_DATE)
tmw$BRAND <- as.numeric(tmw$BRAND)
tmw$MENU <- as.numeric(tmw$MENU)
tmw$GENDER <- as.numeric(tmw$GENDER)

y_pred <- predict(result.lm, tmw)
y_pred
length(y_pred)
y_true <- tmw$QUANTITY
length(y_true)

# 1. MSE 표준화
error <- y_true - y_pred
mse <- mean(error**2)
mse #0.009817525

# 2. 상관계수 r
r <- cor(y_true, y_pred)
r #0.04324513


# 시각화 평가
plot(y_true,col='blue',type='l',label='y true')
points(y_pred,col='red',type='l',label='y pred')
legend('topleft',cex=2,legend=c('y true','y pred'), col=c('blue','red'),pch='--')




# 변수 축출 후 다중 회귀 분석
names(cmw)
cmw2 <- cmw[c(3,5,6,7,8,9)]
cor(cmw2)
str(cmw2)

result.lm2 <- lm(formula=QUANTITY ~ ., data=cmw2)
summary(result.lm2)
# 유의수준 - 브랜드, 가격, 성별, 나이, 온도
plot(result.lm2)
res <- result.lm2$residuals
res
# (1) 표준화 (scale)
res_scale <- scale(res) #mean=0, sd=1
# (2) mse 
mse <- mean(res_scale**2)
mse #0.999999
shapiro.test(res) #p-value = 1.816e-11


names(tmw)
tmw2 <- tmw[c(3,4,5,7,8,9)]


y_pred <- predict(result.lm2, tmw2)
y_pred
length(y_pred)
y_true <- tmw2$QUANTITY
length(y_true)

# 1. MSE 표준화
error <- y_true - y_pred
mse <- mean(error**2)
mse #0.00981797

# 2. 상관계수 r
r <- cor(y_true, y_pred)
r #0.04292904















