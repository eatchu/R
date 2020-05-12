
# chap16_2_RandomForest


##################################################
#randomForest
##################################################
# 결정트리(Decision tree)에서 파생된 모델 
# 랜덤포레스트는 앙상블 학습기법을 사용한 모델
# 앙상블 학습 : 새로운 데이터에 대해서 여러 개의 Tree로 학습한 다음, 
# 학습 결과들을 종합해서 예측하는 모델(PPT 참고)
# DT보다 성능 향상, 과적합 문제를 해결


# 랜덤포레스트 구성방법(2가지)
# 1. 결정 트리를 만들 때 데이터의 일부만을 복원 추출하여 트리 생성 
#  -> 데이터 일부만을 사용해 포레스트 구성 
# 2. 트리의 자식 노드를 나눌때 일부 변수만 적용하여 노드 분류
#  -> 변수 일부만을 사용해 포레스트 구성 
# [해설] 위 2가지 방법을 혼용하여 랜덤하게 Tree(학습데이터)를 구성한다.

# 새로운 데이터 예측 방법
# - 여러 개의 결정트리가 내놓은 예측 결과를 투표방식(voting) 방식으로 선택 


install.packages('randomForest')
library(randomForest) # randomForest()함수 제공 


############################################
###### 분류 tree
############################################

data(iris)

# 1. 랜덤 포레스트 모델 생성 
# 형식) randomForest(y ~ x, data, ntree=500, mtry=2)
model = randomForest(Species~., data=iris)  
names(model)

# node 분할에 사용하는 x변수 갯수
mtry <- sqrt(4) # 범주형 : 2
p <- 14
mtrt <- 1/3*p # 연속형 : 4~5


# 2. 파라미터 조정 300개의 Tree와 4개의 변수 적용 모델 생성 
# ntree : tree의 갯수 mtry : 몇개의 변수를 가지고 tree를 분리했는지
model = randomForest(Species~., data=iris, 
                     ntree=300, mtry=4, na.action=na.omit )
model


# 3. 최적의 파리미터(ntree, mtry) 찾기
# - 최적의 분류모델 생성을 위한 파라미터 찾기

ntree <- c(400, 500, 600)
mtry <- c(2:4)

# 2개 vector이용 data frame 생성 
param <- data.frame(n=ntree, m=mtry)
param


for(i in param$n){ # 400,500,600
  cat('ntree = ', i, '\n')
  for(j in param$m){ # 2,3,4
    cat('mtry = ', j, '\n')
    model = randomForest(Species~., data=iris, 
                         ntree=i, mtry=j, 
                         na.action=na.omit )    
    print(model)
  }
}


# 4. 중요 변수 생성  
model3 <- randomForest(Species ~ ., data=iris, 
                      ntree=500, mtry=2, 
                      importance = TRUE,
                      na.action=na.omit )
model3 
importance(model3)
# MeanDecreaseAccuracy : 분류정확도 개선에 기여하는 변수
# MeanDecreaseGini : 노드 불순도(불확실성) 개선에 기여하는 변수
# 두 수치가 클수록 변수의 기여도가 크다 (y에 영향력이 있다)


varImpPlot(model3) # 변수에 대한 그래프 확인 가능


#########################################
###### 회귀 tree
#########################################
#data = boston
#crim : 도시 1인당 범죄율 
#zn : 25,000 평방피트를 초과하는 거주지역 비율
#indus : 비상업지역이 점유하고 있는 토지 비율  
#chas : 찰스강에 대한 더미변수(1:강의 경계 위치, 0:아닌 경우)
#nox : 10ppm 당 농축 일산화질소 
#rm : 주택 1가구당 평균 방의 개수 
#age : 1940년 이전에 건축된 소유주택 비율 
#dis : 5개 보스턴 직업센터까지의 접근성 지수  
#rad : 고속도로 접근성 지수 
#tax : 10,000 달러 당 재산세율 
#ptratio : 도시별 학생/교사 비율 
#black : 자치 도시별 흑인 비율 
#lstat : 하위계층 비율 
#medv(y) : 소유 주택가격 중앙값 (단위 : $1,000)


library(MASS)
data("Boston")
str(Boston)

ntree <- 500
p <- 13
mtry <- 1/3*p
br <- randomForest(medv~., data=Boston, 
                   ntree=500, mtry=mtry, importance=TRUE)
br
names(br)
importance(br)
varImpPlot(br)
pred <- br$predicted #예측값
true <- br$y #실제값

# 표준화 (O)
err <- pred-true
mse <- mean(err**2)
mse # 9.672766 .=. Mean of squared residuals: 9.672766

# 표준화 (X)
cor(true,pred) # 0.9438731


# model
# 분류 tree : confusion matrix
# 회귀 tree : MES, cor 



###########################################
###### 실습
###########################################

titanic <- read.csv(file.choose())
str(titanic)


tita <- titanic[-c(3,8,10,12,13,14)]
str(tita)
tita$survived <- factor(tita$survived)
x <- c('0','1')
tita$survived <- factor(tita$survived, levels=x)



ntree <- 500
mtry <- round(sqrt(7))
tr <- randomForest(survived~., data=tita, 
                   ntree=500, mtry=mtry, importance=TRUE,
                   na.action=na.omit)
tr
varImpPlot(tr)
# sex, pclass, fare, age




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


####################################
### entropy : 불확실성 척도
####################################
# tree model에서 중요변수 선정 기준


# 1. x1 앞면 x2 뒷면 : 불확실성이 가장 높은 경우
x1=0.5
x2=0.5

e1 <- -x1*log2(x1) - x2*log2(x2)
exp(1) # 2.718282
e1 # 1


# 2. x1=0.8, x2=0.2 
x1=0.8
x2=0.2
e2 <- -x1*log2(x1) - x2*log2(x2)
e2 # 0.7219281 


# 3. x1=0.9, x2=0.1 
x1=0.9
x2=0.1
e3 <- -x1*log2(x1) - x2*log2(x2)
e3 # 0.4689956



