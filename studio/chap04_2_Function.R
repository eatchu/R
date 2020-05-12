# chap04_2_Function

# 1. 사용자 정의함수

# 형식) 
# 함수명 <- function([인수]){
#   실행문
#   실행문
#   [return값]
# }

# function : 백터 입력 가능

# 1) 매개변수 없는 함수
f1 <- function(){
  cat('f1 함수')
} #function()안에 인수 삽입 안함

f1() #f1 함수

# 2) 매개변수 있는 함수
f2 <- function(x){
  x2 <- x^2
  cat('x2=',x2)
}

f2(c(3,10,5)) 

# 3) 리턴 있는 함수
f3 <- function(x,y){
  add <- x+y
  return(add)
}

add
f3(1,2)


num <- 1:10
tot_func <- function(x){
  tot <- sum(x)
  avg <- mean(x)
  return(c(tot,avg))
}


tot_func(num) # 55.0  5.5


# 문제1) calc 함수를 정의하기
# 100+20=120
# 100-20=80
# 100*20=2000
# 100/20=5

calc <- function(x,y){
  a <- x+y
  b <- x-y
  c <- x*y
  d <- x/y
  cat(x,'+',y,'=',a,'\n')
  cat(x,'-',y,'=',b,'\n')
  cat(x,'*',y,'=',c,'\n')
  cat(x,'/',y,'=',d,'\n')
  calc_df <- data.frame(a,b,c,d)
  return(calc_df)
}

calc(100,20)


# 구구단 출력
gugu <- function(dan){
  cat('***',dan,'단***\n')
  for(i in 1:9){
    cat(dan,'*',i,'=',dan*i,'\n')
  }
}


state <- function(fname,data){
  switch(fname,
         SUM = sum(data),
         AVG = mean(data),
         VAR = var(data),
         SD = sd(data))
}

x <- c(10,20,30,40)
state('SUM',x)
state('AVG',x)


# 결측치(NA) 처리 함수
na <- function(x){
  #(1)NA 완전 제거
  x1 <- na.omit(x)
  cat(x1,'\n')
  cat('x의 평균값=',mean(x1),'\n')
  #(2)NA 평균 대체
  x2 <- ifelse(is.na(x),mean(x,na.rm=T),x)
  cat(x2,'\n')
  cat('x의 평균값=',mean(x2),'\n')
  #(3)NA 0으로 대체
  x3 <- ifelse(is.na(x),0,x)
  cat(x3,'\n')
  cat('x의 평균값=',mean(x3),'\n')
}


x <- c(10,5,NA,4.2,6.3,NA,7.5,8,10)
mean(x,na.rm=T)
na(x)



###################################
### 몬테카를로 시뮬레이션 
###################################
# 현실적으로 불가능한 문제의 해답을 얻기 위해서 난수의 확률분포를 이용하여 
# 모의시험으로 근사적 해를 구하는 기법

# 동전 앞/뒤 난수 확률분포 함수 
?runif
?numeric


coin <- function(n){
  r <- runif(n, min=0, max=1)
  #print(r) # n번 시행 
  
  result <- 0
  for (i in 1:n){
    if (r[i] <= 0.5)
      result[i] <- 0 # 앞면 
    else 
      result[i] <- 1 # 뒷면
  }
  return(result)
}

coin(1) #random값이 나옴


# 몬테카를로 시뮬레이션 
montaCoin <- function(n){
  cnt <- 0
  for(i in 1:n){
    cnt <- cnt + coin(1) # 동전 함수 호출 
  }
  result <- cnt / n
  return(result)
}

montaCoin(10000)

# 2. R의 주요 내장함수

# 1) 기술통계함수 

vec <- 1:10          
min(vec)                   # 최소값
max(vec)                   # 최대값
range(vec)                  # 범위
mean(vec)                   # 평균
median(vec)                # 중위수
sum(vec)                   # 합계
prod(vec)                  # 데이터의 곱
1*2*3*4*5*6*7*8*9*10
summary(vec)               # 요약통계량 

n <- rnorm(1000)
mean(n)
sd(n)
sd(rnorm(10))      # 표준편차 구하기
factorial(5) # 팩토리얼=120
sqrt(49) # 루트


sort(x)  # 벡터 정렬 (단, 원래의 값을 바꾸지는 않음) 
order(x)  # 벡터의 정렬된 값의 인덱스를 보여줌 
rank(x)  # 벡터의 각 원소의 순위를 알려줌 
summary(x)  # 데이터에 대한 기본적인 통계 정보 요약 
table(x)  # 데이터 빈도수
abs(x) # 절대값
which.min(x)
which.max(x) # 벡터 내의 최소값과 최대값의 인덱스




install.packages('RSADBE')
library(RSADBE)
library(help='RSADBE')
data(Bug_Metrics_Software)
str(Bug_Metrics_Software)
Bug_Metrics_Software
class(Bug_Metrics_Software)
#소프트웨어 발표 전
Bug_Metrics_Software[,,1]
#행단위 합계 : 소프트웨어별 버그 수
rowSums(Bug_Metrics_Software[,,1])
#열단위 합계 : 버그별 버그 수 
colSums(Bug_Metrics_Software[,,1])
#행/열단위 평균
rowMeans(Bug_Metrics_Software[,,1])
colMeans(Bug_Metrics_Software[,,1])
#소프트웨어 발표 후
Bug_Metrics_Software[,,2]
#어레이에 면 추가하기
#[,,3] = before - after
bug <- Bug_Metrics_Software
side <- c('Before','After','Test')
side2 <- c('JDT','PDE','Equinox','Lucene','Mylyn')
side3 <- c('Bugs','NT.Bugs','Major','Critical','H.Priority')
head(bug)
bug.new <- array(bug,dim=c(5,5,3),
                 dimnames=list(software=side2,bugs=side3,side)) #면추가하기
# x <- array(1:10,c(행,열,면)) 어레이 생성법
dim(bug.new)
bug.new # 면 구조만 추가한거라 1면과 같은 데이터가 생성되어 있음
bug.new[,,3] <- bug[,,1]-bug[,,2]
bug.new
#컬럼이름은 왜 사라질까
#행 대표 이름 열 대표 이름 까지 추가하는법 -> 성공

rm(bug.new)


# 2) 반올림 관련 함수 
x <- c(1.5, 2.5, -1.3, 2.5)
round(mean(x)) # 반올림
ceiling(mean(x)) # x보다 큰 정수 
floor(mean(x)) # 1보다 작은 정수 


# 3) 난수 생성과 확률분포

#(1) 정규분포를 따르는 난수 - 연속확률분포(실수형)
# 형식) rnorm(randomnormal)(n,mean=0,sd=1)
n <- 1000
r <- rnorm(n,mean=0,sd=1)
mean(r) #0.006002574
sd(r) #0.9985189
hist(r) #좌우 대칭성을 보임

#(2) 균등분포를 따르는 난수 - 연속확률분포(실수형)
# 형식) runif(n,min=0,max=1) : min-max사이 안에서 확률분포를 균등하게 생성
r2 <- runif(n,min=0,max=1)
hist(r2) #모든값이 균등한 갯수로 생성
mean(r2)

#(3) 이항분포를 따르는 난수 - 이산확률분포(정수형)
# 형식) rbinom(n,size(시행횟수),prob(시행시 성공 확률))
set.seed(123) #배열을 지정해줌 -> 동일한 난수 생성
n <- 10
r3 <- rbinom(n,1,0.5)#10개의 값중에 1이 나오는 빈도는 1/2
r3 <- rbinom(n,10,0.5)
#(4) sample (replace=F : 중복없음)
sample(10:20,5)#10~20범위내에서 5개 임의 추출
sample(10:20,5,replace=T) #중복값 나올 수 있음
sample(c(10:20, 50:100), 10)
i <-10:20
j <- 50:100
c(i, j)
#홀드아웃방식
#train(70%)/test(30%) 데이터셋
dim(iris) # 150x5
idx <- sample(nrow(iris),nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]
dim(test)



# 4) 행렬연산 내장 함수
x <- matrix(1:9,nrow=3,byrow=T)
x
y <- matrix(1:3,nrow=3)
y
x;y
# 행렬곱의 전제조건
#(1) x,y모두 행렬
#(2) x(열)=y(행) : 수일치 해야함
z <- x%*%y
z <- y%*%x #error 전제조건 불일치

ncol(x) # 열의 수 nrow(x) # 행의 수 
t(x) # 전치행렬
cbind(x,y) # 열을 더할 때 이용되는 함수 
rbind(x,y) # 행을 더할 때 이용되는 함수 
diag(x) # 대각행렬 det(x) # 행렬식 
apply(x, m, fun) # 행 또는 열에 함수 적용 
x %*% y # 두 행렬의 곱 
solve(x) # 역 행렬 
svd(x) # Singular Value Decomposition 
qr(x) # QR Decomposition (QR 분해) 
eigen(x) # Eigenvalues(고유값) 
chol(x) # choleski decomposition(Choleski 분해) 













