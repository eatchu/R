# chap04_1_control
# <실습> 산술연산자 
num1 <- 100 # 피연산자1
num2 <- 20  # 피연산자2
result <- num1 + num2 # 덧셈
result # 120
result <- num1 - num2 # 뺄셈
result # 80
result <- num1 * num2 # 곱셈
result # 2000
result <- num1 / num2 # 나눗셈
result # 5

result <- num1 %% num2 # 나머지 계산
result # 0

result <- num1^2 # 제곱 계산(num1 ** 2)
result # 10000
result <- num1^num2 # 100의 20승
result # 1e+40 -> 1 * 10의 40승과 동일한 결과

# <실습> 관계연산자 
# (1) 동등비교 
boolean <- num1 == num2 # 두 변수의 값이 같은지 비교
boolean # FALSE
boolean <- num1 != num2 # 두 변수의 값이 다른지 비교
boolean # TRUE

# (2) 크기비교 
boolean <- num1 > num2 # num1값이 큰지 비교
boolean # TRUE
boolean <- num1 >= num2 # num1값이 크거나 같은지 비교 
boolean # TRUE
boolean <- num1 < num2 # num2 이 큰지 비교
boolean # FALSE
boolean <- num1 <= num2 # num2 이 크거나 같은지 비교
boolean # FALSE

# <실습> 논리연산자(and, or, not, xor)
logical <- num1 >= 50 & num2 <=10 # 두 관계식이 같은지 판단 
logical # FALSE
logical <- num1 >= 50 | num2 <=10 # 두 관계식 중 하나라도 같은지 판단
logical # TRUE

logical <- num1 >= 50 # 관계식 판단
logical # TRUE
logical <- !(num1 >= 50) # 괄호 안의 관계식 판단 결과에 대한 부정
logical # FALSE

x <- TRUE; y <- FALSE
xor(x,y) # [1] TRUE
x <- TRUE; y <- TRUE
xor(x,y) # FALSE


########################
###### 1. 조건문 #######
########################



# 1) if(조건식) : 산술, 관계, 논리연산자 : 백터 입력 불가능
x <- 10
y <- 5
z <- x*y

# 형식1) if(조건식){참}else{거짓}
if (z>=20){
  cat('z는 20보다 크다')
}else {
    cat('z는 20보다 작다')}
# 형식2) if(조건식1){참}else if(조건식2){참} else{거짓}
score <- scan()
score # 85
grade <- ''

if (score>=90){
  grade <- 'A'
} else if (score>=80){
  grade <- 'B'
} else if (score>=70){
  grade <- 'C'
} else if (score>=60){
  grade <- 'D'
} else{
  grade <- 'F'
}
cat('점수는',score,'이고, 등급은',grade)

# 문제1) 키보드로 임의숫자를 입력받아서 짝수/홀수 판별하기
num <- scan()

if (num%%2==0){
  print('짝수')
} else{
  print('홀수')
}

# 문제2) 주민번호를 사용하요 성별 판별하기
library(stringr)
jumin <- '123456-7234567'

#str_sub(jumin,8,8)
# 1 or 3 : 남자
# 2 or 4 : 여자
# 그 외 : 잘못된 주민번호 양식

if (str_sub(jumin,8,8)==1 | str_sub(jumin,8,8)==3){
  print('남자')
} else if (str_sub(jumin,8,8)==2 | str_sub(jumin,8,8)==4){
  print('여자')
} else{
  print('잘못 입력된 주민번호 입니다.')
}


# 2) ifelse(조건,참,거짓) : 3항 연산자 기능 :백터 입력 가능
# vector 데이터 입력 가능 -> vector 데이터 출력
score <- c(78,85,95,45,65)
ifelse(score>=60, "합격","불합격") #우수

excel <- read.csv(file.choose())
str(excel)
q5 <- excel$q5
length(q5) #402
table(q5)
# 1   2   3   4   5 
# 8  81 107 160  46 

# 5점 척도 -> 범주형 변수로 변환
q5_re <- ifelse(q5>=3,'큰값','작은값')
table(q5_re)
# 작은값  큰값 
#  89     313 


# datas <- c(84,85,62,NA,45)
# datas
# is.na(datas) #FALSE FALSE FALSE TRUE FALSE

x <- c(75,85,42,NA,85)
# NA -> 평균 대체
ifelse(is.na(x),mean(x,na.rm=T),x)
# NA -> 0으로 대체
ifelse(is.na(x),0,x)

#여러개의 ifelse 사용
score <- c(85,100,65,10,45,72,88)
ifelse(score>=90,'A',
       ifelse(score>=80,'B',
              ifelse(score>=7,'C',
                     ifelse(score>=60,'D','F'))))


# 3) switch()
# 형식) switch(비교구문, 실행구문1, 실행구문2, 실행구문3)
switch("id", age=105, name="홍길동", id="hong",
       pwd="1234") 


# 4) which()
# 조건식에 만족하는 위치 반환
# 괄호내의 조건에 해당하는 인덱스 값을 출력
name <- c('kim','lee','choi','park')
which(name=='choi') #3

library(MASS)
data("Boston")
str(Boston) #506x14

name <- names(Boston) #boston이 가지고 있는 칼럼명 입력
length(name) #14

# x(독립변수), y(종속변수) 선택
which(name=='medv') #14
y_col <- which(name=='medv')
y <- Boston[y_col]
head(y)
x <- Boston[-y_col] #[14]칼럼을 제외한 나머지 데이터 입력
head(x)


# 문제3) iris 데이터셋을 대상으로 x변수(1~4), y변수(5) 선택
str(iris) #150x5
name <- names(iris)
y_ <- which(name=='Species')
y<- iris[y_]
x <- iris[-y_]



#############################
######## 2. 반복문 ##########
#############################

# 1) for(변수 in 열거형객체){실행문}

num <- 1:10
# 열거형 객체로 10회 반복을 의미

for(i in num){
  cat('i=',i,'\n')
}
# i= 1 
# i= 2 
# i= 3 
# i= 4 
# i= 5 
# i= 6 
# i= 7 
# i= 8 
# i= 9 
# i= 10 

for(i in num){
  if(i %% 2 != 0){
    cat(i,'\n')
  }else{
    next
  }
}

# 문제4) 키보드로 5개 정수를 입력받아서 짝수/홀수를 구분
num <- scan()

for(i in num){
  if(i%%2==0){
    cat(i,'는 짝수\n')
  }else{
    cat(i,'는 홀수\n')
  }
}

# 문제5) 홀수의 합과 짝수의 합 출력하기
num <- 1:100
even <- 0 #짝수
odd <- 0 #홀수
cnt <- 0 #카운터 변수

for(i in num){
  cnt=cnt+1
  if(i%%2==0){
    even=even+i
  }else{
    odd=odd+i
  }
}
cat('짝수의합=',even,'홀수의합=',odd,'누적값',cnt)
# 짝수의합= 2550 홀수의합= 2500 누적값 100

kospi <- read.csv(file.choose())
str(kospi) #247x6
head(kospi)

#칼럼=칼럼-칼럼
kospi$diff <- kospi$High-kospi$Low
str(kospi) #247x7
mean(kospi$diff)

#diff 평균 이상이면 '평균이상', 아니면 '평균미만'
row <- nrow(kospi) #247 행의 갯수
diff_result=''
for(i in 1:row){
  if(kospi$diff[i]>=mean(kospi$diff)){
    diff_result[i]='평균이상'
  }else{
    diff_result[i]='평균미만'
  }
}
table(diff_result)


# 이중 for문 
for(변수1 in 열거형){
  for(변수2 in 열거형){
    실행문
  }
}

#구구단
for(i in 2:9){
  for(j in 1:9){
    cat(i,'*',j,'=',(i*j),'\n')
  }
}




for(i in 2:9){
  cat('***',i,'단***\n',
      file='C:/IITT/2_Rwork/output/gugu.txt',
      append=T)
  
  for(j in 1:9){
    cat(i,'*',j,'=',(i*j),'\n',
        file='C:/IITT/2_Rwork/output/gugu.txt',
        append=T)
  }
  cat('\n',
      file='C:/IITT/2_Rwork/output/gugu.txt',
      append=T)
}

#append 계속 입력되는 데이터를 추가할것인지 말것인지

gugu.txt <- readLines('C:/IITT/2_Rwork/output/gugu.txt')
#파일을 줄단위로 읽기, 한글이 읽히고 따움표가 생김

gugudan <- read.csv(file.choose())
gugudan
library(stringr)
dim(gugudan)


# 2) while(조건식){실행문}
i = 0 
while(i<5){
  cat('i=',i,'\n')
  i=i+1
}


x <- c(2,5,8,6,9)
n <- length(x)
y <- 0
i <- 0
while(i < n){
  i <- i+1
  y[i] <- x[i]^2
}
y














