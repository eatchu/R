#chap01_Basic

#수업내용
#1. 패키지와 세션
#2. 패키지 사용법
#3. 변수와 자료형
#4. 기본함수와 작업공간

#<1> 패키지와 세션
dim(available.packages())
#[1] 15297(우리가 사용가능한 패키지수)    17(하나의 패키지에 대한 정보)
15305-15297
15305-15247
getOption('max.print')
58*17

#session
sessionInfo() #세션 정보 제공 
#R환경,OS환경(WINDOW AS WELL), 
#다국어 정보 환경(locale 언어의 출처), 자동사용가능한 package

#주요 단축키
#script 실행 : ctrl + enter
#script save : ctrl + s
#자동완성 : ctrl + space bar
#여러줄 주석 처리 : ctrl + shift + c --나 이거 안돼

# a <-10
# b<-20
# c<-a+b
# print(c)


#<2> 패키시 사용법 : package = function + dataset

#1) 패키지 설치
#기존버전 패키지를 현재위치에 설치하는 방법
install.packages('다운받고자하는 URL')
install.packages('다운받고자하는 URL',repos=NULL)
install.packages('stringr') #따움표 안쓰면 오류 발생
#패키지(1) + 의존성 패키지(3) : stringr과 관련된 패키지도 자동으로 설치됨


#2) 패키치 설치 경로 확인
.libPaths()
# [1] "C:/Users/user/Documents/R/win-library/3.6"
# [2] "C:/Program Files/R/R-3.6.2/library"      

#3) im memory : 패키지 -> memory로 upload
library(stringr) #install과 다르게 여기선 따움표 필요없음, 써도됨
library(help='stringr')

#memory 로딩된 패키지 확인
search()

str_extract('홍길동35이순신45','[가-힣]{3}') #[1] "홍길동"
str_extract_all('홍길동35이순신45','[가-힣]{3}') #[1] "홍길동" "이순신"

#4) 패키지 삭제
remove.packages('stringr') #해당 폴더에 들어가서 물리적 삭제도 가능


# 패키지 설치 error 해결법

#1. 최초 패키지 설치하는데 오류가 날 경우
# -관리자 모드 실헝
# -r studio에 오른쪽 마우스 -> 자세히 -> 관리자권한으로 실행 클릭
#2. 기존 패키지 설치하는데 오류가 날 경우
# -1) remove.packages('패키지이름') :일차적으로 패키지 삭제
# -2) rebooting : 컴퓨터 자체를 끔
# -3) install.packages('패키지이름') :다시 설치해줌


#<3> 변수와 자료형

# 1) 변수 : 메모리 이름 

# 2) 변수 작성 규칙 및 특징

#  - 첫자는 영문, 두번째는 숫자 or 특수문자(_ , . ) 
#   ex) score2020, score_2020, score.2020 등 변수이름 설정가능
#  - 예약어, 함수명 사용 불가
#  - 대소문자 구분
#   ex) num , NUM 은 각각 다른변수로 저장됨
#  - 변수 선언시 type 선언이 없음
#   ex) score = 90(R) VS int score = 90 (c) c언어의 경우 정수형 타입 설정

#  - 변수는 가장 최근 값으로 변경됨
#  - R의 모든 변수는 객체 (object)


var1 <- 0 # var1 = 0 동일 
#var1을 변수로 설정하고 값을 넣으면 0을 저장한 객체가 됨

var1 <- 1
var1 #print(var1) 동일

var2 <- 10
var3 <- 20

var1; var2; var3 #한번에 여러값 볼때 ;를 사용


# 색인(index[]) : 메모리에 저장된 저장위치를 의미
#var1 -> [1] 1  변수 하나에 1개가 저장되었다는 의미

var3 <- c(10,20,30,40,50)
var3[5] #50이라는 값을 얻게됨
print(var3) #[1] 10 20 30 40 50

#대소문자
num=200
NUM=100

print(NUM==num) #FALSE

# = 는 값을 넣는 식
# == 값이 같다는 것을 뜻하는 식


#object.member
member.id = 'hong'
member.name = "홍길동"
member.age = 35


member.id
member.name;member.age

#scala(0차원) vs vector(1차원) vs metrix(2차원) 
score <- 95 #scala 값이 하나 들어감
scores <- c(85,75,95,100) #vector 값이 여러개 들어감

score # 95
scores # 85 75 95 100 값이 일정한 방향을 가지고 구성되는 변수


# 3) 자료형(data type) : 숫자형, 문자형, 논리형 

int <- 100 
float <- 125.23
string <- "대한민국"
bool <- TRUE #T 
bool <- FALSE #F

#mode 함수 : 변수의 자료형을 알려주는 함수
mode(int) #numeric
mode(float) #numeric
mode(string) #character
mode(bool) #logical
mode('endml')

# is.xxxx : 변수가 가지고 있는 자료형의 참과 거짓을 알려줌
#내가 가지고 있는 변수가 연산이 가능한지 미리 확인해불 수 있는 함수
is.numeric(int) #TRUE
is.numeric(string) #FALSE
is.character(string) #TRUE
is.logical(bool) #TRUE

datas <- c(84,85,62,NA,45)
datas
is.na(datas) #FALSE FALSE FALSE TRUE FALSE


# 4) 자료형 변환 함수 : ppt 20

#(1) 문자형 -> 숫자형
x <- c(10,20,30,'40') #vector 변수 생성
x #[1] "10" "20" "30" "40"
mode(x) # character
# 변수안에 문자형이 하나만 들어가도 전체가 문자형으로 변함
# 벡터변수 특징 : 자료형을 하나만 가짐. 문자형과 숫자형을 따로 저장X
x*2 # x형이 문자형으로 저장되어있기 때문에 error 발생

as.numeric(x) #[1] 10 20 30 40
x <- as.numeric(x)
x # 숫자형으로 바뀜 -> 연산과 그래프 생성이 가능해짐 (plor,*,+,- 등)
x*2
plot(x) #index=x축 값=y축

#(2) 요인형(factor) : 변주형 변수 생성 (집단변수) category
# 문자형 생성
gender <- c('남','여','남','여','여')
mode(gender) #character
plot(gender) #error

# 문자형 -> 요인형 변환
as.factor(gender) #[1] 남 여 남 여 여 Levels: 남 여
fgender <- as.factor(gender)
mode(fgender) #numeric
plot(fgender)

gender
fgender #집단끼리 category로 묶여있음 (범주) -> ㄱ~ㅎ 순으로 만들어짐

#객체에 대한 구조를 보여주는 함수
str(fgender) # Factor w/ 2 levels "남","여": 1 2 1 2 2 : 문자를 숫자로 해석
             # level에 대한 순서는 변환 가능 but 어떻게 바꿈??
             # 같은 영역끼리 묶기 위해 숫자로 나타낸거지 진짜 숫자의 의미는 아님
             # 더미변수 : 컴퓨터가 처리하게끔 만들어준 변수
             #            숫자에 의미가 없는 숫자형
str(gender) # chr [1:5] "남" "여" "남" "여" ...

# mode vs class
class(fgender) #"factor" -> 자료구조 확인
mode(fgender) #"numeric" -> 자료형 확인



# 숫자형 변수 생성
x <- c(4,2,4,2)
mode(x)
plot(x)
hist(x)
# 숫자형 -> 요인형 
f <- as.factor(x) 
f #[1] 4 2 4 2 Levels: 2 4
plot(f)
hist(f) #error
str(f) #Factor w/ 2 levels "2","4": 2 1 2 1
# 요인형 -> 숫자형
x2 <- as.numeric(f)
x2 #2 1 2 1 더미변수의 숫자로 바뀜 즉 레벨값
   # 원래의 숫자가 복원이 안됨


#그럼 원래 숫자처럼 복원하려면 어떻게 해야할까? 
# 숫자형 변수 생성
x <- c(4,2,4,2)
mode(x)
# 숫자형 -> 요인형 
f <- as.factor(x) 
f #[1] 4 2 4 2 Levels: 2 4
str(f) #Factor w/ 2 levels "2","4": 2 1 2 1
# 요인형 -> 문자형
c <- as.character(f)
c #"4" "2" "4" "2"
# 문자형 -> 숫자형
x2 <- as.numeric(c)
x2 #4 2 4 2

#즉 숫자-요인-숫자 = 값이 바뀜
# 숫자-요인-문자-숫자 = 원래 값이 나옴



#<4> 기본함수와 작업공간

# 1) 기본함수 : 특별한 전제조건 없이 바로 사용가능한 함수 
#               세션에서 확인한 7개의 함수들을 말함
# stats  graphics  grDevices  utils  datasets  methods  base 

#패키지 도움말
library(help='stats') #stats 패키지에 대한 정보를 볼 수 있음
library(help='utils')

#함수 도움말
help(sum)
?sum
?mean

# sum : 합을 구하는 함수
x<-c(10,20,30,NA)
sum(x) #[1] NA
sum(x,na.rm=TRUE) #[1] 60 -> na.rm=TRUE NA의 값을 지우겠다는 의미
sum(20,30,5,NA,na.rm=TRUE) #50
sum(1:5) #15 -> 1에서 5까지 일련의 수를 하나씩 더함

# mean() : 평균을 구하는 함수 -> 괄호안에 변수만 넣을 수 있음
mean(10,20,30,NA,na.rm=TRUE) #10 - 오류는 안뜨지만 정확한 값이 안나옴
mean(x,na.rm=TRUE) #20 - 알맞는 값이 나옴


# 2) 기본 데이터셋
data() #데이터셋 확인
data(Nile)
Nile  
length(Nile) #100
mode(Nile) #numeric
class(Nile)
plot(Nile)
hist(Nile)


# 3) 작업공간
getwd() #작업하고 있는 공간의 위치를 알려줌
setwd("c:/아이티윌/2_rwork/part-i")
getwd()

emp <- read.csv("emp.csv", header = T)
emp




