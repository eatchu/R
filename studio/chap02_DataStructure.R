# chap02_DataStructure

# 자료구조의 유형 (5)





# 1. vector 자료구조
# - 동일한 자료형을 갖는 1차원 배열구조
# - 백터 생성 함수 : c(), seq(), rep()

#(1) c()
x<-c(1,3,5,7)
y<-c(3,5,6)
length(x) #원소가 4개
length(y) #3

# 집합관련 함수
union(x,y) #합집합 x+y : 1,3,5,6,7
setdiff(x,y) #차집합 x-y : 1,7 
setdiff(y,x) #차집합 y-x : 6
intersect(x,y) #교집합 : 3,5
#백터 변수 유형
num <- 1:5 #1~5까지 일련의 값 - :의 의미는 연속, :을 사용하면 값이 항상 연속으로 나옴
num <- c(-10:5)
num <- c(1,2,3,'4')
#백터 원소 이름 지정
text <- c('hong','lee','kang')
age <- c(35,45,55)
names(age) <- text
age #hong  lee kang 
    # 35   45   55 
str(age) 
# Named num [1:3] 35 45 55 --> 실제 데이터
# - attr(*, "names")= chr [1:3] "hong" "lee" "kang" --> 레이블

#(2) seq() 
#기본값은 by로 들어감
help('seq') 
num <- seq(1,10, by=2) #1~10까지 2씩 증가하는 수 넣기
                      #by가 없다면 1~10까지 일련의 숫자가 다 들어감
num2 <- seq(10,1,by=-2) #역수 10 8 6 4 2
num <- seq(1,10,length.out=4)

#(3) rep()
#기본값은 times로 들어감
?rep
rep(1:3,times=3) #123123123
rep(1:3,each=3)#111222333
rep(1:4, each = 2, times = 3, len=20)


#색인(index) : 저장위치
#형식) object[n]
a <- 1:50 
a # 1~50
a[19] 
a[10:19]
a[30,35] #error - [행,열]의 2차원형식이기 때문에 값이 안나옴
a[c(30,35)]

#함수 이용
length(a) #50
a[10:length(a)-5] #5~45 : -5 연산자가 앞 뒤 숫자에 전부 영향을 줌
a[10:(length(a)-5)] #10~45 : 괄호사용
a[seq(2,length(a),by=2)] #짝수만 출력
a[seq(1,length(a),by=2)] #홀수만출력

#특정원소 제외(-)
a[-c(20:30)]
a[-c(10,20,25:35)]

#조건식(boolean)
a[a>=10&a<=30] #10~30 : & = and
a[a>=10|a<=30] #1~50 : | = or
a[!(a>=10)] #1~9 : 10이하의 값이 나옴






# 2.Matrix 
# -행열구조로 데이터가 저장된 자료구조 
# -같은수를 가진 백터 여러개가 행단위로 묶임
# -동일한 자료형을 갖는 2차원 배열구조
# -생성함수 : matrix(), rbind(), cbind()
# -처리함수 : apply()


# 1) 매트릭스 생성함수
# (1) matrix 
#data
m1 <- 1:5 # 행 :1 열 : 5  vector
m1 <- matrix(1:5) #행 :5 열 :1  matrix  
dim(m1) #aox 5x1 구조의 dimension
mode(m1) #numeric - 자료형
class(m1) #matrix - 자료구조
#nrow 행의 갯수 설정
m2 <- matrix(data=c(1:9),nrow=3) #행 : 3 열 : 3
m2 <- matrix(data=c(1:9),nrow=9) #행 : 9 열 : 1
#byrow 
m2 <- matrix(data=c(1:9),nrow=3,byrow=F) #열우선 : 세로축을 기준으로 숫자 배열
m2 <- matrix(data=c(1:9),nrow=3,byrow=T) #행우선 : 가로축을 기준으로 숫자 배열
dim(m2) #3x3
#ncol 열의 갯수 설정
m2 <- matrix(data=c(1:9),ncol=3,byrow=T)

# (2) rbind : 변수를 행단위로 묶어줌
x <- 1:5
y <- 6:10
x # x 1,2,3,4,5
y # y 6,7,8,9,10
m3 <- rbind(x,y)
m3 # x집합 : 첫번째 행에 배치, y집합 : 두번째 행에 배치 (가로)
dim(m3) #2x5

# (3) cbind : 변수를 열단위로 묶어줌
m4 <- cbind(x,y)
m4 # x집합 : 첫번째 열에 배치, y집합 : 두번째 행에 배치 (세로)
dim(m4) #5x2


#[행,] [,열] 
#ex) [2,] 2행의 값들이 출력
#ex) [,3] 3열의 값들이 출력
#ex) [2,4] 2행의 4열 값이 출력
#ex) [1:2,2:3] 1,2행의 2,3열 값 출력
#ex) [c(1,3),2:3] 1,3행의 2,3열 값 출력 
#   - 연속되지 않은 행이나 열을 구하고싶을때 사용


m5 <- matrix(1:9,nrow=3,ncol=3,byrow=T)

#특정 행/열 색인
m5[1,] # 1,2,3
m5[,1] # 1,4,7
m5[1,2:3] # 2,3
m5[2:3,1:2] # 4,5/7,8
m5[c(1,3),2:3] # 2,3/8,9

#(-)속성
m5[-2,] #2행 제거 # 1,2,3/7,8,9
m5[,-3] #3열 제거 # 1,2/4,5/7,8
m5[,-c(1,3)] #1,3열 제거 - 2열만 출력됨 # 2/5/8

# 열 = 칼럼 = 변인
# 행 

#열/행 이름 지정하기 colnames() & rownames
colnames(m5) <- c('one','two','three')
m5 # 가로에 이름 지정됨
m5[,'one'] #열 이름을 사용할때는 한가지만 사용가능 # 1/4/7
m5['one',] #행에는 이름이 지정되지 않아서 error
m5[,('one':'two')] #error

rownames(m5) <- c('one','two','three')
m5 # 세로에 이름 지정됨
m5['one',] # 1,2,3
#열/행에 모두 이름을 지정했을때
m5['two','one'] # 4



# 2)broadcast 연산
# - 작은차원 -> 큰차원 늘어나서 연산

# (0) x변수에 matrix생성
x <- matrix(1:12, nrow=4, ncol=3, byrow=T)
dim(x) #4x3

# (1) scala(0) vs matrix(2)
0.5*x #x(matrix)의 모든 원소에 0.5가 곱해짐

# (2) vactor(1) vs matrix(2) : 두개의 원소 갯수가 서로 배수관계에 있어야함
y <- 10:12 #vector #10,11,12
y + x 
#vector가 어떤 순서로 matrix가 되는지 확인해보기
y <- 10:15 #10,11,12,13,14,15
y + x

# (3) matrix(2) vs matrix(2) : shape
x + x
x - x
x * x

# (4) 전치행렬 : 행->열, 열->행
x
t(x)


# 3) 처리함수 : apply()
help(apply)
#apply(X, MARGIN(행/열), FUN(함수), ...)
# 1=행단위 2=열단위
apply(x,1,sum)#행단위 합계
apply(x,2,mean) #열단위 평균
apply(x,1,var) #행단위 분산
apply(x,1,sd) #행단위 표준편차




# 3. Array 자료구조
# - 동일한 자료형을 갖는 3차원 배열구조
# - 생성 함수 : array()

# 1차원 -> 3차원
?array
arr <- array(1:12,c(3,2,2)) #dim=c(행,열,면) -> Array만들땐 열우선만 가능
arr #3x2의 매트릭스가 2개 존재 #3x2x2=12
mode(arr) #numeric
class(arr) #array

#색인 index
#변수[행,열,면]
arr[,,1] #1,2,3,4,5,6
arr[1,1,2] #7


iris3
dim(iris3) #50,4,3 = 총 600개의 값

iris3[10:20,1:2,1] #1면+1,2열+10~20행 = 22개값



# 4. Data.frame
# - '열 단위 서로 다른 자료형'을 갖는 2차원 배열구조
# - 무조건 칼럼단위로 생성되기 때문에 행/열을 바꾸려면 함수를 사용해야함 : t()
# - 생성 함수 : data.frame()
# - 처리 함수 : apply() -> 행렬 처리

# 1) vector 이용 : 길이가 같아야함
no <- 1:3
name <- c('홍길동','이순신','유관순')
pay <- c(250,350,200)

emp <- data.frame(no,name,pay)
emp <- data.frame(번호=no,이름=name,급여=pay) #별칭부여 

#한번에 입력도 가능
emp2 <- data.frame(번호=1:3,이름=c('홍길동','이순신','유관순'),급여=c(250,350,200))

dim(emp) #3x3
class(emp) #data.frame
mode(emp) #list : 두개 이상의 자료형을 포함한다는 뜻


m <- matrix( c(1,"hong",150, 
               2, "lee", 250, 
               3, "kim", 300) ,3 ,by=T) 
memp <- data.frame(m)   


# 자료참조 : 칼럼명 참조 or index 참조
# 형식1) object$column
# x$y : x(data.frame)에서 y칼럼을 가져오게하는 함수
pay <- emp$급여 #pay에 emp속 급여에 대한 원소들의 값이 부여됨
sum(pay) #250+350+200=800
mean(pay) #266.6667
# 형식2) object[row,column]
emp_row <- emp[c(1,3),] # -> emp[-2,]

# 2) csv(comma seperate value), text file
setwd('c:/IITT/2_Rwork/Part-i')
getwd()

emp_txt <- read.table('emp.txt',header=T, sep='') 
class(emp_txt) #data.frame
emp_csv <- read.csv('emp.csv') 
class(emp_csv) #data.frame

# [실습]
sid <- 1:3 #이산형 : 정수로 이루어짐
score <- c(90,85,83) #연속형 : 실수도 포함가능
gender <- c('M','F','M') #범주형 : 카테고리 형성 가능

student <- data.frame(sid, score, gender, stringsAsFactors = T)
stringsAsFactors = F #문자형
stringsAsFactors = T #요인형
# 자료구조 보기
str(student)
# 'data.frame':	3 obs(행의갯수). of  3 variables(변수):
# $ sid   : int  1 2 3 : 정수(이산형)
# $ score : num  90 85 83 : 실수가능(연속형)
# $ gender: Factor w/ 2 levels "F","M": 2 1 2 : (범주형)



# 특정 칼럼 -> vector
score <- student$score
mean(score)
sum(score)
var(score)
#표준편차
sd(score)
sqrt(var(score))

#산포도 : 분산, 표준편차

#(1)모집단에 대한 분산, 표준편차
#분산= sum(x-산술평균)^2/n
#표준편차 = sqrt(분산) :sqrt=루트

#(1)표본에 대한 분산, 표준편차 <- R 함수
#분산 = sum(x-산술평균)^2/n-1
#표준편차 = sqrt(분산)

score # 90 85 83
avg <- mean(score) # scala
diff <- (score-avg)^2 # vector - scala
VAR <- sum(diff) / (length(score)-1) # 13
sqrt(VAR)


# 5. List
# - key와 value 한쌍으로 자료가 저장됨
# - key를 통해서 값(values)을 참조함
# - key는 중복불가, 값은 중복가능
# - 다양한 자료형(숫자,문자,논리)과 자료구조(1,2,3차원)를 가짐 
# - 생성함수 : list()

# 1) key 생략 : [key=value, key2=value2 ~ ]
lst <- list('lee','이순신',35,'hong','홍길동',30)
# [[1]] -> 기본키(default key)
# [1] -> 'lee'
lst[6] # key+value 같이 나옴
lst[[5]] #'홍길동'

# 2) key = value
lst2 <- list(first=1:5, second=6:10)
lst2
lst2$first #1 2 3 4 5
lst2$second[2:4] #7 8 9
#data.frame ($) = column명
#list ($) = key명

# 3) 다양한 자료형 (숫자형,문자형,논리형)
lst3 <- list(name=c('홍길동','유관순'),
             age=c(35,25),
             gender=c('m','f'))
mode(lst3) #list
class(lst3) #list
mean(lst3$age) #30

# 4) 다양한 자료구조 (vector,matrix,array)
lst4 <- list(one = c ('one','two','three'),
             two = matrix (1:9, nrow=3) ,
             three = array (1:12,c(2,3,2)))

# 5) list 형변환
multi_list <- list(r1=list(1,2,3),
                   r2=list(10,20,30),
                   r3=list(100,200,300))

# do.call(func, object)
mat <- do.call(rbind,multi_list)
mat #이중으로 들어간 리스트의 가상 key가 사라짐 [[1]],[[2]],[[3]]

?do.call

# 6) list 처리 함수
x <- list(1:10) #key 생략
x #key:[[1]] value:1~10
#list -> vector
v <- unlist(x) #list해제
v #key가 사라짐 vector형태가 됨
a <- list(1:5)
b <- list(6:10)
a;b
#lapply(x,function)
lapply(c(a,b),max) #값을 list로 출력
sapply(c(a,b),max) #값을 vector로 출력



# 6. 서브셋 subset : 백터, 매트릭스, 데이터프레임에 활용
# -특정 행 또는 열 선택 -> 새로운 dataset생성

x <- 1:5
y <- 6:10
z <- letters[1:5]

df <- data.frame(x,y,z)
?subset
#subset(x, subset(조건지정), select(특정컬럼설정), drop = FALSE, ...)

# 1) 조건식으로 subset 생성
subset(df, x>=2)
# 2) select로 subset 생성
subset(df,select=c(x,z)) #select없이 c만 사용하면 오류
                         #특정 컬럼을 선택할때는 select를 꼭 사용
# 3) 조건식 + select
subset(df, x>=2 & x<=4, select=c(x,z))
#class(자료구조)는 모두 data.frame임

# 4) 특정 칼럼의 특정 값으로 subset 생성
subset(df,z %in% c('a','c','e')) #인연산자의 특징 : 특정행만 고를때


#[실습] iris
iris
str(iris)
# 'data.frame':	150 obs. of  5 variables:
# $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

iris_df = subset(iris,Sepal.Length>=mean(Sepal.Length),
                 select=c(Sepal.Length,Petal.Length,Species))

mean(iris$Sepal.Length)
subset(iris,select=Sepal.Length)


# 7. 문자열 처리와 정규표현식
install.packages('stringr')
library(stringr)
string <- 'hong35lee45kang55유관순25이사도시30'


library(help='stringr')
methods(stringr)

# 1) str_extract_all()
# 메타문자 : 패턴지정 특수기호 , list 값으로 나옴

# (1) 반복관련 메타문자 : [x] : x 1개, {n} : n개 연속
str_extract_all(string,'[a-z]{3}') 

#영문 소문자가 3개 연속으로 나오는 문장을 찾아 출력
# [1] "hon" "lee" "kan"
str_extract_all(string,'[a-z]{3,}') 
#3개 이상의 영문 소문자를 출력
# [1] "hong" "lee"  "kang"
name <- str_extract_all(string,'[가-힣]{3,}')
# [1] "유관순"   "이사도시"
#unlist = list -> vector로 바꿔줌 , key가 제외됨
unlist(name)
name <- str_extract_all(string,'[가-힣]{2,3}') #범위설정
#숫자 출력하고 평균값 구하기
ages <- str_extract_all(string,'[0-9]{2}') #list
age <- unlist(ages) #vector
age <- as.numeric(age) #numeric
mean(age)
#cat함수 사용
cat('나이평균=',mean(age))

# (2) 단어와 숫자 관련 메타문자
# 단어 : \\w = 영어,숫자,한글 (특수문자 제외)
# 숫자 : \\d = [0-9]

jumin <- '123456-4234567'
str_extract_all(jumin,'[0-9]{6}-[1-4]\\d{6}')

email <- 'kp1234@naver.com'
str_extract_all(email,'[a-z]{3,}@[a-z]{3,}.[a-z]{2,}') #error
str_extract_all(email,'[a-z]\\w{3,}@[a-z]{3,}.[a-z]{2,}') #해결

# (3) 접두어(^)/접미어($) 메타문자
email3 <- 'kp1234@naver.com'
str_extract_all(email3,'[a-z]\\w{3,}@[a-z]{3,}.[a-z]{2,}') #감지못함
str_extract_all(email3,'^[a-z]\\w{3,}@[a-z]{3,}.[a-z]{2,}') #문장의 첫글자에 조건을 부여할때 ^사용
str_extract_all(email3,'^[a-z]\\w{3,}@[a-z]{3,}.com$') #문장의 끝부분에 조건을 지정할때 $사용

# (4) 특정문자 제외 메타문자
string #"hong35lee45kang55유관순25이사도시30"
result <- str_extract_all(string,'[^0-9]{3,}') #숫자를 제외한 3자이상의 문자열 출력
name <- str_extract_all(result[[1]],'[가-힣]{3,}')
unlist(name)


# 2) str_length()
length(string) #1 문자열1개
str_length(string) #28 문자28개

# 3) str_locate() / str_locate_all()
str_locate(string,'g') # 4 4
str_locate_all(string,'g') # 4 4 15 15

# 4) str_replace() / str_replace_all()
str_replace(string,'[0-9]{2}','') #"honglee45kang55유관순25이사도시30"
str_replace_all(string,'[0-9]{2}','') #"hongleekang유관순이사도시"

# 5) str_sub()
str_sub(string,4,6)

# 6) str_split()
string2 <- '홍길동,이순신,강감찬,유관순'
nnn <- str_split(string,'[0-9]{2,}') #숫자를 기준으로 문자열 분리
name <- str_split(string2,',') #"홍길동" "이순신" "강감찬" "유관순"
unlist(name)

# 7) str_join : paste
paste(name,collapse=',')







