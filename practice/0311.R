##################################
########표준편차,분산#############
##################################
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



####################################
#########구구단 파일 만들기#########
####################################
#구구단

for(i in 2:9){
  for(j in 1:9){
    cat(j*i,
        file='C:/IITT/2_Rwork/R-script/studio/gygy.txt',
        append=T)
  }
}

?append


#####################################
#######자료구조 행,열,면 추가########
#####################################
#데이터프레임 열 추가
str(iris)
iris$diff <- iris$Sepal.Length-iris$Sepal.Width
str(iris)
#어레이 열 추가
str(Bug_Metrics_Software)
bug <- Bug_Metrics_Software
side <- c('Before','After','Test')
side2 <- c('JDT','PDE','Equinox','Lucene','Mylyn')
side3 <- c('Bugs','NT.Bugs','Major','Critical','H.Priority')
bug.new <- array(bug,dim=c(5,5,3),
                 dimnames=list(software=side2,bugs=side3,side)) 
dim(bug.new)
bug.new 
bug.new[,,3] <- bug[,,1]-bug[,,2]
bug.new



###################################
##############실습#################
###################################
gugudan <- read.csv(file.choose())
gugudan #매트릭스로 생성됨
library(stringr)
dim(gugudan)
#매트릭스 스플릿하는 법
#79x1을 9x9로 바꾸기
names(gugudan) <- '구구단'
row.names(gugudan)
gugudan[1:9,]



gugu<-function(x){
  y=1
  if(x>=2){
    y=y+(x-1)*10
  }
  gugudan[y:(x*10-1),]
}

gu <- data.frame(gugu(1),gugu(2),gugu(3),gugu(4),gugu(5),gugu(6),gugu(7),gugu(8))
names(gu)<-c('2단','3단','4단','5단','6단','7단','8단','9단')
gu #성공 !

names(gu)



###############################################
###############################################
#####실습시 필요한 내용일것 같아서 출력########
#데이터프레임으로 변환 : 컬럼 단위의 데이터 활용을 위해서
freqData <- as.data.frame(table(galton$child, galton$parent))
freqData # Var1 Var2 Freq(중복 수)
str(freqData) # 154 obs(928 관측치가 중복 제외한 154개 관측치 생성 )
names(freqData)=c("child","parent", "freq") # 컬럼에 이름 지정
#프레임 -> 벡터 -> 수치데이터변환, cex : 빈도수에 0.15 곱(가중치 적)
parent <- as.numeric(as.vector(freqData$parent))
child <- as.numeric(as.vector(freqData$child))
plot(child~parent, pch=21, col="blue", bg="green",
     cex=0.15*freqData$freq, xlab="parent", ylab="child")






#########################################
##############이름지정###################
#########################################
#matrix 열/행 이름 지정하기 colnames() & rownames
colnames(m5) <- c('one','two','three')
m5 # 가로에 이름 지정됨
rownames(m5) <- c('one','two','three')
m5 # 세로에 이름 지정됨
#vector 원소 이름 지정
text <- c('hong','lee','kang')
age <- c(35,45,55)
names(age) <- text
#vector 라벨링 -> names
#matrix 라벨링 -> colnames/rownames


# which()
# 조건식에 만족하는 위치 반환
# 괄호내의 조건에 해당하는 인덱스 값을 출력
name <- c('kim','lee','choi','park')
which(name=='choi') #3
#Boston = data.frame
name <- names(Boston) #boston이 가지고 있는 칼럼명 입력
length(name) #14
# x(독립변수), y(종속변수) 선택
which(name=='medv') #14
y_col <- which(name=='medv')
y <- Boston[y_col]
head(y)

Boston%>%select(medv)%>%head()

#레전드 추가하기 전 행/열 이름 지정
str(VADeaths)
row_names <- rownames(VADeaths)
row_names
col_names <- colnames(VADeaths)
col_names










