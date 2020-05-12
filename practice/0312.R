#################################
#################################
##내가 가진 데이터가 NA가 있는지 확인하는법 
#################################
#################################
# 결측치 확인 써머리 사용하면 NA 값 확인가능
# 숫자에서 사용 : 문자는 안나옴
summary(dataset$price)
#테이블 조회 : 문자열 유용
table(is.na(new_data$age)) #특정 컬럼
# FALSE  TRUE  T값이 결측치
# 217    14 
table(new_data$job2,new_data$age2,useNA='ifany') 
#            장년층 중년층 청년층 <NA>
# 개인사업     18     30     29    8
# 공무원       20     20     14    4
# 회사원       11     49     14    2
# <NA>          2      3      7    0
table(is.na(dataset)) #전체 컬럼
# FALSE  TRUE 
# 2582   118 
# NA가 존재 유무확인 : NA값이 나오면 결측지 존재(숫자에서 사용)
sum(dataset$price) 
#행의 갯수로 확인하는법 : 문자열에 유용
dim(dataset)
length(na.omit(dataset$price)) #행의 갯수 차이나면 결측치 존재






########################################
#########통계함수 적용조건##############
########################################

# mean() 같은 함수 적용 방식
# mean 백터인 경우에 사용가능 값이 하나일땐 안돼
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




#########################################
###############0값 지우기################
########고객별 구매 합계 및 평균#########
#########################################
install.packages('reshape2') 
library(reshape2)

data <- read.csv(file.choose())

# 1) dcast() : long -> wide
# 형식) dcast(dataset,row~col,func)
data <- read.csv(file.choose())
data
data1 <- dcast(data,Customer_ID~Date,mean)
str(data1) #data.frame
summary(data) # 이거 값 왜 안나오는지 질문
apply(data1,1,mean,na.rm=T) # 아이디까지 다 더해버려서 쓸 수 없음


bsum1=0;bsum2=0;bsum3=0;bsum4=0;bsum5=0

for(i in 1:nrow(data)){
  if(data$Customer_ID[i]==1){
    bsum1=bsum1+data$Buy[i]
    names(bsum1) <- 'ID=1'
  }else if(data$Customer_ID[i]==2){
    bsum2=bsum2+data$Buy[i]
    names(bsum2) <- 'ID=2'
  }else if(data$Customer_ID[i]==3){
    bsum3=bsum3+data$Buy[i]
    names(bsum3) <- 'ID=3'
  }else if(data$Customer_ID[i]==4){
    bsum4=bsum4+data$Buy[i]
    names(bsum4) <- 'ID=4'
  }else if(data$Customer_ID[i]==5){
    bsum5=bsum5+data$Buy[i]
    names(bsum5) <- 'ID=5'
  }
}
bsum1;bsum2;bsum3;bsum4;bsum5


data1<-group_by(data,Customer_ID)
summarise(data1,mean(Buy))
#        Customer_ID `mean(Buy)`
#           <int>       <dbl>
# 1           1        4.33
# 2           2        4.17
# 3           3        5   
# 4           4        7   
# 5           5        4.4 




#######################################################
#데이터에 각 칼럼이 가지고 있는 distinct값 갯수 구하기#
###############아마 table 사용#########################
####방법 유니크 함수 사용하기!!!########
unique(mpg_df$drv) #distinct 값
length(unique(mpg_df$drv)) #distinct 갯수









##########################################
########## 각 학년별 나이 합계############ 
#######그룹화 사용해서 다시 구하기########
##########################################
###################1######################
age <- round(runif(12,min=20,max=25))
grade <- rep(1:4, 3)
age[5] <- NA
age[8] <- NA

DF <- data.frame(age,grade)
DF1 <- DF

gra <- group_by(DF1,grade)
SUM <- summarise(gra,sum=sum(age,na.rm=T))
SUM <- as.data.frame(SUM)
SUM <- SUM$sum 

g=0

for(i in 1:nrow(DF1)){
  if(is.na(DF1$age[i])){
    g=DF1$grade[i]
    DF1$age[i]=round(SUM[g]/2)
  }
}

DF1


##################2###################

age <- round(runif(12,min=20,max=25))
grade <- round(runif(12,min=0.5,max=4)) 


age[5] <- NA
age[8] <- NA

DF <- data.frame(age,grade)
DF2 <- DF


gra <- group_by(DF2,grade)
MEAN <- summarise(gra,mean=mean(age,na.rm=T))
MEAN <- as.data.frame(MEAN)
MEAN <- MEAN$mean
MEAN <- round(MEAN)


g=0

for(i in 1:nrow(DF2)){
  if(is.na(DF2$age[i])){
    g=DF2$grade[i]
    DF2$age[i]=MEAN[g]
  }
}

DF2









#####################################
###########문자열계산법##############
#######문자 가지고 통계내기##########
#####################################
# library(hflights)
# hflights$TailNum 이런 데이터 통계내는법





#######################################
##########그룹화 조건 설정#############
#######################################
# 예제) 각 항공기별(TailNum) 비행편수가 40편 이상
#       평균 비행거리가 2000마일 이상인 경우의
#       평균 도착 지연시간
#(1) 항공기별 그룹화
str(hflights_df)
plane <- hflights_df %>% group_by(TailNum)
#(2) 항공기별 요약 통계
planes <- summarise(plane,count=n(),dist_avg=mean(Distance,na.rm=T),
                    delay_avg=mean(ArrDelay,na.rm=T))
#(3) 항공기별 요약 통계 필터링
result <- filter(planes,count>=40 & dist_avg>=2000)






##########################################################
### 꽃의 종류별로 4가지 컬럼의 총 합 구하기###############
##########################################################
iris
iris1<-melt(iris,id='Species')
iris2 <- dcast(iris1,Species~variable,sum)
S <- group_by(iris,Species)
summarise(S,sum(Sepal.Length)+sum(Sepal.Width)+sum(Petal.Length)+sum(Petal.Width))
sum <- iris2[,2]+iris2[,3]+iris2[,4]+iris2[,5] 
iris2$sum <- sum
iris2


#########length함수 용도 ##################
length(iris) #컬럼(열) 갯수
length(iris$Sepal.Length) #데이터(행) 갯수
length(unique(iris$Species)) #distinct 갯수 
length(unique(dataset$resident)) #결측지 값도 같이 갯수로 나옴 
table(dataset$resident) #table로 확인할때는 결측지 안나옴










##########table 여러가지로 활용해보기##########
# 교차분할표 2 : table
# table(행집단변수,열집단변수)
table(mpg_df$cyl,mpg_df$drv)









############################################
#############어레이 이름추가 ###############
############################################

data('airquality')
str(airquality) # 153 6
unique(airquality$Month) # 5 6 7 8 9
table(airquality$Month)
table(airquality$Day)

# wide -> long
air_melt <- melt(airquality,id=c('Month','Day'),
                 na.rm = T) #월단위-일단위로 컬럼이 집계됨
dim(air_melt)
table(air_melt$variable)

# [일,월,컬럼]
air_acast <- acast(air_melt,Day~Month~...)
dim(air_acast) # 31 5 4
class(air_acast) #array
str(air_acast)



side <- c('Before','After','Test')
side2 <- c('JDT','PDE','Equinox','Lucene','Mylyn')
side3 <- c('Bugs','NT.Bugs','Major','Critical','H.Priority')
head(bug)
bug.new <- array(bug,dim=c(5,5,3),
                 dimnames=list(software=side2,bugs=side3,side)) #면추가하기
# x <- array(1:10,c(행,열,면)) 어레이 생성법


