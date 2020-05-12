#################################
## <제7장 연습문제>
################################# 

# 01. 본문에서 생성된 dataset2의 직급(position) 칼럼을 대상으로 1급 -> 5급, 5급 -> 1급 형식으로
# 역코딩하여 position2 칼럼에 추가하시오.
table(dataset2$position)
position <- dataset2$position
cosition <- 6-position
dataset2$position <- cosition
table(dataset2$position)

# 02. dataset2의 resident 칼럼을 대상으로 NA 값을 제거한 후 dataset3 변수에 저장하시오.
dataset2$resident
dim(dataset2) # 232 10
dataset3 <- subset(dataset2, !is.na(resident))
dim(dataset3) # 217 10

# 03. dataset3의 gender 칼럼을 대상으로 1->"남자", 2->"여자" 형태로 코딩 변경하여 
# gender2 칼럼에 추가하고, 파이 차트로 결과를 확인하시오.
head(dataset3,20)
dataset3$gender2[dataset3$gender==1]<-'남자'
dataset3$gender2[dataset3$gender==2]<-'여자'
pie(table(dataset3$gender))


# 04. 나이를 30세 이하 -> 1, 31~55 -> 2, 56이상 -> 3 으로 리코딩하여 age3 칼럼에 추가한 후 
# age, age2, age3 칼럼만 확인하시오.
dataset2$age3[dataset2$age<=30] <-'1'
dataset2$age3[dataset2$age>=31&dataset2$age<=55] <-'2'
dataset2$age3[dataset2$age>=56] <-'3'
head(dataset2[c('age','age2','age3')],20)


# 05. 정제된 data를 대상으로 작업 디렉터리(c:/Rwork/output)에 cleanData.csv 파일명으로 
# 따옴표와 행 이름을 제거하여 저장하고, new_data변수로 읽어오시오.

# (1) 정제된 데이터 저장
str(dataset2)
setwd('C:/IITT/2_Rwork/output')
write.csv(dataset2,'cleanData.csv',row.names = F,quote=F)

# (2) 저장된 파일 불러오기/확인
new_data <- read.csv('cleanData.csv',header=T)
new_data

# 06. mtcars 데이터셋의 qsec(1/4마일 소요시간) 변수를 대상으로 극단치(상위 0.3%)를 
# 발견하고, 정제하여 mtcars_df 이름으로 서브셋을 생성하시오.

library(ggplot2)
str(mtcars) # 'data.frame':	32 obs. of  11 variables:


# (1) 이상치 통계
boxplot(mtcars$qsec)$stats
plot(mtcars$qsec)


# (2) 서브셋 생성 
mtcars_df <- subset(mtcars,qsec>=14.5&qsec<=20.22)

# (3) 정제 결과 확인 
boxplot(mtcars_df$qsec)


# 07. user_data.csv와 return_data.csv 파일을 이용하여 각 고객별 
# 반품사유코드(return_code)를 대상으로 다음과 같이 파생변수를 추가하시오.
user <- read.csv(file.choose())
return <- read.csv(file.choose())
str(user)
str(return)


# <조건1> 반품사유코드에 대한 파생변수 칼럼명 설명 
# 제품이상(1) : return_code1, 변심(2) : return_code2, 
# 원인불명(3) : return_code3, 기타(4) : return_code4 
return$codename <- ifelse(return$return_code==1,'제품이상(1)',
                          ifelse(return$return_code==2,'변심(2)',
                                 ifelse(return$return_code==3,'원인불명(3)','기타(4)')))
head(return,20)
return_data <- dcast(return,user_id~codename,length)


# <조건2> 고객별 반품사유코드를 고객정보(user_data) 테이블에 추가(join)
master_data <- left_join(user,return_data,id='user_id')
head(master_data,20)
dim(master_data)
names(master_data)
master_data$tot_return <- master_data[6]+master_data[7]+master_data[8]+master_data[9]
names(master_data$tot_return) <- 'tot_return'
dim(master_data)
head(master_data,20)








