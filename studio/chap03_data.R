chap03_dataIO

# 1. data 불러오기 (키보드입력, 파일가져오기,웹문서가져오기)

# 1) 키보드입력
#(1) 숫자입력
x <- scan()
x # 10 20 30
x[3] # 30
#(2) 문자입력
y <- scan(what='')
y # 홍길동 이순신 유관순 강감찬
y[3] # 유관순

# 2) 파일 읽기

#(1) read.table() : 컬럼구분, 구분자:공백 특수문자 , sep''와 header=F가 기본값 
setwd('C:/IITT/2_Rwork/Part-I')
#컬럼명이 없는 경우 : 공백
read.table('student.txt') #공백구분, 기본컬럼명 제공 : V1,V2,V3 등
#컬럼명이 있는 경우 : 특수문자
read.table('student2.txt', header=TRUE,sep=';') #header함수 지정해줌
#결측치 처리하기 : - , &
read.table('student3.txt', header=TRUE, na.string='-') #-문자를 NA처리
student3 <- read.table('student3.txt', header=TRUE, na.string='-')
mean(student3$키,na.rm=T)
class(student3) #data.frame

#(2) read.csv() : 구분자 : ,(콤마) , sep','와 header=T가 기본값
read.csv('student4.txt',na.string='-')
#탐색기 이용해서 파일 선택하기
exe <- read.csv(file.choose()) #폴더창이 뜸
exe

#(3) read.xlsx() : 엑셀데이터 읽기
install.packages('xlsx')
library(rJava)
library(xlsx)
#엑셀파일 열기
kospi <- read.xlsx('sam_kospi.xlsx',sheetIndex=1)
#한글이 포함된 엑셀파일 열기
read.xlsx('studentexcel.xlsx',sheetIndex=1,encoding='UTF-8')

#(4) 인터넷 파일 읽기
# 데이터 셋 제공 사이트 
# http://www.public.iastate.edu/~hofmann/data_in_r_sortable.html - Datasets in R packages
# https://vincentarelbundock.github.io/Rdatasets/datasets.html
# https://r-dir.com/reference/datasets.html - Dataset site
# http://www.rdatamining.com/resources/data

titanic <- read.csv('https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv')
str(titanic)
head(titanic) #데이터 자료의 top6까지만 확인가능
table(titanic$survived) #no:817 yes:499 컬럼 값에 대한 빈도수 확인가능
table(titanic$sex) #man:869 women:447
table(titanic$survived,titanic$sex) #성별에 따른 생존비율
tab <-table(titanic$survived,titanic$sex)
barplot(tab,col=rainbow(2)) #생존비율 차트로 확인
plot(tab)


getOption('max.print') #1000개의 항목으로 제한
                       #titanic데이터가 200 5 까지만 나오는 이유
options(max.print=999999999) #option변경
titanic #1316의 행까지 모든 자료 확인가능



# 2. 데이터 저장(출력)하기

# 1) 화면 출력 

#cat()
x=20
y=30
z=x+y
cat('z=',z) # 특정 문자를 입력해서 변수값을 같이 출력하고 싶을때 사용
#print()
print(z) # 변수 또는 수식만 입력 가능
print(z*2) # 100
print('z=',z) #error

# 2) 파일 저장 (출력)
#read.table -> write.table 구분자 : 공백, 특수문자
#read.csv -> write.csv 구분자 : 콤마
#read.xlsx -> write.xlsx 액셀 (패키지필요)

#(1) write.table()
setwd('C:/IITT/2_Rwork/output')
write.table(titanic,'titanic.txt',row.names=F,quote=F)
#row.names : 행번호매김의 여부 quote : 문자의 따움표 여부

#(2) write.csv()
titanic_df <- titanic[-1] #첫번째 컬럼 제외한채 변수에 자료저장
head(titanic_df)
write.csv(titanic_df,'titanic_df.csv',row.names=F,quote=F)

#(3) write.xlsx()
write.xlsx(titanic,'titanic.xlsx',sheetName='titanic', row.names=F)






