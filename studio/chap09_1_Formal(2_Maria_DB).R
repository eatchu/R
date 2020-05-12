# chap09_1_Formal(2_Maria_DB)

# Maria DB 정형 데이터 처리

# 패키지 설치
# - RJDBC 패키지를 사용하기 위해서는 우선 java를 설치해야 한다.
#install.packages("rJava")
#install.packages("DBI")
#install.packages("RJDBC") # JDBC()함수 제공 

# 패키지 로딩
library(DBI)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_151')
library(rJava)
library(RJDBC) # rJava에 의존적이다.

################ MariaDB or MySql ###############
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="C:/IITT/2_Rwork/tools(R)/MariaDB/mysql-connector-java-5.1.46/mysql-connector-java-5.1.46/mysql-connector-java-5.1.46-bin.jar")

# driver가 완전히 로드된 후 db를 연결한다.
conn <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/work", "scott", "tiger")
#################################################           

# DB 연결 확인 :  테이블의 컬럼 보기 
query <- 'show tables'
dbGetQuery(conn, query) # conn, table name
dbGetQuery(conn,"select*from goods")

# DB 구조 변경
# 1. 레코드 추가
query <- "insert into goods values(5,'전화기',4,450000)"
dbSendUpdate(conn,query)
# 2. 레코드 수정 : 전화기 -> phone
query <- "update goods set name='phone' where code=5"
dbSendUpdate(conn,query)
# 3. 레코드 삭제 
query <- "delete from goods where code=5"
dbSendUpdate(conn,query)


# R 변수 저장
goods <- dbGetQuery(conn,"select*from goods")
goods
str(goods)

price <- goods$su*goods$dan
goods$price <- price
goods

# table -> 처리 -> file save
write.csv(goods,'goods.csv',quote = F, row.names = F)
getwd()


# table -> 처리 -> table save
# [단계1] table 생성
query <- "create table goods_manager
(code int, name varchar(50), su int, dan int, price int)"
dbSendUpdate(conn,query)
dbGetQuery(conn,'show tables')
dbSendUpdate(conn,"drop table goods_new")

# [단계2] table에 객체(object)생성
# dbWriteTable(conn,name=테이블이름,value=복사할 테이블) : 구조,객체가 전부 복사됨
dbWriteTable(conn, name="goods_manager", value=goods)
dbGetQuery(conn,'select*from goods_new')
 # [단계3] db 연결 종료
dbDisconnect(conn)





