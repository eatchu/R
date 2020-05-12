# Chap09_Formal(Oracle)


########################################
## Chapter09-1. 정형데이터 처리 
########################################

# Oracle DB 정형 데이터 처리

# 1. 패키지 설치
# - RJDBC 패키지를 사용하기 위해서는 우선 java를 설치해야 한다.
#install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")

# 2. 패키지 로딩
library(DBI)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_151')
library(rJava)
library(RJDBC) # rJava에 의존적이다.(rJava 먼저 로딩)

# 3) Oracle 연동   

############ Oracle 11g ##############
# driver  object : 드라이버 생성
drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
          "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")
# db연동(driver, url,uid,upwd)  object 
conn<-dbConnect(drv, "jdbc:oracle:thin:@//127.0.0.1:1521/xe","scott","tiger")
####################################

query <- 'select*from tab'
dbGetQuery(conn,query)

# table 생성
query <- 'create table db_test
(sid int, pwd char(4), name varchar(20), age int)'
dbSendUpdate(conn,query)
dbGetQuery(conn,'select*from tab')


# db 내용 수정 : insert, update, delete

# 1. insert 
query <- "insert into db_test values(1001,'1234','홍길동',35)"
dbSendUpdate(conn,query)
dbGetQuery(conn,'select*from db_test')

# 2. update
dbSendUpdate(conn,"update db_test set name='김길동' where sid=1001")
dbGetQuery(conn,'select*from db_test')

# 3. delete
dbSendUpdate(conn,"delete from db_test where sid=1001")
dbGetQuery(conn,'select*from db_test')

EMP <- dbGetQuery(conn,'select*from emp')
str(EMP)
mean(EMP$SAL)
summary(EMP)


# 문1) SAL 2500이상이고 JOB이 MANAGER인 사원만 검색하기
query <- "select*from emp where sal>=2500 and job='MANAGER'"
manager_2500 <- dbGetQuery(conn,query)

# 문2) sub query 관련 문제 :
#      부서가 SALES인 전체 사원의 이름, 급여, 직책 조회
#      sub : DEPT, main : EMP
DEPT <- dbGetQuery(conn,'select*from dept')
str(DEPT)
query <- "select ename, sal, job from emp where deptno=
(select deptno from dept where dname='SALES')"
sales <- dbGetQuery(conn,query)
sales

query2 <- "select e.ename, e.sal, e.job, d.dname from emp e ,dept d 
where e.deptno=d.deptno and d.dname='SALES'"
sales2 <- dbGetQuery(conn,query2)
sales2


# 문3) join 쿼리문
product <- dbGetQuery(conn,"select*from product")
sale <- dbGetQuery(conn,"select*from sale")
joinquery <- "select p.code,s.price,s.sdate, p.name
from product p, sale s where p.code=s.code and p.name like'%기' "
dbGetQuery(conn,joinquery)


join <- "select p.code,s.price,s.sdate,p.name from product p inner join sale s
on s.code=p.code and p.name like'%기'"
dbGetQuery(conn,join)


# 4. 테이블 삭제
dbSendUpdate(conn,"drop table db_test purge")
dbGetQuery(conn,"select*from tab")









