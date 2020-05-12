#################################
## <제9장-1 연습문제>
################################# 

# 01. 다음과 같은 단계를 통해서 테이블을 호출하고, SQL문을 이용하여 레코드를 조회하시오.
# (DBMS : Oracle 사용)

# [단계 1] 사원테이블(EMP)을 검색하여 결과를 EMP_DF로 변수로 불러오기
emp_df<-dbGetQuery(conn,'select*from emp')
emp_df

# [단계 2] EMP_DF 변수를 대상으로 부서별 급여의 합계를 막대차트로 시각화
sal_sum <- dbGetQuery(conn,"select sum(sal),deptno from emp group by deptno")
str(sal_sum)
sal_sum$dname[sal_sum$DEPTNO==10] <- 'accounting'
sal_sum$dname[sal_sum$DEPTNO==20] <- 'reserch'
sal_sum$dname[sal_sum$DEPTNO==30] <- 'sales'
sal_sum
name <- sal_sum$dname
barplot(sal_sum$`SUM(SAL)`,col=rainbow(3),
        names.arg=name)


library(dplyr)
deptno <- group_by(emp_df,DEPTNO)
sum_sal <- summarise(deptno,sum=sum(SAL))

barplot(sum_sal$sum, col=rainbow(3),
        main='부서별 월급 합계')



# [단계 3] 막대차트를 대상으로 X축의 축눈금을 부서명으로 표시하기
dept_df <- dbGetQuery(conn,'select*from dept')
dept_df
dname <- dept_df$DNAME[1:3]
barplot(sum_sal$sum, col=rainbow(3),
        main='부서별 월급 합계',
        names.arg = dname)




