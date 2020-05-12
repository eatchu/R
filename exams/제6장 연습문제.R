#################################
## <제6장 연습문제>
################################# 

# <dplyr 패키지 관련 연습문제> 
library(dplyr)
data(iris)


# 01. iris의 꽃잎의 길이(Petal.Length) 칼럼을 대상으로 1.5 이상의 값만 필터링하시오.
iris %>% filter(Petal.Length>=1.5) %>% head()
petalLavg <- iris %>% filter(Petal.Length>=1.5) 
dim(petalLavg) #126 5

# 02. 01번 결과에서 1,3,5번 칼럼을 선택하시오.
iris %>% filter(Petal.Length>=1.5) %>% select(1,3,5) %>% head()
select(petalLavg,c(1,3,5)) # 126 3

col_names <- names(iris)
select(petalLavg,col_names[-c(2,4)])

# 03. 02번 결과에서 1번 - 3번 칼럼의 차를 구해서 diff 파생변수를 만들고, 앞부분 6개만 출력하시오.
summ <- iris %>% filter(Petal.Length>=1.5) %>% select(1,3,5)
head(mutate(summ,diff=Sepal.Length-Petal.Length))

iris_sel <- select(petalLavg,col_names[-c(2,4)])
iris_mut <- mutate(iris_sel,diff=Sepal.Length-Petal.Length)

# 04. 03번 결과에서 꽃의 종(Species)별로 그룹화하여 Sepal.Length와 Petal.Length 변수의 평균을 계산하시오.
resu <- mutate(summ,diff=Sepal.Length-Petal.Length)
resu_grp <- group_by(resu,Species)
summarise(resu_grp,mean(Sepal.Length),mean(Petal.Length))





# <reshape2 패키지 관련 연습문제> 
library('reshape2')

# 05. reshape2 패키지를 적용하여 각 다음 조건에 맞게 iris 데이터 셋을 처리하시오. 

# 조건1) 꽃의 종류(Species)를 기준으로 ‘넓은 형식’을 ‘긴 형식’으로 변경하기(melt()함수 이용)
iris %>% head()
str(iris)
iris_melt <- melt(iris,id='Species')


# 조건2) 꽃의 종별로 나머지 4가지 변수의 합계 구하기(dcast()이용)
dcast(iris_melt,Species~variable,sum)




