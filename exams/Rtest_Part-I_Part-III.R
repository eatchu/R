##################################
###  Part-I ~ Part-III 총정리  ###
##################################

setwd("C:/IITT/2_Rwork/Part-IV")
election <- read.csv("election_2012.csv") # 2012년도 미국 대통령 선거 후원금 현황 
str(election) # 'data.frame':	1001731 obs. of  16 variables:
# 2.cand_id : 대선후보자
# 3.cand_nm : 대선 후보자 이름
# 4.contbr_nm  : 후원자 이름 
# 9.contbr_occupation : 후원자 직업군 
# 10.contb_receipt_amt : 후원금

### 전처리 ###
# [문제1] 위 5개 칼럼으로 data.frame 생성 clean_election2 변수에 저장하시오.
clean_election2 <- election[c(2,3,4,9,10)]
str(clean_election2) 


# [문제2] 직업군 칼럼을 대상으로 NA를 포함하는 관측치를 제외하여 clean_election2 변수에 저장하시오.
# <조건> 전처리 전과 후 관측치 수의 차이는? 
#   힌트) subset(), is.na() 함수 이용 
summary(clean_election2)
# 칼럼명 contbr_occupation에 결측치 128개 존재
length(clean_election2$contbr_occupation) #1001731
clean_election3 <- subset(clean_election2,!is.na(contbr_occupation))
length(clean_election3$contbr_occupation) #1001603
#1001731-128


# [문제3] 5만개 관측치만 샘플링하여 clean_election2 변수에 저장하시오.
#  힌트) sample() 함수 이용
clean_election2 <- clean_election3[sample(nrow(clean_election3),50000),]
str(clean_election2) # 50000x5


# [문제4] 'Romney, Mitt'와 'Obama, Barack' 후보자별 서브셋을 작성하여 romney, obama 변수에 저장하시오
unique(clean_election2$cand_nm)
table(clean_election2$cand_nm) # Obama, Barack29688  Romney, Mitt 5385

romney <- subset(clean_election2,clean_election2$cand_nm=='Romney, Mitt')
obama <- subset(clean_election2,clean_election2$cand_nm=='Obama, Barack')

length(romney$cand_nm) #5385
length(obama$cand_nm) #29688

# [문제5] romney, obama 변수를 대상으로 병합하여 obama_romney 변수에 저장하시오.
# 힌트) rbind()

a <- list(romney,obama)
str(a)
obama_romney <- do.call(rbind,a)
str(obama_romney)



## 교차분석 ## 

# [문제6] 후원자의 직업군과 대통령 당선 유무에 따라서 다음과 같이 파생변수를 만드시오.
# <조건1> 대상 변수 : obama_romney
# <조건2> 다음과 같은 후원자의 직업군을 job1, job2, job3로 리코딩하여 contbr_occupation2 칼럼 저장 
# job1 : INVESTOR - 투자자, EXECUTIVE - 경영진, PRESIDENT  - 회장 
# job2 : LAWYER - 변호사, PHYSICIAN  - 내과의사, ATTORNEY   - 변호사
# job3 : RETIRED  - 퇴직자, HOMEMAKER  - 주부

length(unique(obama_romney$contbr_occupation))


# job1 리코딩 
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='INVESTOR'] <- 'job1'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='EXECUTIVE'] <- 'job1'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='PRESIDENT'] <- 'job1'

obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='LAWYER'] <- 'job2'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='PHYSICIAN'] <- 'job2'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='ATTORNEY'] <- 'job2'

obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='RETIRED'] <- 'job3'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='HOMEMAKER'] <- 'job3'

obama_romney


# <조건3> contbr_occupation2 칼럼에 NA를 포함한 관측( 제거 후 서브셋 작성(obama_romney2 저장)  

length(obama_romney$contbr_occupation2) #35073
obama_romney2 <- subset(obama_romney,!is.na(obama_romney$contbr_occupation2))
length(obama_romney2$contbr_occupation2) #13037


# <조건4> obama_romney2 변수를 대상으로 cand_nm 칼럼이 'Obama, Barack'이면 '당선', 
#               'Romney, Mitt'이면 '낙선'으로 파생변수를 생성하여 cand_pass 칼럼 추가

obama_romney2$cand_pass[obama_romney2$cand_nm=='Obama, Barack'] <- '당선'
obama_romney2$cand_pass[obama_romney2$cand_nm=='Romney, Mitt'] <- '낙선'

obama_romney2
table(obama_romney2$cand_pass)


# [문제7]  [문제6]에서 만든 파생변수(직업유형과 당선유무)를 이용하여 교차분할표를 작성하시오.  
table(obama_romney2$contbr_occupation2,obama_romney2$cand_pass)
#       낙선 당선
# job1  227  286
# job2  397 2345
# job3 1665 8117


## chisquare 검정 ##

# [문제8] 후원자의 직업군과 대통령 당선 유무 여부와 관련성이 있는가를 검정하시오.
# <조건1> 대상 변수 : obama_romney2
# <조건2> 귀무가설과 대립가설 수립 
# <조건3> 변수 모델링 : x변수(contbr_occupation2), y변수(cand_pass)
# <조건4> 검정결과 해석

# 귀무가설 : 직업의 유형과 대통령 당선과 관련성이 없다.
# 대립가설 : 직업의 유형과 대통령 당선과 관련성이 있다.
x<- obama_romney2$contbr_occupation2
y<- obama_romney2$cand_pass

library(gmodels)
table(x,y)
CrossTable(x=x, y=y, chisq = TRUE)
# p =  7.051811e-60 : 귀무가설 기각
# [해설] 직업의 유형과 대통령 당선과 관련성이 있다.



## lattice 패키지 ## 

# [문제9] lattice 패키지의 densityplot()함수를 이용하여 후원자의 직업유형별로 다음과 같이 후원금을 시각화하시오.
# <조건1> 대상 변수 : obama_romney2
# <조건2> 후원금이 300달러 ~ 3000달러 사이의 관측치 선정 -> obama_romney3 변수 저장  
# <조건3> obama_romney3 변수 대상 밀도그래프 시각화(x축 : 후원금, 조건 : 당선유무, 그룹 : 직업유형) 

library(lattice)
range(obama_romney2$contb_receipt_amt)
obama_romney3 <- subset(obama_romney2,obama_romney2$contb_receipt_amt>=300&obama_romney2$contb_receipt_amt<=3000)
range(obama_romney3$contb_receipt_amt)
densityplot(~contb_receipt_amt|factor(cand_pass),data=obama_romney3,
            groups=contbr_occupation2, auto.key = TRUE )


## 평균차이 검정 ## 

# [문제10] romney와 obama 후보자를 대상으로 후원자의 직업군이 'RETIRED'인 후원금에 차이가 있는지 검정하시오.
# <조건1> 대상 변수 : obama_romney3
# <조건2> 두집단 평균차이 검정


obama_money <- subset(obama_romney3,obama_romney3$cand_nm=='Obama, Barack'&obama_romney3$contbr_occupation=='RETIRED')
romney_money <- subset(obama_romney3,obama_romney3$cand_nm=='Romney, Mitt'&obama_romney3$contbr_occupation=='RETIRED')
obama_money2 <- obama_money$contb_receipt_amt
romney_money2 <- romney_money$contb_receipt_amt
length(obama_money2)
length(romney_money2)
mean(obama_money2, na.rm=TRUE) 
mean(romney_money2, na.rm=TRUE) 


var.test(obama_money2, romney_money2) #p-value = 0.00473 비동질성분포 : 비모수검정

wilcox.test(obama_money2, romney_money2) #p-value < 2.2e-16 : 귀무가설 기각
# [해설] 두 집단의 후원금에는 차이가 있다


wilcox.test(obama_money2, romney_money2, alter="greater", conf.int=TRUE, conf.level=0.95)
# p-value = 1
wilcox.test(obama_money2, romney_money2, alter="less", conf.int=TRUE, conf.level=0.95)
# p-value < 2.2e-16
# [해설] romney의 후원자의 직업군이 'RETIRED'인 후원금이 obama보다 더 많다



