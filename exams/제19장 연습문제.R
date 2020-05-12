#################################
## <제19장 연습문제>
################################# 

setwd("C:/IITT/2_Rwork/Part-IV")

# 01. tranExam.csv 파일을 대상으로 중복된 트랜잭션 없이  
# 1~2컬럼만 single 형식으로 트랜잭션 객체를 생성하시오.
# (파일경로 : tranExam.csv)

# 단계1 : 트랜잭션 객체 생성 및 확인
# 단계2 : 각 item별로 빈도수 확인
# 단계3 : 파라미터(supp=0.3, conf=0.1)를 이용하여 규칙(rule) 생성 
# 단계4 : 연관규칙 결과 보기

# 단계1 : 트랜잭션 객체 생성 및 확인
library(arules)
tranExam <- read.transactions("tranExam.csv", format="single", 
                              sep=",", cols=c(1,2), rm.duplicates=T)

# 단계2 : 각 item별로 빈도수 확인

# 단계3 : 파라미터(supp=0.3, conf=0.1)를 이용하여 규칙(rule) 생성 

# 단계4 : 연관규칙 결과 보기


# 02. Adult 데이터셋을 대상으로 다음 단계별로 연관분석을 수행하시오.

# 단계1: 최소 support=0.5, 최소 confidence=0.9를 지정하여 연관규칙 생성
data(Adult)
library(arulesViz)

# 단계2: 수행한 결과를 lift 기준으로 정렬하여 상위 10개 규칙 확인
itemFrequencyPlot(Adult, topN=10, type="absolute")
?itemFrequencyPlot

# 단계3: 연관분석 결과를  LHS와 RHS의 빈도수로 시각화 


# 단계4: 연관분석 결과를 연관어 네트워크 형태로 시각화


# 단계5: 연관어 중심 단어 해설




# 03. Adult 데이터셋을 대상으로 다음 단계별로 연관분석을 수행하시오.

# 단계1 : support=0.3, confidence=0.95가 되도록 연관규칙 생성
rule <- apriori(Adult, parameter=list(supp=0.3, conf=0.95)) 
inspect(rule) 
plot(rules, method="grouped")

#  단계2 : 왼쪽 item이 백인(White)인 규칙만 서브셋으로 작성하고, 시각화
o <- subset(rule, lhs %in% 'race=White')
plot(o)

#  단계3 : 왼쪽 item이 백인이거나 미국인을 대상으로 서브셋을 작성하고, 시각화
o <- subset(rule, lhs %in% c('race=White','native-country=United-States'))
plot(o)

#  단계4 : 오른쪽 item에서 'Husband' 단어를 포함한 규칙을 서브셋으로 작성하고, 시각화
d <- subset(rule, rhs %pin% 'Husband')
plot(o, method='graph')



