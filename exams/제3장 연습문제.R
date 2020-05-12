#################################
## <제3장 연습문제>
#################################   
# 01. R에서 제공하는 quakes 데이터셋을 대상으로 다음과 같이 처리하시오
data("quakes")
quakes # 지진 진앙지 데이터 셋 
str(quakes)
# 'data.frame':	1000 obs. of  5 variables:

# 단계1) 현재 경로에 row.names, quote 없이 "quakes_df.csv" 파일명으로 저장 
write.csv(quakes,'quakes_df.csv')
# 단계2) quakes_data로 파일 읽기 
quakes_data <- read.csv(file.choose())
quakes_data
head(quakes_data)
# 단계3) mag 변수를 대상으로 평균 계산하기 
mag <- quakes_data$mag
mean(mag)
cat('mag평균은',mean(mag))



# 02. R에서 제공하는 CO2 데이터셋을 대상으로 다음과 같이 파일로 저장하시오.
data("CO2")
CO2 # 잔디 식물의 이산화탄소 흡수 데이터셋  
str(CO2)

# 단계1) Treatment 칼럼 값이 'nonchilled'인 경우만 'CO2_df1.csv' 파일로 저장하기 
co2 <- subset(CO2,Treatment%in%'nonchilled')
write.csv(co2,'CO2_df1.csv',row.names = F,quote=F)

# 단계2) Treatment 칼럼 값이 'chilled'인 경우만 'CO2_df2.csv' 파일로 저장 
co3 <- subset(CO2,Treatment == 'chilled')
write.csv(co3,'CO2_df2.csv',row.names = F,quote=F)

