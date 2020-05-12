#################################
## <제5장 연습문제>
################################# 

# 01. 다음 Bug_Metrics_Software 데이터셋을 이용하여 시각화하시오. 
library(RSADBE) # 패키지 로드  
data(Bug_Metrics_Software) # 데이터셋 로드 
str(Bug_Metrics_Software) # 데이터셋 구조보기 

# 단계1) 소프트웨어 발표 전 버그 수를 대상으로 각 소프트웨어별 버그를
# beside형 세로막대 차트로 시각화하기(각 막대별 색상적용) 
barplot(Bug_Metrics_Software[,,1],
        beside=T,
        ylim=c(0,17000),
        ylab='버그 수',xlab='버그이름',
        col=rainbow(5),
        main='소프트웨어 발표 전 버그 수')

row_names <- row.names(Bug_Metrics_Software)
legend(x=20,y=15000,
       legend=row_names,
       fill=rainbow(5))


# 단계2) 소프트웨어 발표 후 버그 수를 대상으로 각 소프트웨어별 버그를
# 누적형 가로막대 차트로 시각화하기(각 막대별 색상적용) 
barplot(Bug_Metrics_Software[,,1],
        beside=F,
        horiz=T,
        xlim=c(0,35000),
        ylab='버그 수',xlab='버그이름',
        col=rainbow(5),
        main='소프트웨어 발표 전 버그 수')



# 02. quakes 데이터 셋을 대상으로 다음 조건에 맞게 시각화 하시오.
data(quakes) # 데이터셋 로드  
str(quakes) # 데이터셋 구조보기 
par(mfrow=c(1,1))

# 조건1) 1번 칼럼 : y축, 2번 컬럼 : x축 으로 산점도 시각화
plot(quakes$long,quakes$lat)


# 조건2) 5번 컬럼으로 색상 지정
plot(quakes$long,quakes$lat,
     col=quakes$stations)


# 조건3) "지진의 진앙지 산점도 차트" 제목 추가  
title('지진의 진앙지 산점도 차트')

plot(quakes$long,quakes$lat,
     col=quakes$stations,
     main='지진의 진앙지 산점도 차트')

# 조건4) "quakes.jpg" 파일명으로 차트 저장하기
# 작업 경로 : "C:/Rwork/output"
# 파일명 : quakes.jpg
 #픽셀 : 폭(720픽셀), 높이(480 픽셀)
jpeg('quakes.jpg',width=720,height = 480)
plot(quakes$long,quakes$lat,
     col=quakes$stations)
title('지진의 진앙지 산점도 차트')
dev.off() 

# 03. iris3 데이터 셋을 대상으로 다음 조건에 맞게 산점도를 그리시오.

# 조건1) iris3 데이터 셋의 자료구조 확인 : 힌트) str() 
str(iris3)
head(iris3,10)

# 조건2) Setosa 꽃의 종을 대상으로 x축은 "Sepal W." 칼럼, 
#        y축은 "Sepal L." 칼럼으로 산점도 그리기 
plot(iris3[,2,1],iris3[,1,1])


# 조건3) "Versicolor" 꽃의 종을 대상으로 산점도 행렬 시각화하기  
pairs(iris3[,,2])

