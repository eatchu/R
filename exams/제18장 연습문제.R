#################################
## <제18장 연습문제>
################################# 

# 01. iris 데이터 셋의 1~4번째 변수를 대상으로 유클리드 거리 매트릭스를 구하여 
# idist에 저장한 후 계층적 클러스터링을 적용하여 결과를 시각화 하시오.

x <- iris[1:4] # 4개변수 전체


#단계1. 유클리드 거리 계산

dist <- dist(x, method="euclidean")

#단계2. 계층형 군집분석(클러스터링)
hc <- hclust(dist)

#단계3. 분류결과를 대상으로 음수값을 제거하여 덴드로그램 시각화
plot(hc)

#단계4. 그룹수를 4개로 지정하고 그룹별로 테두리 표시
rect.hclust(hc, k=4, border="red")

ghc <- cutree(hc,k=4)
table(ghc)

# 02. 다음과 같은 조건을 이용하여 각 단계별로 비계층적 군집분석을 수행하시오.

# 조건1) 대상 파일 : product_sales.csv
# 조건2) 변수 설명 : tot_price : 총구매액, buy_count : 구매횟수, 
#                    visit_count : 매장방문횟수, avg_price : 평균구매액

setwd("c:/IITT/2_Rwork/Part-IV")
sales <- read.csv("product_sales.csv", header=TRUE)
head(sales) 


# 단계1: 비계층적 군집분석 : 3개 군집으로 군집화
result2 <- kmeans(sales, 3)
result2 

# 단계2: 원형데이터에 군집수 추가
sales$cluster <- result2$cluster
head(sales)

# 단계3 : tot_price 변수와 가장 상관계수가 높은 변수와 군집분석 시각화
cor(sales[,-5], method="pearson")
plot(sales$tot_price, sales$avg_price, col=sales$cluster)

# 단계4. 군집의 중심점 표시
result2$centers
points(result2$centers[,c("tot_price", "avg_price")], col=c(3,1,2), pch=8, cex=5)




