#chap05_DataVisualization

# 차트 데이터 생성

chart_data <- c(305,450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2014 1분기","2015 1분기","2014 2분기","2015
2분기","2014 3분기","2015 3분기","2014 4분기","2015 4분기")
str(chart_data)
chart_data
max(chart_data) #520




# 1. 이산변수 
# -정수단위로 나누어 측정할 수 있는 변수

# 1) 막대차트
#barplot 정보 : 백터나 매트릭스
# barplot(height, width = 1, space = NULL,
#         names.arg = NULL, legend.text = NULL, beside = FALSE,
#         horiz = FALSE, density = NULL, angle = 45,
#         col = NULL, border = par("fg"),
#         main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
#         xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
#         axes = TRUE, axisnames = TRUE,
#         cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
#         inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0,
#         add = FALSE, args.legend = NULL, ...)


# 년도별로 나누기
chart_s <- sort(chart_data) #정렬 (내림차순 : decreasing=T)
barplot(chart_s,ylim=c(0,600),
        main='2014년vs2015년 판매현황', 
        col=rainbow(8))

# 세로막대차트
barplot(chart_data,ylim=c(0,600),#ylim=ylimit : y축 데이터 범위 설정
        main='2014년vs2015년 판매현황', #표에 이름붙이기
        col=rainbow(8)) #rainbow(x) x는 그래프의 갯수 : 색을 넣을 갯수 
# 가로막대차트
barplot(chart_data,xlim=c(0,600), #x축에 범위설정 해야함
       horiz=T, #가로막대로 변경
       main='2014년vs2015년 판매현황',
       col=rainbow(8))
# ylab/xlab : x축과 y축에 이름 붙이기
barplot(chart_data, ylim=c(0,600), 
        ylab="매출액(단위:만워)", xlab="년도별분기현황",
        col=rainbow(8), main ="2014년도 vs 2015년도 분기별 매출현황 비교")


# 그래프 박스 공간에 차트를 나눔
par(mfrow=c(1,2)) #1행2열 그래프 보기 : 그래프 두개 확인가능
par(mfrow=c(1,1)) #1행1열로 그래프 다시 한개만 보임


# beside 적용 : 하나의 칼럼에 값이 여러개 있는 경우
# beside=F : 누적형 데이터
barplot(VADeaths,beside=F,horiz=F,
        main='버지니아 사망비율',
        col=rainbow(4))
# beside=T : 개별형 데이터
barplot(VADeaths,beside=T,horiz=F,
        main='버지니아 사망비율',
        col=rainbow(4))
# 행 이름 / 열 이름 출력 다시 확인하기 아마 2장? 3장?
#레전드 추가하기 전 행/열 이름 지정
str(VADeaths)
row_names <- row.names(VADeaths)
row_names
col_names <- col.names(VADeaths)
col_names
max(VADeaths) #71.1
# 그래프에 범례추가
legend(20,70, #위치설정
       cex=1, #범례 사이즈 설정
       legend=row_names, #범례에 들어갈 텍스트
       fill=rainbow(4))


###### 추가정보 -> 다른 챕터에서 끌어옴
barplot(x,
        names.arg=c('남자','여자'),
        legend.text=lab,
        col=rainbow(2))





# 2) 점 차트

#dotchart 정보
# dotchart(x, labels = NULL, groups = NULL, gdata = NULL,
#          cex = par("cex"), pt.cex = cex,
#          pch = 21, gpch = 21, bg = par("bg"),
#          color = par("fg"), gcolor = par("fg"), lcolor = "gray",
#          xlim = range(x[is.finite(x)]),
#          main = NULL, xlab = NULL, ylab = NULL, ...)
# 


dotchart(chart_data, 
         color=c("blue","red"), #연도별로 데이터 구분
         lcolor="black", #그래프 색상구분
         pch=2:1, #포인터의 모양 : 동그라미/삼각형 등 : 숫자 바꿔가며 확인하기
         labels=names(chart_data), #각 점에 라벨링
         xlab="매출액",
         main="분기별 판매현황 점 차트 시각화", 
         cex=1.35) #extend : 글자 확대/축소 여부

?dotchart
dotchart(VADeaths, 
         color=c("blue","red"), 
         lcolor="black",
         pch=15,
         xlab="매출액",
         cex=1.1) 



# 3) 파이 차트

#pie 정보
# pie(x, labels = names(x), edges = 200, radius = 0.8,
#     clockwise = FALSE, init.angle = if(clockwise) 90 else 0,
#     density = NULL, angle = 45, col = NULL, border = NULL,
#     lty = NULL, main = NULL, ...)
?pie
pie(chart_data, 
    labels = names(chart_data),
    border='black', 
    col=rainbow(8), 
    cex=1.5)


pie(chart_data, 
    radius=1, #원 크기 조정
    clockwise=T, #시계/반시계방향 조정
    labels = names(chart_data),
    border='black', 
    col=rainbow(8), 
    cex=1.5)


# 차트에 제목추가 : 함수안에 main을 사용해도 되지만 별도로 이름을 추가하는 방법
title("2014~2015년도 분기별 매출현황") 


table(iris$Species)
pie(table(iris$Species),
    col=rainbow(3),
    main='iris 꽃의 종 빈도수')

#라벨링 변경
dotchart(table(iris$Species),
         pch=3:1,
         labels=c(a,b,c),
         col=rainbow(3), 
         main='iris 꽃의 종 빈도수')


# 2. 연속변수 시각화
# - 시간, 길이 등의 연속성을 갖는 변수



# 1) 상그 그래프 시각화
#boxplot : 요약통계를 쉽게 시각화해주는 함수
summary(VADeaths) #요약통계
boxplot(VADeaths)
range(VADeaths[,1])
mean(VADeaths[,1])
#사분위수
quantile(VADeaths[,1])




# 2) 히스토그램 시각화
str(iris)
range(iris$Sepal.Width) #x축의 범위를 잡을때 참조하면 좋음
summary(iris$Sepal.Width)
table(iris$Sepal.Width)

hist(iris$Sepal.Width, 
     xlab="iris$Sepal.Width",
     col="mistyrose", #지정색상
     main="iris 꽃받침 넓이 histogram", 
     xlim=c(2.0, 4.5))

#frequency 함수 사용 유/무
par(mfrow=c(1,2))
par(mfrow=c(1,1))
#freq = T
hist(iris$Sepal.Width, 
     xlab="iris$Sepal.Width",
     col="green",
     main="iris 꽃받침 넓이 histogram", xlim=c(2.0, 4.5))
#freq = F : 세로축이 밀도로 나옴, 1을 기준으로 어느정도 비율을 차지하고 있는지
hist(iris$Sepal.Width, 
     xlab="iris$Sepal.Width",
     col="mistyrose",
     freq = F, 
     main="iris 꽃받침 넓이 histogram", xlim=c(2.0, 4.5))
# 밀도 분포 곡선 추가
# 밀도 차트를 사용할때 추가 가능
lines(density(iris$Sepal.Width),
      col='black')
curve(dnorm(x, mean=mean(iris$Sepal.Width), sd=sd(iris$Sepal.Width)),
      col="blue",
      add = T) #그래프에 선을 추가할건지 단독으로 그래프를 만들건지의 유무

n <- 10000
x <- rnorm(n,mean=0,sd=1)
hist(x,freq=F)
lines(density(x),
      col='red')
curve(dnorm(x, mean=mean(x), sd=sd(x)),
      col="blue",
      add = F)


# 3) 산점도 시각화
x <- runif(n=15,min=1,max=100)
plot(x) #x축은 갯수 : n의 값[인덱스] , y축은 각 n이 가지는 값(x)
y <- runif(n=15,min=5,max=120)
plot(x,y) #plot(y~x)
# color 반영
head(iris,10) #iris[1-4]컬럼 : 연속형 [5]컬럼 : 범주형
plot(iris$Sepal.Length,iris$Petal.Length,
     col=iris$Species)
# 산전도 선 그리기
price<- rnorm(10)# 1~100사이 10개 난수 발생
price #price <-c(1:10)
par(mfrow=c(2,2)) # 2행 2열 차트 그리기
plot(price, type="l") # 유형 : 실선
plot(price, type="o") # 유형 : 원형과 실선(원형 통과)
plot(price, type="h") # 직선
plot(price, type="s") # 꺾은선
# plot() 함수 속성 : pch : 연결점 문자타입-> plotting characher-번호(1~30)
plot(price, type="o", pch=5) # 빈 사각형
plot(price, type="o", pch=15)# 채워진 마름모
plot(price, type="o", pch=20, col="blue") #color 지정
plot(price, type="o", pch=20, col="orange", cex=1.5) #character expension(확대)
plot(price, type="o", pch=20, col="green", cex=2.0, lwd=3) #lwd : line width
# 만능차트
methods(plot)
methods(subset)
# plot.ts : 시계열자료
WWWusage
plot(WWWusage)
# plot.lm* : 회귀모델
install.packages('UsingR')
library(UsingR)
data(galton)
str(galton) #928x2
#유전학자 갈톤 : 회귀 용어 제안
model <- lm(child~parent,data=galton)
plot(model) #plot 3~4개 정도 나옴




# 4) 산점도 행렬 : 변수 간의 비교
str(iris)
pairs(iris[-5])
pairs(iris[-5],col=iris$Species) #범주형을 뺀 나머지 4개의 컬럼 입력
# 꽃의 종별 산점도 행렬
table(iris$Species)
pairs(iris[iris$Species=='setosa',-5])
pairs(iris[iris$Species=='virginica',-5])
par(mfrow=c(1,2))



# 5) 차트를 파일로 저장하기
setwd("C:/IITT/2_Rwork/output") #폴더지정

jpeg("iris.jpg", width=720, height=480,quality=150) #픽셀지정
plot(iris$Sepal.Length, iris$Petal.Length, col=iris$Species)
title(main="iris 데이터 테이블 산포도 차트")
dev.off() 





# 6) 데이터프레임으로 변환 : 컬럼 단위의 데이터 활용을 위해서
freqData <- as.data.frame(table(galton$child, galton$parent))
freqData # Var1 Var2 Freq(중복 수)
str(freqData) # 154 obs(928 관측치가 중복 제외한 154개 관측치 생성 )
names(freqData)=c("child","parent", "freq") # 컬럼에 이름 지정



# 7) 프레임 -> 벡터 -> 수치데이터변환, cex : 빈도수에 0.15 곱(가중치 적)
parent <- as.numeric(as.vector(freqData$parent))
child <- as.numeric(as.vector(freqData$child))
plot(child~parent, pch=21, col="blue", bg="green",
     cex=0.15*freqData$freq, xlab="parent", ylab="child")









#########################
### 3차원 산점도 
#########################
install.packages('scatterplot3d')
library(scatterplot3d) #3d 표를 나타내는 함수

# 꽃의 종류별 분류 
str(iris)
iris_setosa = iris[iris$Species == 'setosa',] #5번째 컬럼의 값이 setosa인 행들 : 50개
iris_versicolor = iris[iris$Species == 'versicolor',] 
iris_virginica = iris[iris$Species == 'virginica',]

# scatterplot3d(밑변, 오른쪽변, 왼쪽변, type='n') # type='n' : 기본 산점도 제외 
d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type='n')

d3$points3d(iris_setosa$Petal.Length, iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, bg='orange', pch=21)

d3$points3d(iris_versicolor$Petal.Length, iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width, bg='blue', pch=23)

d3$points3d(iris_virginica$Petal.Length, iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, bg='green', pch=25)


































