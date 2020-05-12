#################################
## <제8장 연습문제>
################################# 

#01. 다음 조건에 맞게 airquality 데이터 셋의 Ozone과 Wind 변수를 대상으로  
# 다음과 같이 산점도로 시각화 하시오.

#조건1) y축 : Ozone 변수, x축 : Wind 변수 
#조건2) Month 변수를 factor형으로 변경  
#조건3) Month 변수를 이용하여 5개 격자를 갖는 산점도 그래프 그리기

head(airquality)
str(airquality)

airquality$Wind
airquality$Month<-as.factor(airquality$Month)
class(airquality$Month)

air_df<-transform(airquality,Month=factor(Month))
xyplot(Ozone~Wind|Month,data=air_df)

dotplot(Ozone ~ Wind | factor(Month),data=airquality)
xyplot(Ozone ~ Wind | Month,data=airquality)
coplot(Ozone ~ Wind | factor(Month),data=airquality,row=1,col='blue')


# 02. 서울지역 4년제 대학교 위치 정보(Part-II/university.csv) 파일을 이용하여 레이어 형식으로 시각화 하시오.

# 조건1) 지도 중심 지역 SEOUL, zoom=11
# 조건2) 위도(LAT), 경도(LON)를 이용하여 학교의 포인트 표시
# 조건3) 각 학교명으로 텍스트 표시
# 조건4) 완성된 지도를 "university.png"로 저장하기(width=10.24,height=7.68) 

library(ggmap)
setwd('C:/IITT/2_Rwork/Part-II')
university <- read.csv('university.csv')
str(university)
uname <- university$학교명
lat <- university$LAT
lon <- university$LON
df <- data.frame(uname,lat,lon)
seoul <- c(left = 126.85, bottom = 37.40, 
           right = 127.25, top = 37.70)
map <- get_stamenmap(seoul, zoom=11,  maptype='watercolor')
layer <- ggmap(map) 
layer2 <- layer + geom_point(data=df,aes(x=lon,y=lat,
                                         size=factor(uname),
                                         col=factor(uname)))
layer2
layer3 <- layer2 + geom_text(data=df,aes(x=lon,y=lat,
                                         label=uname))
layer3




