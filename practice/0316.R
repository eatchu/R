





###################################
###우리집 지도찾아서 사진 저장####
##################################

home <- c(left=126.949,bottom=37.478,
          right=126.953,top=37.482)
ex_home <- data.frame(위도=126.951530,경도=37.479715,이름='두의집')


map <- get_stamenmap(home,zoom=15,maptype = 'terrain')
layer<-ggmap(map)
layer2<-layer+geom_point(data=ex_home,aes(x=위도,y=경도,col=이름),size=10)
layer2+geom_text(data=ex_home,aes(x=위도,y=경도+0.0003,label=이름),size=7,
                 col='blue')
                     
setwd('C:/IITT/2_Rwork/output')
ggsave('dueuihome.png')




#########################################
######내가 보고 싶은 데이터 자료에####### 
#####어떤 그래프를 사용하면 좋을지####### 
#########################################









######################################################
##############그래프에 정확한 수치 넣기###############
############최댓값,최솟값 수치 입력하는법#############
######################################################







####################################################
convert <- transform(quakes, depth2=factor(depth2))
########tansform(x,y=factor(y)) -> as.factor(y)#####




##################################################
####점 말고 선형 그래프로 표현하기 ###############
# 동일한 패널에 2개의 y축에 값을 표현
# xyplot(y1+y2 ~ x | 조건, data, type, layout)
str(airquality)
xyplot(Ozone ~ Wind | factor(Month), data=airquality,
       col=c('blue','red'),layout=c(5,1))
xyplot(Ozone + Solar.R ~ Wind + Temp | factor(Month), data=airquality,
       col=rainbow(4),layout=c(5,1))






#############################################
####그래프 범위 더 자세하게 보고싶을때 ######
##########x축 y축############################
qplot(hwy, data=mpg) 





##########################################
# 1. lattice 패키지
#########################################
# The Lattice Plotting System 
# 격자 형태의 그래픽(Trellis graphic) 생성 패키지
# 다차원 데이터를 사용할 경우, 한 번에 여러개의 plot 생성 가능
# 높은 밀도의 plot를 효과적으로 그려준다.

# lattice 패키지의 주요 함수
# xyplot(), barchart(), dotplot(),  cloud(), 
# histogram(), densityplot(), coplot()
###########################################





###########################################
# 2. ggplot2 패키지
###########################################
# ggplot2 그래픽 패키지
# 기하학적 객체들(점,선,막대 등)에 미적 특성(색상, 모양,크기)를 
# 맵핑하여 플로팅한다.
# 그래픽 생성 기능과 통계변환을 포함할 수 있다.
# ggplot2의 기본 함수 qplot()
# geoms(점,선 등) 속성, aes(크기,모양,색상) 속성 사용
# dataframe 데이터셋 이용(변환 필요)
###########################################
####################################################
#########ggplot이랑 geom_bar 사용하는방법###########
####################################################
library(ggplot2)

# 미적 객체 생성
obj <- ggplot(data=new_data,
              aes(x=job2,fill=age2))
# 막대 차트 추가
obj + geom_bar()
# 밀도 1 기준으로 막대 차트 추가
obj + geom_bar(position='fill')
# 결측치 값까지 확인 가능한 교차 테이블
table(new_data$job2,new_data$age2,useNA='ifany') 

