setwd("C:\\ITWILL\\2_Rwork\\samsung")

library(stringr)

df1 = read.csv('mealData_customer_train.csv', stringsAsFactors = FALSE)
df2 = read.csv('mealData_meal_train2.csv', stringsAsFactors = FALSE)
df3 = read.csv('weather2.csv', stringsAsFactors = FALSE)
temp = c('2018/01/01','[\'평균기온::-1\']','[]') # 빠진 부분에 대해서 하나 추가

df3 = rbind(temp,df3) 
names(df3) = c('SELL_DATE', 'temper', 'rain') #dataframe naming 
df3$SELL_DATE 
df3$SELL_DATE= str_replace_all(df3$SELL_DATE, "/", "-")  #date 똑같은 형태로 변환 join하기 위해

df4<-merge(x=df1,y=df2,by="CUSTOMER_ID") # dataframe join
df<-merge(x=df4,y=df3,by="SELL_DATE")

head(df,100)
df$temper= str_replace_all(df$temper, "\\[\\'평균기온:", "") #data 전처리
df$temper= str_replace_all(df$temper, "\\'\\]", "")
df$rain= str_replace_all(df$rain, "\\[\\'일강수량:", "")
df$rain= str_replace_all(df$rain, "\\'\\]", "")
df$rain= str_replace_all(df$rain, "\\[\\]", "0")

str(df)
df$GENDER = as.factor(df$GENDER)
df$MENU = as.factor(df$MENU)
df$BRAND = as.factor(df$BRAND)
df$temper = as.numeric(df$temper)
df$rain = as.numeric(df$rain)
df$BIRTH_YEAR= 2021 - df$BIRTH_YEAR

table(df$temper)
table(df$SELL_DATE)

#온도 category화
df$temper2[df$temper<=-10]="1.~-10" 
df$temper2[df$temper>-10 & df$temper <=-5]="2.-9~-5"
df$temper2[df$temper>-5 & df$temper <=0]="3.-4~0"
df$temper2[df$temper>0 & df$temper <=5]="4.1~5"
df$temper2[df$temper>5 & df$temper <=10]="5.6~10"
df$temper2[df$temper>10 & df$temper <=15]="6.11~15"
df$temper2[df$temper>15 & df$temper <=20]="7.16~20"
df$temper2[df$temper>20 & df$temper <=25 ]="8.21~25"
df$temper2[df$temper>25 & df$temper <=30 ]="9.26~30"
df$temper2[df$temper>30 ]="9.31~"
head(df)

table(df$temper2)

#--------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
#temp <- df %>% group_by(temper2, BRAND) %>%  tally() 행 갯수로 세는거
# sum으로 세기 전체 총합
temp <- df %>% group_by(temper2)  

brand_sum = aggregate(temp$QUANTITY, by=list(temp$temper2), FUN=sum)
names(brand_sum) = c('temper','sum')
ggplot(brand_sum, aes(temper, sum ))+geom_bar(stat="identity", position="dodge")



# brand별 총합
str(df)
a = table(df$temper2)

temper_brand_sum=df %>%group_by(temper2, BRAND) %>% summarise(a_sum=sum(QUANTITY) )
temper_brand_sum$temp[temper_brand_sum$temper2=='1.~-10']=a[1]
temper_brand_sum$temp[temper_brand_sum$temper2=='2.-9~-5']=a[2]
temper_brand_sum$temp[temper_brand_sum$temper2=='3.-4~0']=a[3]
temper_brand_sum$temp[temper_brand_sum$temper2=='4.1~5']=a[4]
temper_brand_sum$temp[temper_brand_sum$temper2=='5.6~10']=a[5]
temper_brand_sum$temp[temper_brand_sum$temper2=='6.11~15']=a[6]
temper_brand_sum$temp[temper_brand_sum$temper2=='7.16~20']=a[7]
temper_brand_sum$temp[temper_brand_sum$temper2=='8.21~25']=a[8]
temper_brand_sum$temp[temper_brand_sum$temper2=='9.26~30']=a[9]
temper_brand_sum$temp[temper_brand_sum$temper2=='9.31~']=a[10]

ggplot(temper_brand_sum, aes(BRAND, a_sum ),fill=temper2)+geom_bar(stat="identity", position="dodge")
qplot(temper2, a_sum, data=temper_brand_sum, color=factor(BRAND), geom="bar")
ggplot(temper_brand_sum, aes(temper2 , a_sum / temp , color=BRAND),fill = BRAND)+geom_bar(stat="identity", position="dodge")

#ggplot(temp, aes(temper2, n , fill = BRAND))+geom_bar(stat="identity", position="dodge")
ggplot(brand_sum, aes(BRAND, sum ))+geom_bar(stat="identity", position="dodge")

table(df$BRAND)
temp2 = subset(df,BRAND=='탕맛기픈')  
temp2 <- temp2 %>% group_by(temper2) %>%  tally()

ggplot(temp2, aes(temper2, n ))+geom_bar(stat="identity", position="dodge")




str(df)
#plot(df$temper, df$QUANTITY, col=df$BRAND)
#temper_brand_sum=df %>%group_by(temper, BRAND) %>% summarise(a_sum=sum(QUANTITY))
subset(df,BRAND=='KOREAN1')
temper_brand_sum=subset(df,BRAND=='KOREAN1') %>%group_by(temper) %>% summarise(a_sum=sum(QUANTITY))
#plot(temper_brand_sum$temper, temper_brand_sum$a_sum, col=temper_brand_sum$BRAND)
plot(temper_brand_sum$temper, temper_brand_sum$a_sum)

library (dplyr)
ggplot(temper_brand_sum, aes(x = temper, y = a_sum)) + geom_bar (stat="identity")
library(lattice)
histogram(temper_brand_sum$temper, temper_brand_sum$a_sum)

#------------------
#사원별 구매 횟수
library (dplyr)
length(unique(setdiff (df1$CUSTOMER_ID, df$CUSTOMER_ID))) #230명은 사원이지만 한번도 안먹엇다
length(unique(df$CUSTOMER_ID)) # 10570
people_eat = df %>% group_by(CUSTOMER_ID) %>% summarise(sum(QUANTITY))
names(people_eat) = c('CUSTOMER_ID', 'QUANTITY')
arrange(people_eat, desc(QUANTITY)) #정렬
mean(people_eat$QUANTITY) #97.57379
t = table(people_eat$QUANTITY>=100) #평균 보다 높게 잡았다.
people_eat$CUSTOMER_ID[people_eat$QUANTITY>=100]
unique(setdiff (df1$CUSTOMER_ID, df$CUSTOMER_ID))
a=c(230,10570)
names(a)=c('취식 안한 사람\n230','취식 한 사람\n10570')
pie(a)

df$samsung = df$CUSTOMER_ID %in% people_eat$CUSTOMER_ID[people_eat$QUANTITY>=100]
head(df)

samsung = subset(df,samsung==TRUE) # 단골
not_samsung = subset(df,samsung==FALSE) # 신규

#--------------------
str(df)
dim(df)
unique(df$rain)
table(df$rain==0)
# FALSE   TRUE 
# 267297 756176

#round (pi, digits = 40)
head(rain_brand_sum,20)
rain_brand_sum=df %>%group_by(비) %>% summarise(a_sum=sum(QUANTITY) )
# 762062
sum(rain_brand_sum$a_sum)
#1031355
# 비가 온날의 판매량  1031355 - 762062 = 269293
length(unique(df$SELL_DATE[df$rain==0]))#306
length(unique(df$SELL_DATE[df$rain!=0]))#104

rain = subset(df , rain==0)
not_rain = subset(df, rain!=0)
rain = rain %>% group_by(SELL_DATE) %>%summarise(a_sum=sum(QUANTITY))
not_rain = not_rain %>% group_by(SELL_DATE) %>%summarise(a_sum=sum(QUANTITY))
plot(df$SELL_DATE,c(rain$a_sum,not_rain$a_sum))
t = c(x,y)
plot(t, xlim=c(1,2) , ylim = c(2000,2800))
m = matrix(c(x,y),nrow=2)
barplot(m,beside=T,names=c("Rain_MEAN_ONEDAY", "Not-Rain_MEAN_ONEDAY"),col=c(2,3),ylim=c(2450,2600))
# 비가 온날의 판매량 / 비가 왔던 날 갯수
# 269293 / 104 = 2589.3557692307691
# 비가 오지 않는 날의 판매량 / 비가 오지 않는 날 갯수
# 762062 / 306 = 2490.3986928104573
x = 2589.3557692307691
y = 2490.3986928104573
library(lattice)
barplot(c(x,y),names())

KOREAN = subset(df , BRAND=='KOREAN1')
head(KOREAN)
korean1_rain_sum=KOREAN %>%group_by( rain) %>% summarise(a_sum=sum(QUANTITY) )
length(unique(df$SELL_DATE[df$rain==0&df$BRAND=='KOREAN1']))
length(unique(df$SELL_DATE[df$rain!=0&df$BRAND=='KOREAN1']))
# 비가 오지 않는 날의 판매량 = 102550
# 비가 오지 않는 날 = 254
# 102550 / 254 = 403.74015748031496
sum(korean1_rain_sum$a_sum)
# 비가 온날의 판매량 138622 - 102550 = 36072
# 비가 온날 = 88
# 36072 / 88 = 409


TakeOut = subset(df , BRAND=='TakeOut')
head(TakeOut)
takeout_rain_sum=TakeOut %>%group_by( rain) %>% summarise(a_sum=sum(QUANTITY) )
length(unique(df$SELL_DATE[df$rain==0&df$BRAND=='TakeOut']))
length(unique(df$SELL_DATE[df$rain!=0&df$BRAND=='TakeOut']))
# 비가 오지 않는 날의 판매량 = 121140
# 비가 오지 않는 날 = 254
# 121140 / 254 = 476.92913385826773
sum(takeout_rain_sum$a_sum)
# 비가 온날의 판매량 163706 - 121140 = 42566
# 비가 온날 = 88
# 42566 / 88 = 483.70454545454544


#----------------------
#     CUSTOMER_ID QUANTITY
# 1     2061390      791
# 2     2147028      717
# 3     2136400      608
# 4     2044358      579
# 5     2256444      575
# 6     2677386      568
# 7     2272739      544
# 8     2718042      540
# 9     2577611      527
# 10     2526086      514

#단골(100번 이상 먹은 사람들)의 개인별 브랜드 선호도(max value)
people_eat = samsung %>% group_by(CUSTOMER_ID) %>% summarise(sum(QUANTITY)) 
names(people_eat) = c('CUSTOMER_ID', 'QUANTITY')
custom_data = arrange(people_eat, desc(QUANTITY))$CUSTOMER_ID[1:4191 ]
dim(people_eat) #4191 

for(i in 1:length(custom_data)){
  custom1 = subset(samsung, CUSTOMER_ID==custom_data[i])
  t = table(custom1$BRAND)
  best=names(t)[t == max(t)]
  print(최고)
  samsung$brand_prefer[samsung$CUSTOMER_ID==custom_data[i]]=best
} 
tab=table(samsung$brand_prefer)

samsung$brand_prefer
pie(tab ,col=rainbow(length(tab)) ,labels=piepercent ,radius=1.6)
legend("right",legend=names(tab),fill = rainbow(length(tab)),cex = 0.9)
# Chef`sCounter       KOREAN1       KOREAN2       TakeOut       Western 
# 32321        193751        122749        275152         11373 
# 가츠엔  고슬고슬비빈    나폴리폴리      스냅스낵    싱푸차이나 
# 33745         33218         35774          6388         66637 
# 아시안픽스    우리미각면      탕맛기픈 
# 6199         36216         37376 

piepercent<- round(100*tab/sum(tab), 1)

summary(samsung)
prop.table(table(samsung$brand_prefer))*100
# Chef`sCounter              KOREAN1              KOREAN2              TakeOut 
# 3.62790843855476330 21.74780755169777891 13.77810503772032646 30.88475798042202314 
# Western               가츠엔         고슬고슬비빈           나폴리폴리 
# 1.27657568366335572  3.78774698366481521  3.72859325243377748  4.01549446121277498 
# 스냅스낵           싱푸차이나           아시안픽스           우리미각면 
# 0.71702852960885577  7.47974798490064519  0.69581400360759194  4.06510726805170997 
# 탕맛기픈 
# 4.19531282446158293 

#비단골 개인별 브랜드 선호도
#단골(100번 이상 먹은 사람들)의 개인별 브랜드 선호도(max value)
people_eat = not_samsung %>% group_by(CUSTOMER_ID) %>% summarise(sum(QUANTITY)) 
names(people_eat) = c('CUSTOMER_ID', 'QUANTITY')
custom_data = arrange(people_eat, desc(QUANTITY))$CUSTOMER_ID[1:6379 ]
dim(people_eat)#6379

for(i in 1:length(custom_data)){
  custom1 = subset(not_samsung, CUSTOMER_ID==custom_data[i])
  t = table(custom1$BRAND)
  best=names(t)[t == max(t)]
  print(최고)
  not_samsung$brand_prefer[not_samsung$CUSTOMER_ID==custom_data[i]]=best
} 
tab = table(not_samsung$brand_prefer)

# Chef`sCounter       KOREAN1       KOREAN2       TakeOut       Western 
# 4214         22707         19117         25789          4590 
# 가츠엔  고슬고슬비빈    나폴리폴리      스냅스낵    싱푸차이나 
# 12833          4612          6541          2872         11224 
# 아시안픽스    우리미각면      탕맛기픈 
# 2928          6158          8989 
pie(tab ,col=rainbow(length(tab)) ,labels=piepercent ,radius=1.6)
piepercent<- round(100*tab/sum(tab), 1)
legend("right",legend=names(tab),fill = rainbow(length(tab)),cex = 0.9)

summary(not_samsung)
prop.table(table(not_samsung$brand_prefer))*100
# Chef`sCounter             KOREAN1             KOREAN2 
# 3.1786021391826451 17.1277927798814247 14.4198711662920331 
# TakeOut             Western              가츠엔 
# 19.4525321707122067  3.4622173276811443  9.6798768989394599 
# 고슬고슬비빈          나폴리폴리            스냅스낵 
# 3.4788118333911626  4.9338482658741540  2.1663372908715131 
# 싱푸차이나          아시안픽스          우리미각면 
# 8.4662150949658308  2.2085778508606513  4.6449530073770120 
# 탕맛기픈 
# 6.7803641739707636 

#------------------------------------------------------------------

# 위 값을 이용하여 일반적인 갯수를 맞추어보자.
# 2018/1/1 부터 하루에 몇명 정도 먹는지 + 단골은 몇명 + 비 단골은 몇명
# 먹는 사람들 중 비율은 어떻게 되는지
# 하루 먹는 사람들중 단골 vs 비단골 나이 ,성별 비율
# samsung data에서 2018/1/1에 TAKE OUT한사람 실제 data와 예측 data
# TAKE OUT = 460 / KOREAN1 = 404 / 고슬고슬 = 218

df %>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY) )
str(df)
temp=subset(df , SELL_DATE=="2018-01-02")
dim(temp) #3126 
temp2 = df %>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
temp2

#토요일 제거
dim(df) # 1023473
df$day <- weekdays(as.Date(df$SELL_DATE))
table(df$day)
df = df[!(df$day == "토요일" ), ]
table(df$day)

#일일 평균
temp2 = df %>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
mean(temp2$a_sum, na.rm = FALSE) #3001.211
max(temp2$a_sum) #3626 "2019-05-20"
min(temp2$a_sum) #1538 "2018-12-31"
length(temp2$a_sum) #342
plot(temp2$a_sum)
summary(temp2$a_sum)
boxplot(temp2$a_sum)$stats #2379 ~ 3626 이상치 제거
temp2 = subset(temp2, a_sum>=2379 & a_sum<=3626)
mean(temp2$a_sum, na.rm = FALSE) #3028

3028
#단골의 일일 평균
samsungman = samsung%>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
mean(samsungman$a_sum, na.rm = FALSE)
boxplot(samsungman$a_sum)$stats #1871.0 ~ 3187.0
samsungman = subset(samsungman , a_sum>=1871 & a_sum<=3187)
mean(samsungman$a_sum) #2634.551

t = c(2634,325)
names(t) = c('단골\n 88%','신규\n12%')
pie(t , col=rainbow(length(t)),radius=1.6)
pie(tab ,col=rainbow(length(tab)) ,labels=piepercent ,radius=1.6)
piepercent<- round(100*tab/sum(tab), 1)
legend("right",legend=names(tab),fill = rainbow(length(tab)),cex = 0.9)
#비단골의 일일 평균
nonsamsungman = not_samsung %>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
mean(nonsamsungman$a_sum, na.rm = FALSE) #325.0756
boxplot(nonsamsungman$a_sum)$stats #10 ~ 672



#TAKEOUT 일일 평균
temp3 = subset(df,BRAND=="TakeOut")
temp3 = temp3%>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
temp3 # 399
boxplot(temp3$a_sum)$stats #307.0 ~ 633.0
temp3 = subset(temp3 , a_sum>=307 & a_sum<=633)
mean(temp3$a_sum) # 479.9353


temp2 = df %>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
dim(temp2)
dim(temp3)
str(temp2)
EVERYDAY_TAKEOUT=temp3$a_sum/temp2$a_sum
EVERYDAY_TAKEOUT
min(EVERYDAY_TAKEOUT)
max(EVERYDAY_TAKEOUT)
boxplot(EVERYDAY_TAKEOUT)
boxplot(EVERYDAY_TAKEOUT)$stats #0.1243988 ~ 0.1938012

# 하루 몇명 먹는지를 맞추는게 우선일듯 찾아보자자
temp2 = df %>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
plot(temp2$a_sum)

#시계열 data
install.packages('forecast')
library(forecast)
fit = auto.arima(temp2$a_sum)
fcast = forecast(fit,h=30)
plot(fcast)

fcast
df4 = read.csv('mealData_meal_test.csv', stringsAsFactors = FALSE)
str(df4)
test = df4 %>% group_by(SELL_DATE)%>%summarise(a_sum=sum(QUANTITY))
test



#------------
#pd.date_range("2018-4-1", "2018-4-30", freq="B")
start_date <- as.Date("1950-01-01")
end_date <- as.Date("2018-12-31")

library(TTR)
library(forecast)
table(df$BRAND)
temp = subset(df , BRAND=='스냅스낵')
temp = temp %>% group_by(SELL_DATE) %>% summarise(a_sum=sum(QUANTITY))
dim(temp)
length(AirPassengers)
AirPassengers
temp
temp$a_sum
myts <- ts(temp$a_sum, start=c(2018, 1,1), end = c(2019,5,24), frequency=30)
myts

fit <- stl(myts, s.window=12)
plot(fit)
monthplot(myts)

#arima(myts, order=c(p, d, q))
auto.arima(myts)             
fit <- arima(myts, order=c(3,1,1))
fit <- arima(myts, order=c(2,0,0))
fit <- arima(myts, order=c(0,1,1))
plot(forecast(fit,5))
forecast(fit,10)

df5 = subset(df4, BRAND=='스냅스낵')
df5 = df5 %>% group_by(SELL_DATE) %>% summarise(a_sum=sum(QUANTITY))
df5

forecast(fit,10)
str(df)

#시계열 데이터 전처리
# 1.날짜 변수 통일
# 2.불연속 시계열 데이터 전처리
# 3.시계열 data 거리 계산

library(dplyr)
str(df)
samsungtime = data.frame(df$SELL_DATE , df$BRAND , df$QUANTITY)
names(samsungtime) = c('SELL_DATE','BRAND','QUANTITY')
samsungtime$SELL_DATE = as.Date(samsungtime$SELL_DATE)
samsungtime$BRAND = as.character(samsungtime$BRAND)
str(samsungtime)

samsungtime2 = samsungtime%>%group_by(BRAND)
table(samsungtime2$BRAND)

ChefsCounter = subset ( samsungtime2 , BRAND == "Chef`sCounter" ) 
ChefsCounter = ChefsCounter %>%group_by(SELL_DATE)%>% summarise(sum=sum(QUANTITY))
KOREAN1 = subset ( samsungtime2 , BRAND == "KOREAN1" ) 
KOREAN1 = KOREAN1 %>%group_by(SELL_DATE)%>% summarise(sum=sum(QUANTITY))
KOREAN2 = subset ( samsungtime2 , BRAND == "KOREAN2" ) 
KOREAN2 = KOREAN2 %>%group_by(SELL_DATE)%>% summarise(sum=sum(QUANTITY))
TakeOut = subset ( samsungtime2 , BRAND == "TakeOut" ) 
TakeOut = TakeOut %>%group_by(SELL_DATE)%>% summarise(sum=sum(QUANTITY))

plot(ChefsCounter$SELL_DATE,ChefsCounter$sum,type='l',col=1,ylim=c(0,800))
lines(KOREAN1$SELL_DATE,KOREAN1$sum,type='l',col=2) 
lines(KOREAN2$SELL_DATE,KOREAN2$sum,type='l',col=3) 
lines(TakeOut$SELL_DATE,TakeOut$sum,type='l',col=4) 

ChefsCounter$sum
BRAND = c("ChefsCounter","KOREAN1","KOREAN2","TakeOut")
t=data.frame(BRAND)
t$array <- list(c(ChefsCounter$sum,KOREAN1$sum,KOREAN2$sum, TakeOut$sum))
str(sum_data)
