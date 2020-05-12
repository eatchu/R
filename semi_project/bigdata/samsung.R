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
rain_brand_sum=df %>%group_by(rain) %>% summarise(a_sum=sum(QUANTITY) )
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
  print(best)
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
  print(best)
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


str(df)
answer =df%>%group_by(SELL_DATE) %>% summarise(sum = sum(QUANTITY))
temp = subset(df , GENDER=='남')
str(temp)
temp = temp%>%group_by(SELL_DATE) %>% summarise(sum = sum(QUANTITY))
temp2 = subset(df , GENDER=='여')
temp2 = temp2%>%group_by(SELL_DATE) %>% summarise(sum = sum(QUANTITY))
data.frame(answer$SELL_DATE, answer$sum , temp$sum,temp2$sum,temp3$rain)
temp3 = df%>%group_by(SELL_DATE)
length(temp3$rain)
answer = data.frame(answer$SELL_DATE, answer$sum , temp$sum, temp2$sum,temp3$rain)
lm(AF7~.,data=train[-1])

#---------------------------------
# 요일과의 상관관계 
# 요일별 판매량 분석

########################### 요일별 판매량 ################################
day <- table(data$date)
day
# 금요일 목요일 수요일 월요일 토요일 화요일 
# 197262 203623 198103 215278   4866 204515

barplot(day, ylim = c(0,220000), main = "요일별 식당이용현황", col = rainbow(6))

pie(day,main = "요일별 식당이용현황", col = rainbow(6))


###################### 요일별 브랜드 판매량 ##############################
tab1 <- table(data$BRAND, data$date)
#               금요일 목요일 수요일 월요일 토요일 화요일
# Chef`sCounter   7529   7676   5868   8232      0   7028
# KOREAN1        25143  26131  27275  33731      0  26160
# KOREAN2        21024  25654  23716  20738   4866  22394
# TakeOut        28493  32481  30519  33321      0  32513
# Western        10320  10164   9897  10064      0  10561
# 가츠엔         15873  13655  15369  16579      0  16156
# 고슬고슬비빈   12468  15476  13772  17662      0  15229
# 나폴리폴리     12379  11623  10882  11592      0  11262
# 스냅스낵        5714   6914   4228   2414      0   3705
# 싱푸차이나     19171  17906  18133  19139      0  19217
# 아시안픽스      9860   6618   9541  10110      0   9669
# 우리미각면     14828  14384  13291  13985      0  13901
# 탕맛기픈       14460  14941  15612  17711      0  16720

tab2 <- table(data$date, data$BRAND)
tab2
#        Chef`sCounter KOREAN1 KOREAN2 TakeOut Western 가츠엔 고슬고슬비빈 나폴리폴리 스냅스낵 싱푸차이나 아시안픽스 우리미각면 탕맛기픈
# 금요일          7529   25140   21022   28493   10319  15863        12468      12375     5713      19166       9853      14828    14458
# 목요일          7673   26127   25651   32481   10162  13649        15475      11620     6914      17900       6615      14382    14937
# 수요일          5868   27271   23712   30518    9897  15365        13768      10879     4227      18133       9541      13291    15607
# 월요일          8232   33725   20731   33321   10060  16574        17661      11587     2414      19136      10108      13985    17704
# 토요일             0       0    4864       0       0      0            0          0        0          0          0          0        0
# 화요일          7028   26159   22386   32513   10560  16151        15226      11262     3704      19212       9667      13898    16715

barplot(tab2, beside = T, horiz = F, col = rainbow(6), 
        main = "요일별 식당이용현황", legend = row.names(tab2))

mosaicplot(tab2, main = "요일별 식당이용현황", col = rainbow(6))

barchart(tab2, main = "요일별 식당이용현황")

###################### 요일, 브랜드, 성별 판매량 #########################
x11()
# 요일별 판매량을 브랜드와 성별로 분류
densityplot(~ date2 | factor(BRAND),
            groups = GENDER, 
            data = data, auto.key = T)

# 브랜드별 판매량을 요일과 성별로 분류
densityplot(~ BRAND2 | factor(date),
            groups = GENDER,
            data = data, auto.key =  T)


# 단어구름


# 메뉴별

dmenu <- as.character(data$MENU)
dmenu
str(dmenu)
mode(dmenu)

tab <- table(dmenu)
str(tab)

tab2 <- as.data.frame(tab)

tab3 <- arrange(tab2,desc(tab2$Freq))

tab4 <- filter(tab3, Freq >= 2500)
tab4
str(tab4)
mode(tab4)

# 2000 -> 164
# 3000 -> 104

pal <- brewer.pal(12,"Paired")
windowsFonts(malgun=windowsFont("서울남산체B"))
x11()
wordcloud(tab4$dmenu, tab4$Freq, scale = c(4,0.03), 
          min.freq = 2, random.order = F,
          rot.per = 0.005, colors = pal, family="malgun")



# 브랜드별 
dbrand <- as.character(data$BRAND)
tab5 <- table(dbrand)
tab6 <- as.data.frame(tab5)
tab7 <- arrange(tab6, desc(tab6$Freq))
tab7
str(tab7)
mode(tab7)

x11()
wordcloud(tab7$dbrand, tab7$Freq, scale = c(5,0.05), 
          min.freq = 2, random.order = F,
          rot.per = 0.01, colors = pal, family="malgun")


# 연관분석


#install.packages("arules")
library(arules)


transdata <- data[,c(1,4)]
transdata

write.csv(transdata, "C:\\Users\\ymh09\\OneDrive\\바탕 화면\\ITWILL\\brightics_academy/transdata.csv",
          quote = F, row.names = F)

tdata <- read.transactions("transdata.csv", format = "single",sep = ",", cols = c(1,2), rm.duplicates = F, 
                           encoding = "UTF-8")
tdata
# transactions in sparse format with
# 10569 transactions (rows) and
# 560 items (columns)
str(tdata)

#inspect(tdata)

#head(inspect(tdata))
inspect(head(tdata))

summary(tdata)
# transactions as itemMatrix in sparse format with
# 10570 rows (elements/itemsets/transactions) and
# 561 columns (items) and a density of 0.09228166 
# 
# most frequent items:
#   부대찌개&라면사리        옛날자장면      해물자장덮밥      병천순대국밥          비지찌개           (Other) 
# 3719              3575              3535              3401              3243            529736 
#
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    5.00   34.00   51.77   97.00  188.00 
#
# includes extended item information - examples:
#   labels
# 1           [특식]닭다리짬뽕
# 2 [특식]돈목살스테이크비빔밥
# 3          [특식]보쌈&막국수
# 
# includes extended transaction information - examples:
#   transactionID
# 1       2001331  # 233
# 2       2001337  # 84
# 3       2001387  # 160


# # supp=0.1, conf=0.6, maxlen=2
# trdata <- apriori(tdata, parameter = list(supp=0.1, conf=0.6, maxlen=2))
# 
# inspect(sort(trdata, by="lift")) # 354개 규칙
# 
# trdata2 <- sort(trdata, decreasing = T, by="confidence")
# inspect(trdata2)
# 
# 
# #install.packages("arulesViz")
# library(arulesViz)
# x11()
# plot(trdata2, method = "graph")
# x11()
# plot(trdata2)


# supp=0.1, conf=0.65, maxlen=2
trdata <- apriori(tdata, parameter = list(supp=0.1, conf=0.65, maxlen=2))

inspect(sort(trdata, by="lift")) # 110개 규칙

trdata2 <- sort(trdata, decreasing = T, by="confidence")
inspect(trdata2)


#install.packages("arulesViz")
library(arulesViz)
x11()
plot(trdata2, method = "graph")
x11()
plot(trdata2)




#       lhs                           rhs                     support   confidence lift    count
# [1]   {HOTTOGO}                  => {김밥}                  0.1195950 0.8149581  2.711994 1264 
# [2]   {HOTTOGO}                  => {한식도시락}            0.1169458 0.7969052  3.297765 1236 
# [3]   {쌈밥}                     => {김밥}                  0.1468445 0.7641556  2.542935 1552 
# [4]   {삼치구이}                 => {갈치구이}              0.1162835 0.7633540  3.014906 1229 
# [5]   {3종선택과일}              => {김밥}                  0.1083357 0.7613032  2.533443 1145 
# [6]   {삼치구이}                 => {고등어구이}            0.1149588 0.7546584  3.083102 1215 
# [7]   {사천자장면}               => {옛날자장면}            0.2182799 0.7487829  2.213674 2307 
# [8]   {3종선택과일}              => {샐러드팩}              0.1058757 0.7440160  3.224069 1119 
# [9]   {치즈순두부찌개}           => {부대찌개&라면사리}     0.1394645 0.7381072  2.097622 1474 
# [10]  {간자장면}                 => {옛날자장면}            0.2073044 0.7335119  2.168528 2191 
# [11]  {한식도시락}               => {김밥}                  0.1760810 0.7286609  2.424817 1861 
# [12]  {헬스기빙도시락(음료포함)} => {샐러드팩}              0.1194058 0.7277970  3.153787 1262 
# [13]  {헬스기빙도시락(음료포함)} => {김밥}                  0.1192166 0.7266436  2.418103 1260 
# [14]  {돈육김치찌개}             => {부대찌개&라면사리}     0.1672817 0.7210440  2.049130 1768 
# [15]  {순두부찌개}               => {부대찌개&라면사리}     0.1168512 0.7192778  2.044111 1235 
# [16]  {나가사키부대찌개}         => {부대찌개&라면사리}     0.1677548 0.7178138  2.039950 1773 
# [17]  {돼지국밥}                 => {병천순대국밥}          0.1708771 0.7143987  2.220077 1806 
# [18]  {고추잡채덮밥}             => {해물자장덮밥}          0.1119311 0.7139409  2.134552 1183 
# [19]  {청량고추된장찌개}         => {부대찌개&라면사리}     0.1357744 0.7107479  2.019869 1435 
# [20]  {오삼불고기}               => {부대찌개&라면사리}     0.1048349 0.7107120  2.019767 1108
# [21]  {꽃게짬뽕}                 => {옛날자장면}            0.1144858 0.7084309  2.094379 1210 
# [22]  {샐러드팩}                 => {김밥}                  0.1632132 0.7072571  2.353589 1725 
# [23]  {바지락된장찌개}           => {부대찌개&라면사리}     0.1284890 0.7069235  2.009001 1358 
# [24]  {착즙주스(프리미엄)}       => {김밥}                  0.1117419 0.7067624  2.351943 1181 
# [25]  {제육돌솥비빔밥}           => {부대찌개&라면사리}     0.1072003 0.7063591  2.007397 1133 
# [26]  {오징어볶음&소면}          => {부대찌개&라면사리}     0.1411676 0.7041057  2.000993 1492 
# [27]  {육개장}                   => {부대찌개&라면사리}     0.1540354 0.7035436  1.999396 1628 
# [28]  {스무디}                   => {김밥}                  0.1212035 0.7015334  2.334542 1281 
# [29]  {참치김치찌개}             => {부대찌개&라면사리}     0.1942473 0.7011612  1.992625 2053 
# [30]  {순두부찌개}               => {비지찌개}              0.1131611 0.6965638  2.270115 1196

#------------
# Brightics Academy

# 라이브러리 (실행 시 한 번만 하면 됨)
{
  library(DBI)
  library(RJDBC)
  library(dplyr)
  
  drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
            "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")
  conn<-dbConnect(drv, "jdbc:oracle:thin:@//127.0.0.1:1521/xe","scott","tiger")
}


# 파일 읽기 & DB에 넣기 (최초 한 번만 하면 됨)
{
  path <- "C:/ITWILL/2_Rwork/brightics_academy/"
  
  # 안되면 메모장 켜서 다른 이름으로 저장(인코딩 ANSI로)
  # R studio가 UTF-8을 제대로 지원 안하는 것 같음
  customer_train <- read.csv(paste0(path, "mealData_customer_train.csv"))
  meal_train <- read.csv(paste0(path, "mealData_meal_train.csv"))
  
  dbWriteTable(conn, name = 'meal_customer_train', value = customer_train)
  dbWriteTable(conn, name = 'meal_meal_train', value = meal_train)
  
  customer_test <- read.csv(paste0(path, "mealData_customer_test.csv"))
  meal_test <- read.csv(paste0(path, "mealData_meal_test.csv"))
  
  dbWriteTable(conn, name = 'meal_customer_test', value = customer_test)
  dbWriteTable(conn, name = 'meal_meal_test', value = meal_test)
}

idx <- sample(nrow(result2), nrow(result2)*0.5)
result_sample <- result[idx,]

model <- multinom(BRAND2 ~., data = result_sample)
model$fitted.values


avg <- mean(train_origin$NUM) 232.6784
sctDay <- c('2018-03-02','2018-05-21','2018-10-08','2018-12-24','2018-12-31') # 샌드위치 데이
# 2000개 미만 "2018-03-02" "2018-04-30" "2018-05-21" "2018-10-08" "2018-12-24" "2018-12-31" # 5월 1일은 근로자의 날


result <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) num
                                          FROM meal_customer_train NATURAL JOIN meal_meal_train
                                          GROUP BY sell_date
                                          HAVING SUM(quantity) > 1500
                                          ORDER BY sell_date"))
sctDay <- result$SELL_DATE[result$NUM < 1850]
sctAvg <- mean(result$NUM[result$NUM < 1850])
sctAvg <- mean(result$NUM)

result <- dbGetQuery(conn, "select * from meal_customer_train NATURAL JOIN meal_meal_train")

result2 <- dbGetQuery(conn, "select * from meal_customer_train NATURAL JOIN meal_meal_train where quantity > 2")


result2 <- result[,c(result$PRICE)]
result2 <- subset(result, select = c(BRAND2, GENDER2, BIRTH_YEAR, SELL_DATE, PRICE))

sort(result, decreasing = TRUE, by=QUANTITIY)

result2 <- dbGetQuery(conn, "select sell_date, brand, menu, price, SUM(quantity) from meal_meal_train group by sell_date, brand, price, menu")


result_uk <- dbGetQuery(conn, "select sell_date, SUM(quantity) num from meal_meal_train group by sell_date having count(*) > 1500")

result_uk <- dbGetQuery(conn, "select sell_date, SUM(quantity) num from meal_meal_train
                        where BRAND = 'TakeOut' group by sell_date order by sell_date")

result <- dbGetQuery(conn, "select sell_date, SUM(quantity) num from meal_meal_train group by sell_date order by sell_date")


# 브랜드 하나 더 뽑아야됨
result_uk <- dbGetQuery(conn, "select sell_date, SUM(quantity) num from meal_meal_train
                        where BRAND = 'KOREAN2' and to_char(to_date(sell_date),'D') != 7 group by sell_date")
mean(result_uk$NUM)



table(result$PRICE, result$PRICE)

result <- dbGetQuery(conn, "select brand from meal_meal_train group by brand order by brand")

result3 <- dbGetQuery(conn, "select sell_date, brand, menu, price, SUM(quantity) from meal_meal_train group by sell_date, brand, price, menu")

result4 <- group_by(result2, PRICE)
summarise(result4)

sum(result2$`COUNT(*)`[result2$PRICE == 6000])  441785
length(result2$PRICE[result2$PRICE == 6000])    6430

sum(result2$`COUNT(*)`[result2$PRICE == 6000])  441785
length(result2$PRICE[result2$PRICE == 6000])    6430

# 한글 값 포함 데이터 카테고리화
{
  result$BRAND2[result$BRAND == 'Chef`sCounter'] <- 1
  result$BRAND2[result$BRAND == 'KOREAN1'] <- 2
  result$BRAND2[result$BRAND == 'KOREAN2'] <- 3
  result$BRAND2[result$BRAND == 'TakeOut'] <- 4
  result$BRAND2[result$BRAND == 'Western'] <- 5
  result$BRAND2[result$BRAND == '가츠엔'] <- 6
  result$BRAND2[result$BRAND == '고슬고슬비빈'] <- 7
  result$BRAND2[result$BRAND == '나폴리폴리'] <- 8
  result$BRAND2[result$BRAND == '싱푸차이나'] <- 9
  result$BRAND2[result$BRAND == '스냅스낵'] <- 10
  result$BRAND2[result$BRAND == '아시안픽스'] <- 11
  result$BRAND2[result$BRAND == '우리미각면'] <- 12
  result$BRAND2[result$BRAND == '탕맛기픈'] <- 13
  
  result$GENDER2[result$GENDER == '남'] <- 1
  result$GENDER2[result$GENDER == '여'] <- 2
}

result

tab <- table(result$SELL_DATE, result$BRAND)



# 뭐 하나 할 때 마다 시간 너무 오래 걸리는데 샘플링 할까
result_bg <- bartlett.test(BRAND2 ~ GENDER2, data=result)
# Bartlett test of homogeneity of variances
# data:  BRAND2 by GENDER2
# Bartlett's K-squared = 1220.4, df = 1, p-value < 2.2e-16

result_bb <- bartlett.test(BRAND2 ~ BIRTH_YEAR, data=result)

result_bs <- bartlett.test(BRAND2 ~ SELL_DATE, data=result)
# Bartlett test of homogeneity of variances
# data:  BRAND2 by SELL_DATE
# Bartlett's K-squared = Inf, df = 409, p-value < 2.2e-16

result_bgs <- aov(formula = BRAND2 ~ GENDER2 + SELL_DATE, data=result)
# Call:
#   aov(formula = BRAND2 ~ GENDER2 + SELL_DATE, data = result)
# 
# Terms:
#                  GENDER2 SELL_DATE Residuals
# Sum of Squares     90179    110140  13657172
# Deg. of Freedom        1       409   1023062
# 
# Residual standard error: 3.653671
# Estimated effects may be unbalanced

summary(result_bgs)
#                  Df   Sum Sq Mean Sq F value Pr(>F)    
# GENDER2           1    90179   90179 6755.32 <2e-16 ***
# SELL_DATE       409   110140     269   20.17 <2e-16 ***
# Residuals   1023062 13657172      13                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


result2 %>% group_by(result$BRAND2) %>% summarise(result$BRAND = mean(buy))

result2 <- group_by(result, BRAND2, GENDER2, SELL_DATE)
summarise(result2)
table(result2)

# MAE 안쓸듯
{
  mae <- function(actual, predicted){
    value = 0
    for(i in 1:length(actual)){
      value = value + abs(actual[i] - predicted[i])
    }
    return(value)
  }
  
  mae2 <- function(actual, predicted){
    value = 0
    for(i in 1:nrow(actual)){
      cat('actual / ', i , ' / ', (actual[,i]), '\n')
      cat('predicted / ', i , ' / ', (predicted[,i]), '\n')
      value = value + mae(as.vector(actual[,i]), as.vector(predicted[,i]))
      print(value)
      print()
    }
    return(value)
  }
  
  mae2(actual, predicted)
  
  actual <-    matrix(c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6,
                        0.9, 1.8, 2.5, 4.5, 5.0, 6.2),nrow=2)
  
  predicted <- matrix(c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2,
                        1.1, 1.9, 3.0, 4.4, 5.0, 5.6),nrow=2)
  
  actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
  predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
}
# Project Note
library(lattice)
library(ggplot2)

result <- dbGetQuery(conn, "select CUSTOMER_ID, BRAND, SELL_DATE, PRICE from meal_customer_train NATURAL JOIN meal_meal_train")

result$SELL_DATE <- gsub('-','',result$SELL_DATE)

month01 <- subset(result, SELL_DATE < "20180200")
month02 <- subset(result, SELL_DATE > "20180200" & SELL_DATE < "20180300")
month03 <- subset(result, SELL_DATE > "20180300" & SELL_DATE < "20180400")
month04 <- subset(result, SELL_DATE > "20180400" & SELL_DATE < "20180500")
month05 <- subset(result, SELL_DATE > "20180500" & SELL_DATE < "20180600")
month06 <- subset(result, SELL_DATE > "20180600" & SELL_DATE < "20180700")
month07 <- subset(result, SELL_DATE > "20180700" & SELL_DATE < "20180800")
month08 <- subset(result, SELL_DATE > "20180800" & SELL_DATE < "20180900")

table(result)
max(table(result$SELL_DATE[]))

s1 <- subset(result, SELL_DATE == '20181231')

result1 <- dbGetQuery(conn, "select SELL_DATE, count(*) num from meal_meal_train group by SELL_DATE having count(*) > 1000")
result2 <- dbGetQuery(conn, "select SELL_DATE, count(*) num from meal_meal_train group by SELL_DATE having count(*) > 1000 and count(*) < 2500")

q0 <- dbGetQuery(conn, "select BRAND, count(*) num from meal_meal_train group by BRAND")
q1 <- dbGetQuery(conn, "select SELL_DATE, BRAND, count(*) num from meal_meal_train group by SELL_DATE, BRAND order by SELL_DATE, BRAND")

q2 <- dbGetQuery(conn, "select SELL_DATE, BRAND, count(*) num from meal_meal_train where
            SELL_DATE == '2018-12-31' | 
            SELL_DATE == '2018-05-21' or
            SELL_DATE == '2018-12-24' or
            SELL_DATE == '2018-10-08' or
            SELL_DATE == '2018-04-30' or
            SELL_DATE == '2018-03-02' or
            SELL_DATE == '2018-02-14' or
            SELL_DATE == '2018-09-27' or
            SELL_DATE == '2018-12-28' or
            SELL_DATE == '2019-02-28' or
            SELL_DATE == '2018-03-30' or
            SELL_DATE == '2018-09-28'group by SELL_DATE, BRAND order by SELL_DATE, BRAND")

tot0 <- sum(q0$NUM)
q0$p <- round(q0$NUM / tot0,4 )

{
  s1 <- subset(q1, SELL_DATE == '2018-09-27')
  tot1 <- sum(s1$NUM)
  s1$p <- round(s1$NUM / tot1,4)
  s1
}
s2 <- subset(q1, 
             SELL_DATE == '2018-12-31' | 
               SELL_DATE == '2018-05-21' | 
               SELL_DATE == '2018-12-24' | 
               SELL_DATE == '2018-10-08' | 
               SELL_DATE == '2018-04-30' | 
               SELL_DATE == '2018-03-02' | 
               SELL_DATE == '2018-02-14' | 
               SELL_DATE == '2018-09-27' | 
               SELL_DATE == '2018-12-28' |
               SELL_DATE == '2019-02-28' |
               SELL_DATE == '2018-03-30' |
               SELL_DATE == '2018-09-28')
s2 <- q2

tot2 <- sum(s2$NUM)
s2$p <- s2$NUM / tot2

vec0 <- q0$p
vec1 <- s1$p
vec2 <- s2$p


plot(vec0, vec1)

cor(vec0, vec1)

prop.test(vec0, vec1)


var.test(vec0, vec1, paired=TRUE) 
# 동질성 분포 : t.test()
# 비동질성 분포 : wilcox.test()

t.test(vec0, vec1, paired=TRUE) # p-value < 2.2e-16 

sheet1 <- read.csv("sheet1.csv")
sheet1

row_total = sheet1[1,]
row_select = round(sapply(sheet1[2:8,], mean),4)

row1 <- c(0.0355,0.1352,0.1157,0.1537,0.0498,0.0758,0.0729,0.0564,0.0224,0.0914,0.0447,0.0688,0.0776)
row2 <- c(0.0276,0.1335,0.0911,0.1672,0.0641,0.0876,0.0615,0.0656,0.0247,0.0892,0.0385,0.0633,0.0858)
df <- t(data.frame(row1,row2))
row.names(df) <- c('전체','특정')
colnames(df) <- c('Chef`s','KOREAN1','KOREAN2','TakeOut','Western','가츠엔','고슬고슬비빈','나폴리폴리','스냅스낵','싱푸차이나','아시안픽스','우리미각면','탕맛기픈')
ggplot(df)

{
  library(ggplot2)
  
  df2 = data.frame()
  df2 <- rbind(df2, data.frame(brand='Western',date='전체',value=0.0498))
  df2 <- rbind(df2, data.frame(brand='Western',date='특정',value=0.0641))
  df2 <- rbind(df2, data.frame(brand='나폴리',date='전체',value=0.0564))
  df2 <- rbind(df2, data.frame(brand='나폴리',date='특정',value=0.0656))
  df2 <- rbind(df2, data.frame(brand='가츠엔',date='전체',value=0.0758))
  df2 <- rbind(df2, data.frame(brand='가츠엔',date='특정',value=0.0876))
  df2 <- rbind(df2, data.frame(brand='탕맛기픈',date='전체',value=0.0776))
  df2 <- rbind(df2, data.frame(brand='탕맛기픈',date='특정',value=0.0858))
  df2 <- rbind(df2, data.frame(brand='스냅스낵',date='전체',value=0.0224))
  df2 <- rbind(df2, data.frame(brand='스냅스낵',date='특정',value=0.0247))
  df2 <- rbind(df2, data.frame(brand='TakeOut',date='전체',value=0.1537))
  df2 <- rbind(df2, data.frame(brand='TakeOut',date='특정',value=0.1672))
  df2 <- rbind(df2, data.frame(brand='KOREAN1',date='전체',value=0.1352))
  df2 <- rbind(df2, data.frame(brand='KOREAN1',date='특정',value=0.1335))
  df2 <- rbind(df2, data.frame(brand='차이나',date='전체',value=0.0914))
  df2 <- rbind(df2, data.frame(brand='차이나',date='특정',value=0.0892))
  df2 <- rbind(df2, data.frame(brand='미각면',date='전체',value=0.0688))
  df2 <- rbind(df2, data.frame(brand='미각면',date='특정',value=0.0633))
  df2 <- rbind(df2, data.frame(brand='아시안',date='전체',value=0.0447))
  df2 <- rbind(df2, data.frame(brand='아시안',date='특정',value=0.0385))
  df2 <- rbind(df2, data.frame(brand='고슬비빈',date='전체',value=0.0729))
  df2 <- rbind(df2, data.frame(brand='고슬비빈',date='특정',value=0.0615))
  df2 <- rbind(df2, data.frame(brand='KOREAN2',date='전체',value=0.1157))
  df2 <- rbind(df2, data.frame(brand='KOREAN2',date='특정',value=0.0911))
  df2 <- rbind(df2, data.frame(brand='Chef`s',date='전체',value=0.0355))
  df2 <- rbind(df2, data.frame(brand='Chef`s',date='특정',value=0.0276))
  
  bar <- ggplot(df2, aes(x=brand, y=value,fill=date))
  bar + geom_bar(stat="identity", position="dodge")
  bar + geom_bar(stat="identity", position="fill")
}
# 498/(498+641) :43.72%
# 355/(355+276) :56.26%

# 얘도 사실 관계 없는 듯
vec1 <- c(4233,5510,5523,5767,5797,6048,6055,6094,6105,6123,6130,6148,6178) # 평균 가격
vec2 <- c(163706,138622,118607,93638,79576,77740,74762,70441,57804,51152,45830,36361,23116) # 팔린 개수(내림차순)
vec3 <- sort(vec2) # 팔린 개수(오름차순)
vec4 <- c(23116,138622,118607,163706,51152,70441,57804,74762,93638,77740,79576,36361,45830) # vec1과 브랜드 일치

cor(vec1,vec2) # -0.877532
cor(vec1,vec3) # 0.682306
cor(vec1,vec4) # 0.0947006


png('bar1.png',400,400)

vec <- c(3028,1640)
mt <- t(matrix(c(3028,1640)))
names(mt) <- c('전체', '샌드위치 휴일')
barplot(vec, col=c(2,3), ylab = '날짜별 평균 수요량')

dev.off()

# 일주일 뒤 예측 프로젝트

library(DBI)
library(RJDBC)
library(randomForest)
library(xgboost)

drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
          "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")
conn<-dbConnect(drv, "jdbc:oracle:thin:@//127.0.0.1:1521/xe","scott","tiger")

path <- "C:/ITWILL/2_Rwork/brightics_academy/"

# 안되면 메모장 켜서 다른 이름으로 저장(인코딩 ANSI로)
# R studio가 UTF-8을 제대로 지원 안하는 것 같음
customer_train <- read.csv(paste0(path, "mealData_customer_train.csv"))
meal_train <- read.csv(paste0(path, "mealData_meal_train.csv"))

dbWriteTable(conn, name = 'meal_customer_train', value = customer_train)
dbWriteTable(conn, name = 'meal_meal_train', value = meal_train)

customer_test <- read.csv(paste0(path, "mealData_customer_test.csv"))
meal_test <- read.csv(paste0(path, "mealData_meal_test.csv"))

dbWriteTable(conn, name = 'meal_customer_test', value = customer_test)
dbWriteTable(conn, name = 'meal_meal_test', value = meal_test)

weather <- read.csv(paste0(path,"weather.csv"))

brand_names = c("Chef\`sCounter","KOREAN1","KOREAN2","TakeOut","Western","가츠엔","고슬고슬비빈",
                "나폴리폴리","스냅스낵","싱푸차이나","아시안픽스","우리미각면","탕맛기픈")

tmp <- weather[1:2]
rin <- weather[c(1,3)]
yes_pre <- ifelse(rin$rain < 20, 0, 1)
yes <- cbind(rin[1],yes_pre)

num_x = 2
holiday = 0.54
m1 <- as.matrix(rep(0,41)) # 결과값이 41개
m2 <- as.matrix(rep(0,41))
predict_table <- data.frame(temp = c(0,0,0))
for(brand_name in brand_names){
  print(brand_name)
  nrow(train)
  head(train)
  tail(train)
  # DB 데이터
  {
    train_af7 <- dbGetQuery(conn, paste0("SELECT TO_CHAR(TO_DATE(sell_date)-7) SELL_DATE, SUM(quantity) af7
                                       FROM meal_customer_train NATURAL JOIN meal_meal_train
                                       WHERE brand = '", brand_name,"'
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    train_af7$SELL_DATE <- gsub('/','-',train_af7$SELL_DATE)
    train_af7$SELL_DATE <- gsub('^1','201',train_af7$SELL_DATE)
    train_origin <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) num
                                          FROM meal_customer_train NATURAL JOIN meal_meal_train
                                          WHERE brand = '", brand_name,"'
                                          GROUP BY sell_date
                                          ORDER BY sell_date"))
    train_men <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) men
                                       FROM meal_customer_train NATURAL JOIN meal_meal_train
                                       WHERE (brand = '", brand_name,"') AND gender = '남'
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    train_wom <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) wom
                                       FROM meal_customer_train NATURAL JOIN meal_meal_train
                                       WHERE (brand = '", brand_name,"') AND gender = '여'
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    train_yr6 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr6
                                       FROM meal_customer_train NATURAL JOIN meal_meal_train
                                       WHERE (brand = '", brand_name,"') AND birth_year < 1970
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    train_yr7 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr7
                                       FROM meal_customer_train NATURAL JOIN meal_meal_train
                                       WHERE (brand = '", brand_name,"') AND birth_year >= 1970 AND birth_year < 1980
                                       GROUP BY sell_date"))
    train_yr8 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr8
                                       FROM meal_customer_train NATURAL JOIN meal_meal_train
                                       WHERE (brand = '", brand_name,"') AND birth_year >= 1980 AND birth_year < 1990
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    train_yr9 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr9
                                       FROM meal_customer_train NATURAL JOIN meal_meal_train
                                       WHERE (brand = '", brand_name,"') AND birth_year >= 1990
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    
    test_af7 <- dbGetQuery(conn, paste0("SELECT TO_CHAR(TO_DATE(sell_date)-7) SELL_DATE, SUM(quantity) af7
                                       FROM meal_customer_test NATURAL JOIN meal_meal_test
                                       WHERE brand = '", brand_name,"'
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    test_af7$SELL_DATE <- gsub('/','-',test_af7$SELL_DATE)
    test_af7$SELL_DATE <- gsub('^1','201',test_af7$SELL_DATE)
    test_origin <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) num
                                          FROM meal_customer_test NATURAL JOIN meal_meal_test
                                          WHERE brand = '", brand_name,"'
                                          GROUP BY sell_date
                                          ORDER BY sell_date"))
    test_men <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) men
                                       FROM meal_customer_test NATURAL JOIN meal_meal_test
                                       WHERE (brand = '", brand_name,"') AND gender = '남'
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    test_wom <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) wom
                                       FROM meal_customer_test NATURAL JOIN meal_meal_test
                                       WHERE (brand = '", brand_name,"') AND gender = '여'
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    test_yr6 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr6
                                       FROM meal_customer_test NATURAL JOIN meal_meal_test
                                       WHERE (brand = '", brand_name,"') AND birth_year < 1970
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    test_yr7 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr7
                                       FROM meal_customer_test NATURAL JOIN meal_meal_test
                                       WHERE (brand = '", brand_name,"') AND birth_year >= 1970 AND birth_year < 1980
                                       GROUP BY sell_date"))
    test_yr8 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr8
                                       FROM meal_customer_test NATURAL JOIN meal_meal_test
                                       WHERE (brand = '", brand_name,"') AND birth_year >= 1980 AND birth_year < 1990
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
    test_yr9 <- dbGetQuery(conn, paste0("SELECT sell_date, SUM(quantity) yr9
                                       FROM meal_customer_test NATURAL JOIN meal_meal_test
                                       WHERE (brand = '", brand_name,"') AND birth_year >= 1990
                                       GROUP BY sell_date
                                       ORDER BY sell_date"))
  }
  
  train <- train_af7
  train <- merge(train, train_origin, by = 'SELL_DATE', all.x = TRUE)
  # train <- merge(train, train_men, by = 'SELL_DATE', all.x = TRUE)
  # train <- merge(train, train_wom, by = 'SELL_DATE', all.x = TRUE)
  # train <- merge(train, train_yr6, by = 'SELL_DATE', all.x = TRUE)
  # train <- merge(train, train_yr7, by = 'SELL_DATE', all.x = TRUE)
  # train <- merge(train, train_yr8, by = 'SELL_DATE', all.x = TRUE)
  # train <- merge(train, train_yr9, by = 'SELL_DATE', all.x = TRUE)
  # train$YR9 <- ifelse(is.na(train$YR9), 0, train$YR9)
  # train <- merge(train, tmp, by = 'SELL_DATE', all.x = TRUE)
  # train <- merge(train, rin, by = 'SELL_DATE', all.x = TRUE)
  train <- merge(train, yes, by = 'SELL_DATE', all.x = TRUE)
  train <- na.omit(train)
  
  test <- test_af7
  test <- merge(test, test_origin, by = 'SELL_DATE', all.x = TRUE)
  # test <- merge(test, test_men, by = 'SELL_DATE', all.x = TRUE)
  # test <- merge(test, test_wom, by = 'SELL_DATE', all.x = TRUE)
  # test <- merge(test, test_yr6, by = 'SELL_DATE', all.x = TRUE)
  # test <- merge(test, test_yr7, by = 'SELL_DATE', all.x = TRUE)
  # test <- merge(test, test_yr8, by = 'SELL_DATE', all.x = TRUE)
  # test <- merge(test, test_yr9, by = 'SELL_DATE', all.x = TRUE)
  # test$YR9 <- ifelse(is.na(test$YR9), 0, test$YR9)
  # test <- merge(test, tmp, by = 'SELL_DATE', all.x = TRUE)
  # test <- merge(test, rin, by = 'SELL_DATE', all.x = TRUE)
  test <- merge(test, yes, by = 'SELL_DATE', all.x = TRUE)
  test <- na.omit(test)
  {
    train$DAY <- weekdays(as.Date(train$SELL_DATE))
    train <- filter(train, DAY != "토요일")
    train <- train[1:(ncol(train)-1)]
    test$DAY <- weekdays(as.Date(test$SELL_DATE))
    test <- filter(test, DAY != "토요일")
    test <- test[1:(ncol(test)-1)]
    
    # predict
    y_true <- test$NUM
    ## lm
    model <- lm(AF7~.,data=train[-1])
    y_pred <- predict(model, test[c(3:(2+num_x))], family = 'response')
    y_pred[8] <- y_pred[8] * holiday
    a <- abs(y_true - y_pred)
    m1 <- cbind(m1, as.matrix(y_true))
    m2 <- cbind(m2, as.matrix(y_pred))
    
    print(model$coefficients)
    
    ## random forest (4회 평균)
    vec = vector()
    for(i in 1:4){
      model <- randomForest(AF7~., data=train[-1], importance = T)
      y_pred <- predict(model, test[c(3:(2+num_x))], family = 'response')
      y_pred[8] <- y_pred[8] * holiday
      y <- abs(y_true - y_pred)
      vec <- c(vec, mean(y))
    }
    b <- mean(vec)
    
    ## xgboost
    train_x <- as.matrix(train[c(3:(2+num_x))])
    train_y <- train$AF7
    test_x <- as.matrix(test[c(3:(2+num_x))])
    test_y <- test$AF7
    dtrain <- xgb.DMatrix(data = train_x, label = train_y)
    xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 0.5, nthread = 2, nrounds = 10, objective = "reg:squarederror", verbose = 0)
    y_pred <- predict(xgb_model, test_x)
    y_pred[8] <- y_pred[8] * holiday
    c <- abs(y_true - y_pred)
    
    # 결과값 추가
    result_vector <- c(mean(a),mean(b),mean(c))
    predict_table <- cbind(predict_table, result_vector)
    
  }
}
predict_table = predict_table[2:ncol(predict_table)]
colnames(predict_table) <- brand_names
predict_table$average <- apply(predict_table,1,mean)
print(predict_table)

true_marix <- rowSums(m1)
lm_matrix <- rowSums(m2)

png('Result.png',1000,400)
plot(true_marix, type='l',col='red',xlab='날짜',ylab='취식량', ylim=c(2200,3800))
lines(lm_matrix, type='l',col='blue')
legend(38,2500,c("실제값","예측값"),lwd=c(1,1),col=c("red","blue"))
dev.off()