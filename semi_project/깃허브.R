
# 1,2 데이터 조인

customer_train <- read.csv(file.choose()) # 고객정보 파일
meal_train <- read.csv(file.choose()) # 식당정보 파일
customer_meal <- merge(meal_train,customer_train,by='CUSTOMER_ID') #공통칼럼 기준으로 데이터조인
str(customer_meal) #'data.frame':	1023473 obs. of  8 variables

# 3 데이터 처리 및 추가 조인

weather <- read.csv(file.choose()) # 날씨정보 파일
str(weather) #'data.frame':	576 obs. of  3 variables
names(weather) # "X2018.01.01" "X..평균기온..1.." "X.." 
names(weather) <- c('SELL_DATE','TEMP','PRECIP')

library(stringr)
weather$SELL_DATE <- str_replace_all(weather$SELL_DATE,'[^0-9]','-') 
weather$SELL_DATE <- factor(weather$SELL_DATE)

c_m_w <- merge(x=customer_meal,y=weather,by="SELL_DATE")
str(c_m_w)


# 데이터 전처리 1 : 문자열 전처리
unique(c_m_w$TEMP) #['평균기온:-0'] ['평균기온:-1'] ... 
unique(c_m_w$PRECIP) #['일강수량:0.0'] ['일강수량:0.1'] ... 

c_m_w$TEMP <- str_replace_all(c_m_w$TEMP, "\\[\\'평균기온:", "")
c_m_w$TEMP <- str_replace_all(c_m_w$TEMP, "\\'\\]", "")
c_m_w$PRECIP= str_replace_all(c_m_w$PRECIP, "\\[\\'일강수량:", "")
c_m_w$PRECIP= str_replace_all(c_m_w$PRECIP, "\\'\\]", "")
c_m_w$PRECIP= str_replace_all(c_m_w$PRECIP, "\\[\\]", "0")

str(c_m_w)
c_m_w$TEMP <- as.numeric(c_m_w$TEMP)
c_m_w$PRECIP <- as.numeric(c_m_w$PRECIP)

# 데이터 전처리 2 : 오차, 결측치 전처리 + 자료 정보
summary(c_m_w) #결측치 없음
table(c_m_w$GENDER) #이상치 없음 (남 녀)
range(c_m_w$BIRTH_YEAR) #나이범위 1952~1997
length(unique(c_m_w$MENU)) #560가지 메뉴
length(unique(c_m_w$BRAND)) #13개 브랜드
length(unique(c_m_w$CUSTOMER_ID)) #구내식당 1번 이상 방문한 사원수 10570명
range(c_m_w$PRICE) #3500~6500 가격 범주
range(as.character(c_m_w$SELL_DATE)) #"2018-01-02" "2019-05-24" 날짜 범위
range(c_m_w$TEMP) #-14~33도 온도 범위
range(c_m_w$PRECIP) #0~96.5 강수량 범위





# 전체 데이터에서 남녀 비율 맞추기 - QUANTITY 기준
women <- customer_meal %>% filter(GENDER=='여') %>% select(BRAND,MENU,QUANTITY) %>% 
  group_by(BRAND) %>% summarise(women=sum(QUANTITY))
women$women<- women$women/sum(women$women)*100

men <- customer_meal %>% filter(GENDER=='남') %>% select(BRAND,MENU,QUANTITY) %>% 
  group_by(BRAND) %>% summarise(men=sum(QUANTITY))
men$men<- men$men/sum(men$men)*100


### 파생변수에서 성별 비율 맞추기 - BRAND 기준
library(dplyr)
gender <- customer_meal %>% select(BRAND,GENDER,QUANTITY)

prop.table(table(gender$GENDER))
# 남        여 
# 0.7462385 0.2537615 


mpercent <- gender %>% filter(gender$GENDER=='남') %>% 
  group_by(BRAND) %>% summarise(man=n()/768817*100)
fpercent <- gender %>% filter(gender$GENDER=='여') %>% 
  group_by(BRAND) %>% summarise(women=n()/262538*100)

percent <- merge(mpercent,fpercent,by='BRAND')

percent$man <- round(percent$man,2)
percent$women <- round(percent$women,2)

library(ggplot2)
library(reshape2)
percent <- melt(percent,id='BRAND')
ggplot(data=percent, aes(x=BRAND,y=value,fill=variable))+geom_col(position='dodge')+
  geom_text(aes(y=value+0.1,label=value), vjust=-0.2, label.size=-1) +
  ylim(0, max(percent$value) * 1.05)


### 남녀에 따른 브랜드별 선호도 

genderm <- gender %>% filter(GENDER=='남')
genderf <- gender %>% filter(GENDER=='여')

par(mfrow=c(1,2))

pct <- round(prop.table(table(gender$GENDER))*100, 1)
lab <- paste(names(pct), "\n", pct, "%")
pie(table(gender$GENDER), labels=lab, 
    main='사원들의 성비')

genderm <- genderm %>% group_by(BRAND) %>% summarise(n=n()) %>% arrange(desc(n))
genderm <- as.data.frame(genderm)
pct <- round(genderm$n/sum(genderm$n)*100, 2)
lab <- paste(genderm$BRAND, "\n", pct, "%")
pie(genderm$n, labels=lab, 
    main='남성의 브랜드별 취식 비율')

genderf <- genderf %>% group_by(BRAND) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(5)
genderf <- as.data.frame(genderf)
pct2 <- round(genderf$n/sum(genderf$n)*100, 2)
lab2 <- paste(genderf$BRAND, "\n", pct2, "%")
pie(genderf$n, labels=lab2, 
    main='여성의 브랜드별 취식 비율')


# 남녀에 따른 브랜드별 선호도 비율

par(mfrow=c(1,2))


labm <- paste(percent$BRAND[1:13], "\n", percent$value[1:13], "%")
pie(percent$value[1:13], labels=labm, 
    main='남성의 브랜드별 취식 비율')


labf <- paste(percent$BRAND[14:26], "\n", percent$value[14:26], "%")
pie(percent$value[14:26], labels=labf, 
    main='여성의 브랜드별 취식 비율')















