

library(dplyr)
sum_gender <- customer_meal %>% group_by(GENDER) %>% summarise(sum(QUANTITY))
# GENDER `sum(QUANTITY)`
# <fct>            <int>
# 1 남              768817
# 2 여              262538

sum_menu <- customer_meal %>% group_by(BRAND) %>% summarise(sum(QUANTITY))



# 전체 데이터에서 남녀 비율 맞추기
women <- customer_meal %>% filter(GENDER=='여') %>% select(BRAND,MENU,QUANTITY) %>% 
    group_by(BRAND) %>% summarise(women=sum(QUANTITY))
women$women<- women$women/sum(women$women)*100

men <- customer_meal %>% filter(GENDER=='남') %>% select(BRAND,MENU,QUANTITY) %>% 
    group_by(BRAND) %>% summarise(men=sum(QUANTITY))
men$men<- men$men/sum(men$men)*100




##### 성별

### 파생변수에서 성별 비율 맞추기
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











####### 그래프 시각화 함수 사용

qplot(BRAND, data=gender, fill=GENDER, facets=.~ GENDER)
qplot(BRAND, data=ggg, fill=GENDER, facets=.~ GENDER, ylim=c(1,1.05))


p<- ggplot(gender, aes(BRAND))
p+geom_bar(aes(fill=GENDER), position="fill")
ggplot(data=gender, aes(x=BRAND,y=QUANTITY,fill=GENDER))+geom_col(position='dodge')
ggplot(data=percent, aes(x=BRAND,y=value,fill=variable))+geom_col(position='dodge')



histogram(~variable | factor(BRAND), data=percent , col=rainbow(2),
          auto.key=TRUE)
?histogram

barplot(gender_mt)







######
#나이

## 나이 파생변수 생성

str(customer_meal)
age <- customer_meal %>% select(BIRTH_YEAR,BRAND,QUANTITY)
range(age$BIRTH_YEAR) #1952 1997 : 69~24
table(age$BIRTH_YEAR)
age$age2[age$BIRTH_YEAR>=1952&age$BIRTH_YEAR<=1969] <- '50~60년대생'
age$age2[age$BIRTH_YEAR>=1970&age$BIRTH_YEAR<=1979] <- '70년대생'
age$age2[age$BIRTH_YEAR>=1980&age$BIRTH_YEAR<=1989] <- '80년대생'
age$age2[age$BIRTH_YEAR>=1990&age$BIRTH_YEAR<=1997] <- '90년대생'
str(age)
age$age <- 2019 - customer_meal$BIRTH_YEAR + 1
range(age$age)

str(age)
table(age$age2)
# 50~60년대생    70년대생    80년대생    90년대생 
# 143385      403921      426191       49976 

p1 <- subset(customer_meal,customer_meal$age2=='50~60년대생')
p2 <- subset(customer_meal,customer_meal$age2=='70년대생')
p3 <- subset(customer_meal,customer_meal$age2=='80년대생')
p4 <- subset(customer_meal,customer_meal$age2=='90년대생')

g <- group_by(p1,BRAND)
g2 <- group_by(p2,BRAND)
g3 <- group_by(p3,BRAND)
g4 <- group_by(p4,BRAND)

percent1 <- as.data.frame(summarise(g,sum(QUANTITY)/143385*100))
percent2 <- as.data.frame(summarise(g2,sum(QUANTITY)/403921*100))
percent3 <- as.data.frame(summarise(g3,sum(QUANTITY)/426191*100))
percent4 <- as.data.frame(summarise(g4,sum(QUANTITY)/49976*100))



per <- data.frame(percent1,percent2[2],percent3[2],percent4[2])


names(per) <- c('BRAND','50~60년대','70년대','80년대','90년대')
perc <- melt(per,id='BRAND')
names(agepercent) <- c('BRAND','AGE','VALUE')
agepercent$VALUE <- round(agepercent$VALUE,2)
str(agepercent)





library(ggplot2)
ggplot(data=perc, aes(x=BRAND,y=value,fill=variable))+geom_col(position='dodge')


qplot(AGE, data=agepercent, fill=BRAND)


p<- ggplot(agepercent, aes(BRAND))
p+geom_bar(aes(fill=AGE), position="fill")

summary(age)

barchart( ~age2 | factor(BRAND), data=age)
histogram(~age2 | factor(BRAND), data=age)
?histogram




barplot(gender_mt)

qplot(BRAND, data=age, fill=age2, geom="bar")



##################
customer_meal
str(customer_meal)
length(unique(customer_meal$CUSTOMER_ID))

gid <- group_by(customer_meal,CUSTOMER_ID)
new <- summarise(gid,price=sum(PRICE),year=unique(BIRTH_YEAR))

gy <- group_by(new,year)
new2 <- summarise(gy,price=max(price))
new2 <- as.data.frame(new2)

new2$age <- 2019 - new$year + 1


customer_meal$age2[customer_meal$BIRTH_YEAR>=1952&age$BIRTH_YEAR<=1969] <- '50~60년대생'
customer_meal$age2[customer_meal$BIRTH_YEAR>=1970&age$BIRTH_YEAR<=1979] <- '70년대생'
customer_meal$age2[customer_meal$BIRTH_YEAR>=1980&age$BIRTH_YEAR<=1989] <- '80년대생'
customer_meal$age2[customer_meal$BIRTH_YEAR>=1990&age$BIRTH_YEAR<=1997] <- '90년대생'

id <- group_by(customer_meal,age2)
new <- summarise(id,sum=sum(QUANTITY))
# 1 50~60년대생 144313
# 2 70년대생    407111
# 3 80년대생    429524
# 4 90년대생     50407

prop.table(table(customer_meal$age2))
# 50~60년대생    70년대생    80년대생    90년대생 
# 0.14009651  0.39465721  0.41641646  0.04882982 

labf <- paste(names(table(customer_meal$age2)), "\n", round(prop.table(table(customer_meal$age2)),2), "%")
pie(table(customer_meal$age2),labels=labf)






### 숫자 정리 (사원 수, 주문 수 등)

customer_train <- read.csv(file.choose())
meal_train <- read.csv(file.choose())

customer_meal <- merge(meal_train,customer_train,by='CUSTOMER_ID')
str(customer_meal)

sort(table(customer_meal$CUSTOMER_ID),decreasing=TRUE)

length(unique(customer_meal$CUSTOMER_ID)) # 10570명
length(unique(customer_meal$SELL_DATE)) # 410일
table(table(customer_meal$CUSTOMER_ID)>=100) # 4179명 6391명
range(table(customer_meal$CUSTOMER_ID)) 
names(table(customer_meal$QUANTITY)) #하루에 1~21개 주문까지 들어옴
table(customer_meal$QUANTITY)
#   1          2       3       4       5       6       7       8       9 
# 1016735    6004     539     128      24      19       7       3       3 
# 10      11      12      13      19      21 
# 3       1       3       1       2       1 


### 사원 정리

library(dplyr)
customer <- customer_meal[c(1,3,6)]
str(customer)
customer1 <- subset(customer,customer$QUANTITY==1)
customer2 <- subset(customer,customer$QUANTITY!=1)
length(unique(customer1$CUSTOMER_ID)) #10565 : 5명 빠짐
length(unique(customer2$CUSTOMER_ID)) #1773
str(customer1)


check <- customer_meal %>% group_by(CUSTOMER_ID) %>% summarise(sum=sum(QUANTITY))
range(check$sum) #1~791 




#### 개인 주문자 중 100번 이상 방문한 사원의 선호도 메뉴

library(reshape2)
num <- dcast(customer1,CUSTOMER_ID~BRAND,sum)
num$sum <- apply(num[-1],1,sum)
range(num$sum) #1~641
str(num)

new <- subset(num,num$sum>=100)
str(new) 
range(new$sum) #100~641


cusb <- arrange(melt(new[-15],id='CUSTOMER_ID'),CUSTOMER_ID)
head(cusb,20)
str(cusb)
length(unique(cusb$CUSTOMER_ID))

g <- group_by(cusb,CUSTOMER_ID)
cusmax <- summarise(g,which.max(value))
cusmax <- cusmax$`which.max(value)`
head(cusmax)
length(cusmax)


for(i in num){
    cusmax[i]<-cusmax[i]+(13*(i-1))
}

brand<-cusb[cusmax,2]

library(lattice)
brand<-table(brand)
n <- names(brand)
b <- as.vector(brand)
names(b)<-n
barplot(b)
hist(b)











