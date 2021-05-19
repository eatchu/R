
### 다수 방문자와 전체 방문자 브랜드별 선호도

customer_train <- read.csv(file.choose())
meal_train <- read.csv(file.choose())

customer_meal <- merge(meal_train,customer_train,by='CUSTOMER_ID')
str(customer_meal)


### 파생변수

customer <- customer_meal[c(1,3,6)] # 브랜드, QUANTITY, 사원번호
str(customer)




#### 데이터 구조 변경
library(reshape2)
number <- dcast(customer,CUSTOMER_ID~BRAND,sum)
number$sum <- apply(number[-1],1,sum)
range(number$sum) #1~791
str(number)




###전체 사원의 선호도 메뉴
cus <- arrange(melt(number[-15],id='CUSTOMER_ID'),CUSTOMER_ID)
head(cus,20)
length(cus$CUSTOMER_ID) #10570x13
length(unique(cus$CUSTOMER_ID)) #10570 전체사원의 수

#사원별 가장 많이 먹은 브랜드 index 뽑아내기
g1 <- group_by(cus,CUSTOMER_ID)
cusmax1 <- summarise(g1,which.max(value))
cusmax1 <- cusmax1$`which.max(value)`
head(cusmax1)
length(cusmax1) #10570

#가장 많이 먹은 브랜드 행 번호 구하기
num1 <- 1:length(cusmax1)
for(i in num1){
  cusmax1[i]<-cusmax1[i]+(13*(i-1))
}
head(cusmax1) # 최대 많이 먹은 브랜드의 행 번호


# 브랜드 이름 뽑아내기
brand1<-cus[cusmax1,2]

# 막대 그래프
library(lattice)
brand1<-table(brand1)
n1 <- names(brand1)
b1 <- as.vector(brand1)
names(b1)<-n1
barplot(b1,
        main='전체 사원의 브랜드별 선호도')


# 파이 그래프
pct1 <- round((brand1/sum(brand1)*100),2)
lab1 <- paste(names(brand1),"\n", pct1, "%")
pie(brand1,labels=lab1,
    main='전체 사원의 브랜드별 선호도')




### 100번이상 방문자의 선호도 메뉴

# 100이상 구매 사원 뽑아내기
new <- subset(number,number$sum>=100)
str(new) 
range(new$sum) #100~791


cusb <- arrange(melt(new[-15],id='CUSTOMER_ID'),CUSTOMER_ID)
head(cusb,20)
str(cusb)
length(unique(cusb$CUSTOMER_ID)) #4191명 


g2 <- group_by(cusb,CUSTOMER_ID)
cusmax2 <- summarise(g2,which.max(value))
cusmax2 <- cusmax2$`which.max(value)`
head(cusmax2)
length(cusmax2)

num2 <- 1:length(cusmax2)

for(i in num2){
  cusmax2[i]<-cusmax2[i]+(13*(i-1))
}

brand2<-cusb[cusmax2,2]

library(lattice)
brand2<-table(brand2)
n2 <- names(brand2)
b2 <- as.vector(brand2)
names(b2)<-n2
barplot(b2,
        main='단골사원의 브랜드별 선호도')



pct2 <- round((brand2/sum(brand2)*100),2)
lab2 <- paste(names(brand2),"\n", pct2, "%")
pie(brand2,labels=lab2,
    main='단골사원의 브랜드별 선호도')




par(mfrow=c(1,1))




custom_data = arrange(people_eat,desc(QUANTITY))$CUSTOMER_ID[1:4191 ]
for(i in 1:length(custom_data)){
  custom1 = subset(samsung, CUSTOMER_ID==custom_data[i])
  t = table(custom1$BRAND)
  best=names(t)[t == max(t)]
  print(best)
  samsung$brand_prefer[samsung$CUSTOMER_ID==custom_data[i]]=best
} 







str(customer_meal)

customer <- customer_meal[c(1,3,6)]
str(customer)

library(reshape2)
number <- dcast(customer,CUSTOMER_ID~BRAND,sum)
number$sum <- apply(number[-1],1,sum)
range(number$sum) #1~791
str(number)

library(dplyr)
custom_data = arrange(number,desc(sum))$CUSTOMER_ID[1:4191]
for(i in 1:length(custom_data)){
  custom1 = subset(customer_meal, CUSTOMER_ID==custom_data[i])
  t = table(custom1$BRAND)
  best=names(t)[t == max(t)]
  print(best)
  customer_meal$brand_prefer[customer_meal$CUSTOMER_ID==custom_data[i]]=best
} 

str(customer_meal)
summary(customer_meal)
table(customer_meal$brand_prefer,useNA='ifany')



             
                