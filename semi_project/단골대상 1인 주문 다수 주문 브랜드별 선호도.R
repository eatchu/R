
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


#### 개인 주문자

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

num <- 1:length(cusmax)

for(i in num){
  cusmax[i]<-cusmax[i]+(13*(i-1))
}

brand<-cusb[cusmax,2]

library(lattice)
brand<-table(brand)
n <- names(brand)
b <- as.vector(brand)
names(b)<-n
barplot(b,
        main='개인주문자 브랜드별 선호도')
hist(b)



##### 단체 주문자


library(reshape2)
num2 <- dcast(customer2,CUSTOMER_ID~BRAND,sum)
num2$sum <- apply(num2[-1],1,sum)
range(num2$sum) #2~482
str(num2)

new <- subset(num2,num2$sum>=100)
str(new) 
range(new$sum) #105~482



cusb <- arrange(melt(new[-15],id='CUSTOMER_ID'),CUSTOMER_ID)
head(cusb,20)
str(cusb)
length(unique(cusb$CUSTOMER_ID)) #16명


g <- group_by(cusb,CUSTOMER_ID)
cusmax <- summarise(g,which.max(value))
cusmax <- cusmax$`which.max(value)`
head(cusmax)
length(cusmax)

num <- 1:length(cusmax)

for(i in num){
  cusmax[i]<-cusmax[i]+(13*(i-1))
}

brand<-cusb[cusmax,2]

library(lattice)
brand<-table(brand)
n <- names(brand)
b <- as.vector(brand)
names(b)<-n
barplot(b,
     main='단체 주문자 브랜드별 선호도')

















