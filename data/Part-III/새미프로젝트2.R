

weather <- read.csv(file.choose())
str(weather)
names(weather)
library(stringr)
weather$X2018.01.01 <- str_replace_all(weather$X2018.01.01,'[^0-9]','-')
range(weather$X2018.01.01) #"2018-01-02" "2019-07-31"


customer_train <- read.csv(file.choose())
meal_train <- read.csv(file.choose())
customer_meal <- merge(meal_train,customer_train,by='CUSTOMER_ID')
str(customer_meal)
customer_meal$SELL_DATE <- as.character(customer_meal$SELL_DATE)
range(customer_meal$SELL_DATE) #"2018-01-02" "2019-05-24"


weather1<-subset(weather,weather$X2018.01.01<="2019-05-24")
str(weather1)
range(weather1$X2018.01.01)


c_m_w <- merge(x=customer_meal,y=weather1,by.x="SELL_DATE",by.y="X2018.01.01")
str(c_m_w)
tail(c_m_w)



new <- subset(customer_meal,customer_meal$GENDER=='여')
new <- subset(new,new$BRAND=='TakeOut')
str(new)
table(new$BRAND)
take <- head(sort(table(new$MENU),decreasing = TRUE),10)
barplot(take, main='여자사원의 takeout 브랜드에서의 선호도 메뉴')



length(unique(c_m_w$MENU)) #560개
head(sort(table(customer_meal$MENU),decreasing = TRUE),100)
names(customer_meal)


####### 상관관계#############
customer <- customer_meal[-c(1,2)]
str(customer)
customer$BRAND <- as.numeric(customer$BRAND)
customer$MENU <- as.numeric(customer$MENU)
customer$GENDER <- as.numeric(customer$GENDER)

cor(customer)

###### 브랜드별 가격 비교 ###########
customer <- customer_meal[c(3,5)]
customer <- customer_meal[c(3,6)]
g <- group_by(customer,BRAND)
p <- summarise(g,mean=mean(PRICE))
p <- summarise(g,sum=sum(QUANTITY))
p


mean <- arrange(p,mean)

sum <- arrange(p,desc(sum))


cor(sum$sum,mean$mean) #-0.8776884 



pricecor <- data.frame(sum=sum$sum,mean=mean$mean)
str(pricecor)




#### test

test_customer <- read.csv(file.choose())
test_meal <- read.csv(file.choose())

str(test_customer)

test <- merge(test_customer,test_meal,by='CUSTOMER_ID')
str(test)
str(customer_meal)
customer <- test[c(5,7)]
customer <- test[c(5,8)]
g <- group_by(customer,BRAND)
str(customer)
p <- summarise(g,mean=mean(PRICE))
p <- summarise(g,sum=sum(QUANTITY))
p
mean <- arrange(p,mean)
sum <- arrange(p,desc(sum))

testpricecor <- data.frame(sum=sum$sum,mean=mean$mean)
str(testpricecor)




###
model <- lm(formula=sum ~ mean, data=pricecor)
model
summary(model)


# 
y_pred <- predict(model, testpricecor)
y_pred
length(y_pred)
y_true <- testpricecor$sum
y_pred - y_true
cor(y_true, y_pred) #0.8424653


plot(y_true,col='blue',type='l',label='y true')
points(y_pred,col='red',type='l',label='y pred')
legend('topright',cex=1,legend=c('y true','y pred'), col=c('blue','red'),pch='--')




