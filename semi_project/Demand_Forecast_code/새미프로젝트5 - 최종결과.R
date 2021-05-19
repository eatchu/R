# 일주일 뒤 예측 프로젝트

library(DBI)
library(RJDBC)
library(randomForest)
library(xgboost)
library(rJava)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_151')
drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
          "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")
conn<-dbConnect(drv,"jdbc:oracle:thin:@//127.0.0.1:1521/xe","scott","tiger")

weather <- read.csv(file.choose())

wild_card = "' OR 1=1 OR brand = '" # wild card
brand_names = c("Chef\`sCounter","KOREAN1","KOREAN2","TakeOut","Western","가츠엔","고슬고슬비빈",
                "나폴리폴리","스냅스낵","싱푸차이나","아시안픽스","우리미각면","탕맛기픈")

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
tmp <- weather[1:2]
rin <- weather[c(1,3)]
yes_pre <- ifelse(rin$rain < 20, 0, 1)
yes <- cbind(rin[1],yes_pre)


num_x = 4
m1 <- as.matrix(rep(0,41)) # 결과값이 41개
m2 <- as.matrix(rep(0,41)) # 결과값이 41개
m3 <- as.matrix(rep(0,41)) # 결과값이 41개
predict_table <- data.frame(temp = c(0,0,0))
for(brand_name in brand_names){
  print(brand_name)
  
  train <- train_af7
  train <- merge(train, train_origin, by = 'SELL_DATE', all.x = TRUE)
  train <- merge(train, train_men, by = 'SELL_DATE', all.x = TRUE)
  train <- merge(train, train_wom, by = 'SELL_DATE', all.x = TRUE)
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
  test <- merge(test, test_men, by = 'SELL_DATE', all.x = TRUE)
  test <- merge(test, test_wom, by = 'SELL_DATE', all.x = TRUE)
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
    y_pred[8] <- y_pred[8] * 0.6
    a <- abs(y_true - y_pred)
    m1 <- cbind(m1, as.matrix(y_true))
    m2 <- cbind(m2, as.matrix(y_pred))
    
    ## random forest (4회 평균)
    vec = vector()
    for(i in 1:4){
      model <- randomForest(AF7~., data=train[-1], importance = T)
      y_pred <- predict(model, test[c(3:(2+num_x))], family = 'response')
      y_pred[8] <- y_pred[8] * 0.6
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
    y_pred[8] <- y_pred[8] * 0.6
    c <- abs(y_true - y_pred)
    m3 <- cbind(m3, as.matrix(y_pred))
    
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
xg_matrix <- rowSums(m3)

plot(true_marix, type='l',col='red',xlab='날짜',ylab='취식량')
lines(lm_matrix, type='l',col='blue')
lines(xg_matrix, type='l',col='blue')
legend(28,2340,c("실제값","xg 예측값"),lwd=c(1,1),col=c("red","blue"))
legend(30,2340,c("실제값"," 예측값"),lwd=c(1,1),col=c("red","blue"))



