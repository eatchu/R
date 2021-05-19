install.packages('foreign')
library(foreign)
hpc <- read.spss(file.choose())
hpc <- as.data.frame(hpc) #list -> dataframe
str(hpc) #16664 obs. of  957 variables
welfare <- hpc
head(welfare)
View(welfare)
summary(welfare)

##################################################
####### 변수명 변경하고 파생 변수 만들기##########
##################################################
library(dplyr) 
# rename(data, new변수명=old변수명, ... )
welfare <- rename(welfare, sex=h10_g3, birth=h10_g4,
                  marry=h10_g10, religion=h10_g11,
                  job=h10_eco9,region=h10_reg7,income=p1002_8aq1 )
welfare_1 <- welfare[c('sex','birth','marry','religion','job','region','income')]
str(welfare_1) #'data.frame':	16664 obs. of  7 variables
summary(welfare_1)


##################################################
####### 데이터 상태 확인##########
##################################################
welf <- welfare_1
table(welf$sex,useNA='ifany') # 이상치 없음 1,2만 존재
# 1    2 
# 7578 9086 
welf$sex <- factor(welf$sex, levels=c(1,2), labels=c("1male","2female"))
str(welf)
boxplot(welf$birth) # 이상치 없음
range(welf$birth) #1907 2014
table(welf$marry,useNA='ifany')
#   0    1    2    3    4    5    6 
# 2861 8431 2117  712   84 2433   26 
welf$marry <- factor(welf$marry, levels=c(0,1,2,3,4,5,6),
                     labels=c("0비해당(18세미만)","1유배우","2사별","3이혼","4별거",
                              "5미혼","6기타(사망등)"))
table(welf$religion,useNA='ifany')
# 1    2 
# 8047 8617 
welf$religion <- factor(welf$religion, levels=c(1,2),
                        labels=c('1있음','2없음'))
table(welf$job,useNA='ifany') #결측치9135
length(unique(welf$job)) #146개의 직업
table(welf$region,useNA = 'ifany')
# 1    2    3    4    5    6    7 
# 2486 3711 2785 2036 1467 1257 2922 
summary(welf$income) # 결측치 12030
range(welf$income,na.rm=TRUE) # 0 2400
boxplot(welf$income)$stats # 0~608

# sex, religion, marry 범주형 설정 
# region 범주형 미설정
# job, income 결측치 존재


##################################################
####### 성별에 따른 월급 차이 ##########
##################################################
wel_s_i <- welf[c('sex','income')]
str(wel_s_i)
library(ggplot2)
qplot(wel_s_i$sex)
summary(wel_s_i$income)
library(prettyR)
i<-table(wel_s_i$income)
which.max(i)
M <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's  최빈수
# 0.0   122.0   192.5   241.6   316.6  2400.0   12030    20
# 월급은 122~316만원 사이에 가장 많이 분포하고 있다

qplot(wel_s_i$income)
qplot(wel_s_i$income,xlim=c(0,1000))

wel_s_i$income <- ifelse(wel_s_i$income %in% c(0,9999),NA,wel_s_i$income) #0제거
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.46  123.00  193.00  242.35  316.70 2400.00   12044 
table(is.na(wel_s_i$income)) #12044


# 내가 계산한 성별에 따른 임금 평균
wel <- wel_s_i$income
mean(wel,na.rm=TRUE)
wel <- na.omit(wel)
summary(wel)
mean(wel)
wf <- subset(wel_s_i,wel_s_i$sex=='2female')
mean(wf$income,na.rm=TRUE)
avg_income <- wel_s_i %>% group_by(sex) %>% summarise(mean(income,na.rm=TRUE))
# 1 1male                           312.
# 2 2female                         163.


# 책에서 만든 성별에 따른 임금 평균
sex_income <- wel_s_i %>% filter(!is.na(income)) %>% 
  group_by(sex) %>% summarise(mi=mean(income))
# 1 1male             312.
# 2 2female           163.


# 소수점 자리 조정하기
options()$digits # 7
options()$scipen
options(scipen=4)


ggplot(data=sex_income, aes(x=sex,y=mi)) + geom_col()



# Corpus , ggplot , geom_col , vector source










