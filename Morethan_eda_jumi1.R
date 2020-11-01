install.packages("dplyr")
install.packages("DescTools")
install.packages("tidyr")
install.packages("lubridate")
install.packages("psych")
#### 사용한 라이브러리

library(dplyr)
library(DescTools)
library(tidyr)
library(lubridate)
library(psych)
## 1. 고객 데이터 불러오기

* reservation.csv : 고객예약정보
* user.csv : 회원정보
* payment.csv : 결제정보
* product.csv : 이용권정보
### 1) csv 파일 불러오기
reserve_data = read.csv(file= "D:/R/reservation.csv", fileEncoding = "UTF-8-BOM")   #고객예약정보

user_data = read.csv(file= "D:/R/user.csv", fileEncoding = "UTF-8-BOM")   #회원정보

payment_data = read.csv(file = "D:/R/payment.csv", fileEncoding = "UTF-8-BOM")   #결제정보

product_data = read.csv(file = "D:/R/product.csv", fileEncoding = "UTF-8-BOM")   #이용권정보

str(reserve_data)   #데이터 살펴보기
str(user_data)
str(payment_data)
str(product_data)

##결측값, 특이값 제거
## reserve_data
#좌석예약 종료시간 결측값 제거
nrow(reserve_data)   #14461
reserve_data[18,5]    #""
table(reserve_data$petime == "")   #1228개 종료시간 없음
reserve_filter <- reserve_data[!reserve_data$petime == "",]    #좌석예약 종료시간 없는 행 삭제
nrow(reserve_filter)    #13233
## user_data
#생년월일, 성별 결측값 제거
nrow(user_data)    #953
user_data$ubird <- as.Date(user_data$ubird)
user_data[13,2]   #NA

user_filter <- user_data %>% filter(!is.na(ubird))   #생년월일이 없는 행 삭제
user_filter$ubird <- as.character(user_filter$ubird)

user_filter <- user_filter[order(user_filter$ubird, decreasing = T),]
user_filter <- user_filter[-1:-12,]   #2019년이후 값이 들어있는 행 삭제

user_filter <- user_filter[!user_filter$usex == 0,]   #성별이 0인 행 삭제

user_filter <- user_filter[!user_filter$maketime == "",]   #가입시간 없는 행 삭제
nrow(user_filter)    #688

## product_data
unique(product_data$pname)   #21

product_filter <- product_data[product_data$pusecheck == 'Y',]   #현재 판매되는 이용권(Y-판매중)만 남기고 삭제
product_filter <- product_filter[-9:-10,]   #특이값 삭제
unique(product_filter$pname)   #8

product_filter <- product_filter[,-8:-11]   #결측값이 많은 컬럼 삭제

##변수 파악, 이해 및 변환
#1)변수 파악
str(reserve_filter)   #데이터 살펴보기
str(user_filter)
str(payment_data)
str(product_filter)

#2)변수 변환
## reserve_filter
reserve_filter$usenum = as.factor(reserve_filter$usenum)   #예약순서 factor로 변경
reserve_filter$uid = as.factor(reserve_filter$uid)   #고객 번호 factor로 변경
reserve_filter$pdnum = as.factor(reserve_filter$pdnum)   #이용권 구매 번호 factor로 변경
reserve_filter$pstime = as.Date(reserve_filter$pstime)   #좌석예약시작시간 data로 변경
reserve_filter$petime = as.Date(reserve_filter$petime)   #좌석예약종료시간 data로 변경
reserve_filter$cid = as.factor(reserve_filter$cid)   #좌석 번호 factor로 변경
reserve_filter$pid = as.factor(reserve_filter$pid)   #이용권 번호 factor로 변경


## user_filter
user_filter$uid = as.factor(user_filter$uid)   #고객 번호 factor로 변경
user_filter$usex = as.factor(user_filter$usex)   #성별 factor로 변경
user_filter$maketime = as.Date(user_filter$maketime)   #가입일자 date로 변경


## payment_data
payment_data$snum = as.factor(payment_data$snum)   #구매순서번호 factor로 변경
payment_data$payid = as.factor(payment_data$payid)   #구매id factor로 변경
payment_data$uid = as.factor(payment_data$uid)   #고객 번호 factor로 변경
payment_data$pid = as.factor(payment_data$pid)   #이용권 번호 factor로 변경
payment_data$payreqtime = as.Date(payment_data$payreqtime)   #구매요청시간 date로 변경
payment_data$paycomptime = as.Date(payment_data$paycomptime)   #구매완료시간 date로 변경
payment_data$numcnt = as.factor(payment_data$numcnt)    #1회 결제시 구매이용권 개수 factor로 변경


## product_filter
product_filter$pid = as.factor(product_filter$pid)   #이용권 번호 factor로 변경
product_filter$pcategory = as.factor(product_filter$pcategory)   #상품 카테고리번호 factor로 변경


##3.단변량분석1
#1) 기술통계량 summary

summary(reserve_filter)
summary(user_filter)
summary(user_filter)
summary(product_filter)
#2) 명목형 변수 분석
## reserve_filter
Desc(reserve_filter$cid, plotit = TRUE, main = "좌석 예약빈도")   #예약 선호 좌석
Desc(reserve_filter$uid, plotit = TRUE, main = "고객 예약빈도")   #고객 예약 빈도(일별아님주의)
## user_filter
Desc(user_filter$usex, plotit = TRUE, main = "성별: 남자1, 여자2")   #성별
## payment_data
Desc(payment_data$uid, plotit = TRUE, main = "고객 결제빈도")   #고객 결제 빈도(상위id만 나와서 재부호화 해야함)
Desc(payment_data$numcnt, plotit = TRUE, main = "1회 결제시 구매 이용권 개수")   #1회 구매 수량
Desc(payment_data$pname, plotit = TRUE, main = "이용권(이름) 구매빈도")   #이용권 구매 빈도, '4주(28일)' 및 '2주(14일)'은 사물함 이용권

#3)연속형 파생변수 생성 - 24,25번째 변수
## user_filter에 age(나이) 컬럼 추가
today <- Sys.Date()   #오늘 날짜

user_filter$ageDays <- difftime(today, user_filter$ubird, units = "days")   #나이(일) 컬럼 생성

user_filter$ageDays <- as.integer(user_filter$ageDays)   #나이 integer로 변경
user_filter$ageYears <- ceiling(user_filter$ageDays/365)   #나이(년) 컬럼 추가

#4)연속형 변수 분석
## user_filter
Desc(user_filter$ageYears, plotit = TRUE, main = "나이")
## payment_data
Desc(payment_data$pdmoney, plotit = TRUE, main = "결제금액")

##단변량분석 2
#1)일별, 월별 이용권 구매 히스토그램
#월별 구매 빈도 수 
month_p <- cbind(payment_data, month=month(payment_data$paycomptime))
month_p
Desc(month_p, plotit = TRUE, main = "월별 구매 빈도")
#일별 구매 빈도 수 
day_p <- cbind(payment_data, day=day(payment_data$paycomptime))
day_p
Desc(day_p, plotit = TRUE, main = "일별 구매 빈도")
#시간대별 구매 빈도 수 
#hoUr <- cbind(payment_data, hour=hour(payment_data$paycomptime))
#hoUr
#Desc(hoUr, plotit = TRUE, main = "시간별 구매 빈도") #실행했을 때 시간이 0이 나오네요 ,, ㅠㅠ


#2) 일별, 월별 예약 히스토그램

month_R <- cbind(reserve_filter, month=month(reserve_filter$pstime))
month_R
Desc(month_R, plotit = TRUE, main = "월별 좌석 예약 빈도")
#일별 구매 빈도 수 
day_R <- cbind(reserve_filter, day=day(reserve_filter$pstime))
day_R
Desc(day_R, plotit = TRUE, main = "일별 좌석 예약 빈도")

month_R <- as.numeric(month_R)


##다변량 분석
##변수 변환 및 파생변수
#1. 나이X성별 고객 군집 산점도
str(user_filter$ubird)
user_age <- cbind(user_filter, year=year(user_filter$ubird)) #사용자 생년월일에서 나이 구하기
user_age <- as.numeric(user_filter$ageYears) #수치형으로 변환

user_s_a = data.frame(cbind(user_filter$usex,user_filter$ageYears))
plot(user_filter$ageYears ~ user_filter$usex,data = user_s_a,
     main = "고객 군집 산점도",
     xlab = "성별",
     ylab = "나이")

res = lm(user_filter$ageYears ~ user_filter$usex,data = user_s_a) #산점도를 표현할 수 있는 선형모델 (회귀식)
abline(res) #선그리기

##나이대X이용권 종류 산점도
reserve_user <- inner_join(reserve_filter,user_filter)#Reserve data에 User 정보 추가
reserve_user <- na.omit(reserve_user) # 결측값 제거
reserve_user
cbind(reserve_user, year=year(reserve_user$ubird)) #사용자 생년월일에서 나이 구하기

plot(reserve_user$ageYears ~ reserve_user$pid,
     main = "나이별 이용권 구매 산점도",
     xlab = "이용권 ID",
     ylab = "나이")

age_pid_res = lm(reserve_user$ageYears ~ reserve_user$pid) #산점도를 표현할 수 있는 선형모델 (회귀식)
abline(age_pid_res) #선그리기


##나이X 월별 예약 산점도
cbind(reserve_user, month=month(reserve_user$pstime))

##여기 해결x.... 
'''plot(month ~ pid,
     main = "나이X 월별 예약",
     xlab = "월",
     ylab = "이용권 종류")'''


