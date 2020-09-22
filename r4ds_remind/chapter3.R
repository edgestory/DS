library(tidyverse)
#install.packages("nycflights13")
library(nycflights13)

flights

##3.2 filter()로 행 필터링하기
filter(flights, month==1, day==1)

jan1 <- filter(flights, month == 1, day ==1)
jan1

(dec25 <- filter(flights, month == 12, day ==25))

##3.2.1 비교연산
sqrt(2)^2 == 2
1/49*49 == 1

near(sqrt(2)^2,2)

near(1/49*49,1)

##3.2.2 논리연산자
filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11,12))
nov_dec

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay<=120, dep_delay <= 120)

###3.2.3 결측값
NA > 5
10 == NA
NA + 10
NA/2

NA == NA
x <- NA

y <- NA

x == y

is.na(x)

df <- tibble(x=c(1,NA,3))

filter(df, x>1)
filter(df, is.na(x)|x>1)

##3.2.4 연습문제
### 1.
####a.
filter(flights, dep_delay >= 120)

####b.
filter(flights, dest == 'IAH' | dest == 'HOU')

####c.
filter(flights, carrier %in% c('UA','AA','DL'))

flights$carrier %>% 
  unique()

###3.
flights %>% is.na()

###3.3 arrange()로 행 정렬하기
