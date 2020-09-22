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

##3.
flights %>% is.na()

##3.3 arrange()로 행 정렬하기
arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

df<- tibble(x=c(5,2, NA))
arrange(df, x)

arrange(df, desc(x))

##3.4 select()로 열 선택하기
select(flights, year, month, day)
select(flights, year:day)
select(flights,-(year:day))

rename(flights, tail_num=tailnum)

select(flights, time_hour, air_time, everything())
select(flights, time_hour, everything())

select(flights, contains('time'))

##3.5 mutate()새로운 변수 추가하기
flights_sml <- select(flights,
                      year:day,
                      ends_with('delay'),
                      distance,
                      air_time)
flights_sml
mutate(flights_sml,
       gain=arr_delay-dep_delay,
       speed=distance/air_time*60)

mutate(flights_sml,
       gain=arr_delay-dep_delay,
       hours=air_time/60,
       gain_per_hour = gain/hours)

transmute(flights_sml,
          gain=arr_delay-dep_delay,
          hours=air_time/60,
          gain_per_hour = gain/hours)

##3.5.1 유용한 생성 함수
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute=dep_time %% 100)

(x <- 1:10)
lag(x)
lead(x)
x
cumsum(x)
cummean(x)
cummin(x)
cummax(x)

y <- c(1,2,2,NA,3,4)
min_rank(y)

min_rank(desc(y))

row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

## 3.6 summarize()로 그룹화 요약하기

summarize(flights, delay=mean(dep_delay, na.rm=TRUE))

by_day <- group_by(flights, year, month,day)
summarize(by_day, delay=mean(dep_delay, na.rm=T))

## 3.6.1 파이프로 여러 작업 결합하기
by_dest <-  group_by(flights, dest)
delay <- summarize(by_dest,
                   count=n(),
                   dist=mean(distance, na.rm=T),
                   delay=mean(arr_delay, na.rm=T))

delay <- filter(delay, count > 20, dest != "HNL")
delay

ggplot(delay, aes(dist, delay))+
  geom_point(aes(size=count), alpha=1/3)+
  geom_smooth(se=F)

delays <- flights %>% 
  group_by(dest) %>% 
  summarize(
    count=n(),
    dist=mean(distance,na.rm=T),
    delay=mean(arr_delay, na.rm=T)
  ) %>% 
  filter(count>20, dest !='HNL')

delays

##3.6.2 결측값

flights %>% 
  group_by(year, month, day) %>% 
  summarize(
    mean = mean(dep_delay,na.rm = T)
  )

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean=mean(dep_delay))

##3.6.3 카운트

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(
    delay=mean(arr_delay)
  )

ggplot(delays, aes(delay))+
  geom_freqpoly(binwidth=10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(
    delay = mean(arr_delay, na.rm =T),
    n=n()
  )

ggplot(delays, aes(x=n, y=delay))+
  geom_point(alpha=1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(aes(x=n, y=delay))+
  geom_point(alpha=1/10)

batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarize(
    ba=sum(H, na.rm=T)/sum(AB, na.rm=T),
    ab=sum(AB, na.rm=T)
  )

batters %>% 
  filter(ab>100) %>% 
  ggplot(aes(ab,ba))+
  geom_point()+
  geom_smooth(se=F)

batters %>% 
  arrange(desc(ba))

##3.6.4

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay>0])
  )
