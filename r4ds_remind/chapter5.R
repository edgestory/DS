library(tidyverse)

ggplot(data = diamonds)+
  geom_bar(aes(x=cut))

diamonds %>% 
  count(cut)

ggplot(diamonds)+
  geom_histogram(aes(carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat,0.5))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(smaller, aes(x=carat))+
  geom_histogram(binwidth = 0.1)

ggplot(smaller, aes(x=carat, color=cut))+
  geom_freqpoly(binwidth=0.1)

###5.3.2 일반적인 값
ggplot(smaller, aes(x=carat))+
  geom_histogram(binwidth=0.01)

ggplot(data=faithful, aes(x=eruptions))+
  geom_histogram(binwidth = 0.25)

###5.3.3 이상값
ggplot(diamonds)+
  geom_histogram(aes(x=y), binwidth = 0.5)

diamonds %>% 
  ggplot(aes(x=y))+
  geom_histogram(binwidth = 0.5)+
  coord_cartesian(ylim=c(0,50))

unusual <- diamonds %>% 
  filter(y<3|y>20) %>% 
  select(price,x,y,z) %>% 
  arrange(y)

unusual

###5.4 결측값
diamonds2 <- diamonds %>% 
  filter(between(y,3,20))

diamonds2 <- diamonds %>% 
  mutate(y=ifelse(y<3|y>20,NA, y))

diamonds2 %>% 
  filter(is.na(y)==1)

ggplot(diamonds2, aes(x=x, y=y))+
  geom_point()

ggplot(diamonds2, aes(x=x, y=y))+
  geom_point(na.rm=T)

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min/60
) %>% 
  ggplot(aes(sched_dep_time))+
  geom_freqpoly(
    aes(color=cancelled),
    binwidth = 1/4
  )

