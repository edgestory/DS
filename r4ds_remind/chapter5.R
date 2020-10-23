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

### 5.5.1 범주형 변수와 연속형 변수
ggplot(diamonds, aes(x=price))+
  geom_freqpoly(aes(color=cut), binwidth=500)

ggplot(diamonds)+
  geom_bar(aes(x=cut))

diamonds %>% 
  ggplot(aes(price, ..density..))+
  geom_freqpoly(aes(color=cut),binwidth=500)

diamonds %>% 
  ggplot(aes(cut, price))+
  geom_boxplot()

ggplot(mpg, aes(class, hwy))+
  geom_boxplot()

ggplot(mpg)+
  geom_boxplot(
    aes(
      x= reorder(class, hwy, FUN=median),
      y=hwy
    )
  )

ggplot(mpg)+
  geom_boxplot(
    aes(
      x= reorder(class, hwy, FUN=median),
      y=hwy
    )
  )+
  coord_flip()

## 5.5.3 두개의 범주형 변수
ggplot(diamonds)+
  geom_count(aes(cut, color))

diamonds %>% 
  count(color,cut)

diamonds %>% 
  count(color, cut) %>% 
  ggplot(aes(color, cut))+
  geom_tile(aes(fill=n))

diamonds %>% 
  ggplot(aes(carat,price))+
  geom_point()

diamonds %>% 
  ggplot(aes(carat, price))+
  geom_point(
    aes(carat, price),
    alpha=1/100
  )

ggplot(smaller)+
  geom_bin2d(aes(carat, price))

#install.packages("hexbin")
ggplot(smaller)+
  geom_hex(aes(carat, price))

ggplot(smaller, aes(y=price,group=cut_width(carat,0.1)))+
  geom_boxplot()

###5.6 패턴과 모델
ggplot(faithful)+
  geom_point(aes(x=eruptions, y=waiting))

library(modelr)

mod <- lm(log(price)~ log(carat), data=diamonds)
mod

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(diamonds2)+
  geom_point(aes(x=carat, y=resid))

ggplot(diamonds2)+
  geom_boxplot(aes(cut, resid))

