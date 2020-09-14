library(tidyverse)

mpg

##1.2.2 ggplot생성하기
ggplot(data=mpg)+
  geom_point(aes(x=displ, y=hwy))

##1.2.4 연습문제
###1.
ggplot(mpg)

###2
str(mpg)

###3
?mpg

###4.
ggplot(mpg)+
  geom_point(aes(x=hwy, y=cyl))

###5.
ggplot(mpg)+
  geom_point(aes(x=class, y=drv))

#1.3 심미성 매핑

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, color=class))

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, size= class))

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, alpha=class))

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, shape=class))

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy), color='blue')

##1.3.1 연습문제
###1.

###2.
str(mpg)

###3.
ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, color=cyl))

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, shape=cyl))

ggplot(mpg)+
  geom_point(aes(displ, y=hwy, size=cyl))

###4.
ggplot(mpg)+
  geom_point(aes(displ, y=hwy, size=displ))

?geom_point

###5.
ggplot(mpg)+
  geom_point(aes(displ, hwy,size=3
                 ,fill='white', stroke=1))

###6.
ggplot(mpg)+
  geom_point(aes(displ, hwy,color=displ<5))


#1.4 자주 일어나는 문제들

ggplot(mpg)+
  geom_point(aes(displ, hwy))


#1.5 facet
ggplot(mpg)+
  geom_point(aes(displ, hwy))+
  facet_wrap(~class, nrow=2)

ggplot(mpg)+
  geom_point(aes(displ, hwy))+
  facet_grid(drv~cyl)

##1.5 연습문제

###1.
ggplot(mpg)+
  geom_point(aes(displ, hwy))+
  facet_wrap(~cyl)

###2. 
ggplot(mpg)+
  geom_point(aes(drv, cyl))+
  facet_grid(drv~cyl)

###3.
ggplot(mpg)+
  geom_point(aes(displ, hwy))+
  facet_grid(drv~.)

###4.
ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy))+
  facet_wrap(~class, nrow= 2)
