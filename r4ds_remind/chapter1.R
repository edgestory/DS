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

##1.6 연습문제

ggplot(mpg)+
  geom_point(aes(displ, hwy))

ggplot(mpg)+
  geom_smooth(aes(displ, hwy))

ggplot(mpg)+
  geom_smooth(aes(x=displ, y=hwy, linetype=drv))

ggplot(mpg)+
  geom_smooth(aes(displ, hwy))

ggplot(mpg)+
  geom_smooth(aes(displ, hwy, group=drv))

ggplot(mpg)+
  geom_smooth(aes(displ, hwy, color=drv))

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy))+
  geom_smooth(aes(x=displ, y=hwy))

ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  geom_smooth()

ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point(aes(color=class))+
  geom_smooth()

ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point(aes(color=class))+
  geom_smooth(data=filter(mpg, class=='subcompact'), se=FALSE)

##1.6.1 연습문제

###1. 
ggplot(mpg, aes(x=displ, y=hwy))+
  geom_line()

ggplot(mpg, aes(y=hwy,group=displ))+
  geom_boxplot()

###2.
ggplot(mpg, aes(displ, hwy, color=drv))+
  geom_point()+
  geom_smooth(se=FALSE)

###3.
ggplot(mpg, aes(displ, hwy, color=drv,show.legend = FALSE))+
  geom_point()+
  geom_smooth()

###4.

###5.
ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()+
  geom_smooth()

ggplot()+
  geom_point(data=mpg, aes(x=displ, y=hwy))+
  geom_smooth(data=mpg, aes(x=displ, y=hwy))


###6.

##1.7 통계적 변환
ggplot(diamonds)+
  geom_bar(aes(x=cut))

ggplot(diamonds)+
  stat_count(aes(x=cut))

demo <- tribble(
  ~cut, ~freq,
  "Fair", 1610,
  "Good", 4906,
  "Very Good", 12082,
  "Premium", 13791,
  "Ideal", 21551
)

ggplot(demo)+
  geom_bar(aes(x=cut,y=freq),stat='identity')

ggplot(diamonds)+
  geom_bar(
    aes(x=cut, y=..prop.., group=1)
  )


ggplot(diamonds)+
  stat_summary(
    mapping=aes(x=cut, y=depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

## 1.7.1 연습문제
### 1.
ggplot(diamonds) + geom_bar(aes(x=cut,y=depth),stat = "summary",fun=mean)


###2.
ggplot(diamonds) +geom_col(aes(x=cut,y=depth))

###3.


###4.

###5.
ggplot(diamonds)+
  geom_bar(aes(x=cut, y=..prop..,group=1))

ggplot(diamonds)+
  geom_bar(aes(x=cut,group=1,fill=cut,y=..prop..))

##1.8 위치 조정

ggplot(diamonds)+
  geom_bar(aes(x=cut, color=cut))

ggplot(diamonds)+
  geom_bar(aes(x=cut, fill=cut))

ggplot(diamonds)+
  geom_bar(aes(x=cut, fill=clarity))

ggplot(diamonds,aes(x=cut, fill=clarity))+
  geom_bar(alpha=1/5, position='identity')

ggplot(diamonds,aes(x=cut, color=clarity))+
  geom_bar(fill=NA, position='identity')

ggplot(diamonds)+
  geom_bar(aes(x=cut, fill=clarity), position='fill')

ggplot(diamonds)+
  geom_bar(aes(x=cut, fill=clarity), position='dodge')

ggplot(mpg)+
  geom_point(
    aes(x=displ, y=hwy),
    position='jitter'
  )

##1.9좌표계

ggplot(mpg, aes(x=class, y=hwy))+
  geom_boxplot()

ggplot(mpg,aes(class, hwy))+
  geom_boxplot()+
  coord_flip()

bar <- ggplot(diamonds)+
  geom_bar(
    aes(cut, fill=cut),
    show.legend = FALSE,
    width=1
  )+theme(aspect.ratio = 1)+
  labs(x=NULL,y=NULL)

bar+coord_flip()
bar+coord_polar()
