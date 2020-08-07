library(fpp2)

beer2 <- window(ausbeer, start=1992, end=c(2007,4))

beer2

autoplot(beer2)+
  autolayer(meanf(beer2, h=11), series = "평균", PI=F)+
  autolayer(naive(beer2, h=11), series = "단순", PI=F)+
  autolayer(snaive(beer2, h=11), series = "계절성 단순", PI=F)+
  autolayer(rwf(beer2, h=11, drift=T), series = "표류기법", PI=F)+
  ggtitle("분기별 맥주 생산량 예측값")+
  xlab("연도")+ylab("단위: 백만 리터")+
  guides(colour=guide_legend(title="예측"))

autoplot(goog200)+
  autolayer(meanf(goog200, h=40), series = "평균",PI=F)+
  autolayer(rwf(goog200,h=40), series = "단순",PI=F)+
  autolayer(rwf(goog200, h=40,drift = T), series = "표류",PI=F)+
  ggtitle("구글 주식(2013년 12월 6일까지)")+
  xlab("날짜")+ylab("종가(미국 달러")+
  guides(colour=guide_legend(title="예측"))

dframe <- cbind(Monthly = milk,
                DilyAverage = milk/monthdays(milk))
dframe

colnames(dframe)<-c("월별", "일별 평균")
autoplot(dframe, facet=T)+
  xlab("연도")+ylab("파운드")+
  ggtitle("젖소별 우유 생산량")

lambda<-BoxCox.lambda(elec)
autoplot(BoxCox(elec, lambda))

#Alt + - = <-
#Ctrl + Shift + M = %>%
# Ctrl _ Shift _ C = 블록 주석 처리

file_list <-  list.files()
file_list


fc <- rwf(eggs, drift=TRUE, lambda = 0, h=50, level=80)
fc2 <- rwf(eggs, drift = TRUE, lambda = 0, h=50, level=80,
           biasadj = T)

autoplot(eggs) +
  autolayer(fc, series = "단순 역변환")+
  autolayer(fc2, series ="편향 조정", PI = F)+
  guides(colour = guide_legend(title="예측"))

autoplot(goog200)+
  xlab("날짜")+ylab("종가(미국 달러)")+
  ggtitle("구글 주식 일별 가격(2013년 12월 6일까지)")

res <- residuals(naive(goog200))
autoplot(res)+xlab("날짜")+ylab("")+
  ggtitle("나이브 기법에서 얻은 잔차")

gghistogram(res)+ggtitle("잔차의 히스토그램")
ggAcf(res) + ggtitle("잔차의 ACF")
ggAcf(goog200)


Box.test(res, lag=10, fitdf=0)
Box.test(res, lag=10, fitdf=0, type="Lj")

checkresiduals(naive(goog200))

window(ausbeer, start=1995)
subset(ausbeer, start=length(ausbeer)-4*5)
subset(ausbeer, start=197)
subset(ausbeer, quarter = 1)

tail(ausbeer, 4*5)

beer2 <- window(ausbeer, start=1992, end=c(2007,4))
beer2

beerfit1 <- meanf(beer2, h=10)
beerfit2 <- rwf(beer2, h=10)
beerfit3 <- snaive(beer2, h=10)

autoplot(window(ausbeer, start=1992))+
  autolayer(beerfit1, series = "평균", PI=F)+
  autolayer(beerfit2, series="단순", PI=F)+
  autolayer(beerfit3, series ="계절성 단순", PI=F)+
  xlab("연도")+ylab("백만 리터")+
  ggtitle("분기별 맥주 생산량 예측값")+
  guides(colour=guide_legend(title="예측"))

beer3 <- window(ausbeer, start=2000)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift = T, h = 40)

autoplot(subset(goog, end =240)) +
  autolayer(googfc1, PI=F, series="평균")+
  autolayer(googfc2, PI=F, series = "단순")+
  autolayer(googfc3, PI=F, series = "표류")+
  xlab("날짜")+ylab("종가(미국 달러)")+
  ggtitle("구글 일별 주가(2013년 12월 6일가지)")+
  guides(colour=guide_legend(title="예측"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)


e <- tsCV(goog200, rwf, drift = T, h = 1)
sqrt(mean(e^2, na.rm = T))

sqrt(mean(residuals(rwf(goog200, drift=T))^2,na.rm=T))

goog200 %>% tsCV(forecastfunction = navie) -> e
e

e^2 %>% mean(na.rm=T) %>% sqrt()

goog200 %>%
  rwf(drift = T) %>%
  residuals() -> res

res^2 %>% mean(na.rm=T) %>% sqrt()

naive(goog200,h=10)

autoplot(naive(goog200),PI=F)

forecast(ausbeer, h=4)
ausbeer

lambda <-BoxCox.lambda(usnetelec)
autoplot(BoxCox(usnetelec, lambda))
autoplot(usnetelec)

lambda <-BoxCox.lambda(usgdp)
autoplot(BoxCox(usgdp, lambda))
autoplot(usgdp)    

autoplot(cangas)
lambda <- BoxCox.lambda(cangas)
autoplot(BoxCox(cangas,lambda))

beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc,PI=F)
res <- residuals(fc)
autoplot(res)

checkresiduals(fc)


autoplot(WWWusage)

autoplot(bricksq)


train1 <- window(visnights[,"QLDMetro"], end = c(2015,4))
train2 <- window(visnights[,"QLDMetro"], end = c(2014,4))
train3 <- window(visnights[,"QLDMetro"], end = c(2013,4))

fc1 <-snaive(train1)
fc2 <-snaive(train2)
fc3 <-snaive(train3)

test1 <- window(visnights[,"QLDMetro"], start=c(2016,1))
test2 <- window(visnights[,"QLDMetro"], start=c(2015,1))
test3 <- window(visnights[,"QLDMetro"], start=c(2014,1))

accuracy(fc1, test1)
accuracy(fc2, test2)
accuracy(fc3, test3)
