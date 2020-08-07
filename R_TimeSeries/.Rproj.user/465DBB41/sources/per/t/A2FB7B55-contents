#install.packages("fpp2")
library(forecast)
library(ggplot2)
library(tidyverse)
library(fpp2)

y <- ts(c(123,39,78,52,110), start=2012)

melsyd

autoplot(data=melsyd,x=Economy.class)

?autoplot
  
autoplot(melsyd[,"Economy.Class"])+
  ggtitle("이코노미석 탑승객: 멜버른-시드니")+
  xlab("연도")+
  ylab("탑승객(eksdnl: 100명")

a10

autoplot(a10)+
  ggtitle("당뇨병 약 판매량")+
  ylab("판매량(단위: 백만 달러")+
  xlab("연도")

ggseasonplot(a10, year.labels = T, year.labels.left = T)+
  ylab("백만달러")+
  xlab("월")+
  ggtitle("계절성 그래프: 당뇨병 약 판매량")


ggseasonplot(a10, polar =T)+
  ylab("백만 달러")+
  xlab("월")+
  ggtitle("계절성 극좌표 그래프 : 당뇨병 약 판매량")

a10


ggsubseriesplot(a10)+
  ylab("백만 달러")+
  xlab("월")+
  ggtitle("계절성 부시계열 그래프: 당뇨병 약 판매량량")

autoplot(elecdemean)


beer2 <- window(ausbeer, start=1992)
stats::lag(beer2,k=2)
stats::lag(beer2,k=6)


ggAcf(beer2)


aelec <- window(elec, start = 1980)
aelec

autoplot(aelec) + xlab("연도")+ylab("기가와트시(GWh)")

ggAcf(aelec, lag=48)

set.seed(30)
y <- ts(rnorm(50))
autoplot(y)+ggtitle("백색잡음")

ggAcf(y)

?gold

autoplot(gold)
gold

autoplot(woolyrnq)

autoplot(gas)

frequency(gold)
frequency(gas)
frequency(woolyrnq)

tute1 <- read.csv("tute1.csv", header=T)
View(tute1)

tute1

mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)

autoplot(mytimeseries, facets=T)

#install.packages("readxl")

retaildata <- readxl::read_excel("retail.xlsx", skip=1)

myts <- ts(retaildata[,"A3349873A"], frequency = 12 , start=c(1982, 4))
myts

autoplot(myts)

ggseasonplot(myts)

ggsubseriesplot(myts)

gglagplot(myts)

ggAcf(myts)

bicoal
chicken
dole
usdeaths
lynx

goog

autoplot(goog)

ggseasonplot(writing)
ggseasonplot(fancy)
ggseasonplot(a10)
ggseasonplot(h02)

ggsubseriesplot(hsales)

gglagplot(hsales)
ggAcf(hsales)

arrivals

autoplot(arrivals, facet= T)
ggseasonplot(arrivals[,"Japan"])
ggseasonplot(arrivals[,"UK"])
ggseasonplot(arrivals[,"NZ"])
ggseasonplot(arrivals[,"US"])

ggsubseriesplot(arrivals[,"Japan"])
ggsubseriesplot(arrivals[,"UK"])
ggsubseriesplot(arrivals[,"NZ"])
ggsubseriesplot(arrivals[,"US"])

arrivals

pigs

mypigs <- window(pigs, start=1990)
mypigs

par(mfrow=c(2,1),pty="s")
autoplot(mypigs)
ggAcf(mypigs)

ddj <- diff(dj)

ggAcf(dj)

ggAcf(ddj)
