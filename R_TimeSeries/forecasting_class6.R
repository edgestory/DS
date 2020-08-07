library(fpp2)
#install.packages("seasonal")

autoplot(elecsales)+ xlab("연도")+ ylab("GWh")+
  ggtitle("연간 전력 판매: 남 호주")

elecsales

ma(elecsales,2)

autoplot(elecsales, series="데이터")+
  autolayer(ma(elecsales,5), series = "5-MA")+
  xlab("연도")+ylab("GWh")+
  ggtitle("연간 전력 판매량: 남 호주")+
  scale_colour_manual(values=c("데이터"="grey50","5-MA"="red"),
                      breaks=c("데이터","5-MA"))+
  guides(colou=guide_legend(title=" "))


(2354.34+2379.71)/2
(2318.52+2379.71)/2

autoplot(elecequip, series="데이터")+
  autolayer(ma(elecequip, 12), series="12-MA")+
  xlab("연도")+ylab("신규 주문 지수")+
  ggtitle("전자 장비 제조(유럽 지역)")+
  scale_colour_manual(
    values=c("데이터"="grey", "12-MA"="red"),
    breaks=c("데이터","12-MA")
  )+
  guides(colour=guide_legend(title=" "))

elecequip %>% 
  decompose(type="multiplicative") %>% 
  autoplot()+ xlab("연도")+
  ggtitle("전자 장비 지수의 고전적 곱셈 분해")


library(seasonal)
elecequip %>% 
  seas(x11="")-> fit

fit

autoplot(fit)+
  ggtitle("전자 장비 지수의 x11 분해")


autoplot(elecequip, series="데이터")+
  autolayer(trendcycle(fit), series="추세")+
  autolayer(seasadj(fit), series='계절성으로 조정된 것')+
  xlab("연도")+ylab("신규 주문 지수")+
  ggtitle("전자 장비 제조 (유럽 지역)")+
  scale_colour_manual(values = c("데이터"="gray",
                                 "계절성으로 조정된 것"="blue",
                                 "추세"="red"),
                    breaks=c("데이터","계절성으로 조정된 것",
                                          "추세"))

fit %>% 
  seasonal() %>%
  ggsubseriesplot()


elecequip %>% 
  seas() %>% 
  autoplot()+
  ggtitle("전자 장비 지수의 SEATS분해")

elecequip %>% 
  stl(t.window=13, s.window="periodic", robust=T) %>% 
  autoplot()

fit <- stl(elecequip, t.window=13, s.window="periodic", robust=T)
fit %>% 
  seasadj() %>% 
  naive() %>% 
  autoplot()+ylab("신규 구매 지수")+
  ggtitle("계절성으로 조정된 데이터의 단순 예측값")

fit %>% 
  forecast(method="naive") %>% 
  autoplot() + ylab("신규 구매 지수")+
  ggtitle("STL과 확률보행으로 얻은 예측값")

fcast <- stlf(elecequip, method='naive')
fcast
