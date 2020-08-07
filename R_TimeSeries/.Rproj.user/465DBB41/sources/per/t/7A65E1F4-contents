library(fpp2)

oildata <- window(oil, start = 1996)
autoplot(oildata)+
  ylab("원유(백만 톤)")+xlab("연도")

oiddata <- window(oil, start=1996)

fc <- ses(oildata, h =5)

round(accuracy(fc),2)

autoplot(fc)+
  autolayer(fitted(fc), series="적합값")+
  ylab("원유(백만 톤)")+xlab("연도")+
  ggtitle("단순 지수평활로 얻은 예측값")

air <-window(ausair, start=1990)
fc <- holt(air, h=5)

fc <- holt(air, h=15)
fc2 <-  holt(air, damped = T, phi = 0.9, h=15)

autoplot(air)+
  autolayer(fc, series = "홀트 기법", PI=F)+
  autolayer(fc2, series = "감쇠 홀트 기법", PI=F)+
  ggtitle("홀트 기법으로 얻은 예측값")+xlab("연도")+
  ylab("호주 항곡백 (백만 명)")+
  guides(colour=guide_legend(title="예측값"))

autoplot(livestock)+
  xlab("연도")+ylab("아시아의 양 목축 (단위:백만)")

e1 <- tsCV(livestock, ses, h =1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=T, h=1)

mean(e1^2, na.rm=T)
mean(e2^2, na.rm=T)
mean(e3^2, na.rm=T)
mean(abs(e1),na.rm=T)
mean(abs(e2),na.rm=T)
mean(abs(e3),na.rm=T)


fc <-  holt(livestock, damped=T)
fc[["model"]]

autoplot(fc)+
  xlab("연도")+ylab("아시아의 양 목축 (단위 : 백만)")+
  ggtitle("감쇠 홀트 기법으로 얻은 예측값")

aust <- window(austourists, start=2005)

fit1 <- hw(aust, seasonal = "additive")
fit2 <- hw(aust, seasonal = "multiplicative")

autoplot(aust)+
  autolayer(fit1, series="HW 덧셈 예측", PI=F)+
  autolayer(fit2, series = "HW 곱셉 예측", PI=F)+
  xlab("연도")+
  ylab("호주 국제선 여행객 숙박일 (단위:백만)")+
  guides(colour=guide_legend(title="예측"))

str(aust)

hw(aust, damped = T, seasonal = "multiplicative")

fc <- hw(subset(hyndsight, end=length(hyndsight)-35),
         damped=T, seasonal = "multiplicative", h =35)

autoplot(hyndsight)+
  autolayer(fc, series="HW곱셈 감쇠",PI=F)+
  guides(colour=guide_legend(titile="일별 예측값"))

aust <- window(austourists, start=2005)
fit <- ets(aust)
fit
summary(fit)

autoplot(fit)+
  ggtitle("ETS(M,A,M) 기법의 성분")

cbind('잔차'=residuals(fit),
      '예측 오차'=residuals(fit, type='response')) %>% 
  autoplot(facet=T)+xlab("연도")+ylab(" ")
