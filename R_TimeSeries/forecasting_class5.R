library(fpp2)


colnames(uschange)[1:2] <- c("소비","소득")
autoplot(uschange[,c("소비","소득")])+
  ylab("%변화")+xlab("연도")+
  guides(colour=guide_legend(title=" "))

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=소득, y= 소비))+
  xlab("소비 (분기별 %변화")+
  ylab("소득 (분기별 %변화")+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)


uschange

tslm(소비 ~ 소득, data = uschange)

colnames(uschange) <- c("소비","소득","생산","저축","실업률")
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

colnames(uschange) <- c("Consumption","Income","Production",
                        "Savings","Unemployment")


fit.consMR <- tslm(
  Consumption ~ Income+Production+Unemployment+Savings,
  uschange
)

summary(fit.consMR)

autoplot(uschange[, 'Consumption'], series="데이터")+
  autolayer(fitted(fit.consMR), series = "적합값")+
  xlab("연도")+ylab("")+
  ggtitle("미국 소비 주출의 백분율 변화")+
  guides(colour=guide_legend(title=" "))

cbind(Data= uschange[,"Consumption"],
      Fitted = fitted(fit.consMR)) %>% 
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted))+
  geom_point()+
  ylab("적합값 (예측된 값)")+
  xlab("데이터 (예측된 값)")+
  ggtitle("미국 소비 지출의 백분율 변화")+
  geom_abline(intercept = 0, slope=1)

checkresiduals(fit.consMR)

df <- as.data.frame(uschange)
df[,"Residuals"] <- as.numeric(residuals(fit.consMR) )

p1 <- ggplot(df, aes(x=Income, y=Residuals))+
  geom_point()+xlab("소득")+ylab("잔차")

p2 <- ggplot(df, aes(x=Production, y=Residuals))+
  geom_point()+xlab("생산")+ylab("잔차")

p3 <- ggplot(df, aes(x=Savings, y=Residuals))+
  geom_point()+ xlab("소득")+ylab("잔차")

p4 <- ggplot(df, aes(x=Unemployment, y=Residuals))+
  geom_point() + xlab("실업률")+ylab("잔차")

gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)


cbind(Fitted = fitted(fit.consMR),
      Residuals = residuals(fit.consMR)
      ) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Fitted, y=Residuals))+geom_point()+
  xlab("적합값")+ylab("잔차")

aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)

checkresiduals(fit)

beer2 <- window(ausbeer, start=1992)
autoplot(beer2)+xlab("연도")+ylab("백만 리터")

fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)

autoplot(beer2, series = "데이터")+
  autolayer(fitted(fit.beer),series = "적합값")+
  xlab("연도")+ylab("백만 리터")+
  guides(colour=guide_legend(title=" "))+
  ggtitle("분기별 맥주 생산량")

cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y=Fitted,
             colour = as.factor(cycle(beer2))
             )
         )+
  geom_point()+
  ylab("적합값")+xlab("실제 값")+
  ggtitle("분기별 맥주 생산량")+
  scale_colour_brewer(palette="Dark2",name="분기")+
  geom_abline(intercept = 0, slope=1)

fourier.beer <- tslm(beer2 ~ trend + fourier(beer2,K=2))
summary(fourier.beer)

CV(fit.consMR)


beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast)+
  ggtitle("회귀를 이용한 맥주 생산량 예측값")+
  xlab("연도")+ylab("백만 리터")

fit.consBest <- tslm(Consumption ~ Income + Savings + Unemployment,
                     data = uschange)
h <- 4
newdata <- data.frame(
  Income= c(1,1,1,1),
  Savings = c(0.5,0.5,0.5,0.5),
  Unemployment = c(0,0,0,0)
)

fcast.up <- forecast(fit.consBest, newdata = newdata)

newdata <- data.frame(
  Income = rep(-1,h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h)
)
fcast.down <- forecast(fit.consBest, newdata = newdata)
autoplot(uschange[,1])+
  ylab("미국 소비 지출%변화")+
  autolayer(fcast.up, PI = T, series="증가")+
  autolayer(fcast.down, PI=T, series = "감소")+
  guides(colour= guide_legend(title="시나리오"))+
  xlab("연도")

fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
                      newdata = data.frame(
                        Income = rep(mean(uschange[,"Income"]),h)
                      )
                      )
fcast.up <- forecast(fit.cons,
                     newdata = data.frame(Income = rep(5,h))
  
)

autoplot(uschange[,"Consumption"])+
  ylab("미국 소비%변화")+
  autolayer(fcast.ave, series = "평균적인 증가",
            PI=T)+
  autolayer(fcast.up, series = "극단적인 증가",
            PI=T)+
  guides(colour = guide_legend(title = "시나리오"))+
  xlab("연도")

h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)

fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h=h)

t <-  time(marathon)
t.break1 <- 1940
t.break2 <- 1980

tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <-ts(pmax(0, t -t.break2), start = 1897)

fit.pw <- tslm(marathon ~ t + tb1 + tb2)

t.new <-t[length(t)]+seq(h)
tb1.new <- tb1[length(tb1)]+seq(h)
tb2.new <-  tb2[length(tb2)]+seq(h)

newdata <-cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>% 
  as.data.frame()

fcasts.pw <-forecast(fit.pw, newdata = newdata)

fit.spline <-  tslm(marathon ~ t + I(t^2) + I(t^3)+
                      I(tb1^3)+I(tb2^3)
                    )
fcasts.spl <- forecast(fit.spline, newdata = newdata)

autoplot(marathon)+
  autolayer(fitted(fit.lin), series = "선형")+
  autolayer(fitted(fit.exp), series = "지수")+
  autolayer(fitted(fit.pw), series = "조각별")+
  autolayer(fitted(fit.spline), series = "3차 스플라인")+
  autolayer(fcasts.pw, series="조각별")+
  autolayer(fcasts.lin, series="선형", PI=F)+
  autolayer(fcasts.exp, series="지수",PI=F)+
  autolayer(fcasts.spl, series="3차 스플라인", PI=F)+
  xlab("연도")+ylab("우승 기록(분)")+
  ggtitle("보스턴 마라톤")+
  guides(colour=guide_legend(title=" "))


marathon %>% 
  splinef(lambda=0) %>% 
  autoplot()+
  xlab("연도")

marathon %>% 
  splinef(lambda = 0) %>% 
  checkresiduals()
