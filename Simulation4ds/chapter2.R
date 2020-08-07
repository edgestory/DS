ls()
getwd()

v.num <- c(1,3,5.9,7)
v.num

is.numeric(v.num)

v.num > 3

v1 <- c(1,2,3)
v2 <-  c(4,5)

v1+v2

v2 <- c(100,TRUE,"A",FALSE)
is.numeric(v2)

data(Cars93, package="MASS")
hp <- Cars93[1:10,"Horsepower"]
hp

str(Cars93)

hp[c(1,6)]

hp[-c(2:5, 7:10)]
hp<150
hp[hp<150]

class(Cars93)
class(Cars93$Cylinders)
levels(Cars93$Cylinders)
summary(Cars93$Cylinders)

names(Cars93)
model <- lm(Price ~ Cylinders + Type + EngineSize + Origin, data = Cars93)
class(model)
model$coefficients
names(model)

w <-  Cars93$Cylinders %in% c("3","4") & Cars93$Horsepower < 80
str(Cars93[w,])

dim(Cars93)

#install.packages("vcd")
library(vcd)
data("PreSex")
PreSex

PreSex[,,1,2]

PreSex[,,"Yes","Men"]

sum(is.na(Cars93))

#install.packages("VIM")
require("VIM")

matrixplot(Cars93, sortby= "Weight", cex.axis=0.6)

#install.packages("robCompositions")
m <- robCompositions::missPatterns(Cars93)
m


length(methods(summary))
class(Cars93$Cylinders)
summary(Cars93$Cylinders)

summary(as.character(Cars93$Cylinders))

func <- function(x){
  return(sum(is.na(x)))
}

apply(Cars93,2,func) -> na
na[na>0]

p <- ncol(Cars93)
na_for <- numeric(p)
for(i in 1:p){
  na_for[i]<- func(Cars93[,i])
}

identical(as.numeric(na),na_for)

colnames(Cars93)[na_for>0]

m <- robCompositions::missPatterns(Cars93)
class(m)
m

m

lapply(m, length)

s <- sapply(m, length)
class(s)
s

args(aggregate)

methods(aggregate)

args(aggregate.data.frame)

aggregate(Cars93[,c("Horsepower","Weight")],by=list(Cars93$Cylinders), median)

library(dplyr)

class(Cars93)
Cars93 <- tbl_df(Cars93)

class(Cars93)
slice(Cars93,1)

slice(Cars93,c(1,4,10,15,n()))
filter(Cars93, Manufacturer == "Audi"&Min.Price > 25)

Cars93 <- arrange(Cars93, Price)
Cars93

head(select(Cars93, Manufacturer, Price),3)

head(select(Cars93, Manufacturer:Price),3)

select(Cars93, -Min.Price, - Max.Price)
select(Cars93, starts_with("Man"))
select(Cars93, contains("Price"))

select(Cars93, myPrize = Price, Min.Price)

Cars93_1 <- select(Cars93, Manufacturer, EngineSize)
dim(Cars93_1)

Cars93_1 <- distinct(Cars93_1)
dim(Cars93_1)

dim(Cars93)
dim(distinct(Cars93, Manufacturer))
dim(distinct(Cars93, Manufacturer, EngineSize))

m <- mutate(Cars93, is_ford = Manufacturer == "Ford")
m[1:3, c(1,28)]

d<-transmute(Cars93, is_ford = Manufacturer == "Ford", Manufacturer)
d

head(transmute(Cars93, Manufacturer, is_ford= Manufacturer == "Ford", num_ford = ifelse(is_ford , -1 ,1)),3 )

by_type <- group_by(Cars93, Type)
summarize(by_type,
          count = n(), min_es = min(EngineSize),
          max_es = max(EngineSize))

Cars93 %>%
  group_by(Type) %>% 
  summarize(count= n(),
            min_es = min(EngineSize),
            max_es = max(EngineSize))

by_type <- group_by(Cars93, Type)
slice(by_type, 1:2)

Cars93 %>% 
  group_by(Type) %>% 
  slice(1:2)

#랭킹/순서: row_nmber(), min_rank(), percent_rank()
#시간 조정: lag(), lead()
#누적 함수: cumsum(), cummin(), cummax(),cummean()

Cars93 %>% 
  group_by(Type) %>% 
  arrange(Type) %>% 
  select(Manufacturer:Price) %>% 
  mutate(cmean = cummean(Price), csum = cumsum(Price))

library(data.table)

Cars93 <- data.table(Cars93)
Cars93

tables()
Cars93$tmp1 <- Cars93[, j=Manufacturer == "Ford"]
Cars93[,tmp2 := rnorm(nrow(Cars93))]
Cars93[,tmp1 := NULL]
Cars93$tmp1

Cars93$tmp2 <- NULL
Cars93$tmp2

Cars93[i=2]
Cars93[i=c(1,5)]

Cars93[i=-c(1:5)]

Cars93[j=3]
Cars93[j="Price"]
Cars93[j=Price]

Cars93[1:3, .(Price, Horsepower, Diff.Price=Max.Price - Min.Price, Mean.Price = mean(Price))]

setkey(Cars93, Type)
key(Cars93)

setkey(Cars93, Type)
Cars93["Van"]

setkey(Cars93, Type, DriveTrain, Origin)
Cars93[.("Van","4WD","non-USA")]

#install.packages("microbenchmark")
N <- 1000000
dat <- data.table(
  x = sample(LETTERS[1:20], N, replace=TRUE),
  y = sample(letters[1:5], N, replace=TRUE)
)
head(dat, 3)

setkey(dat, x,y)

library(microbenchmark)
microbenchmark(
  data.table = dat[list(c("B","D"),c("b","d"))],
 # dplyr = dat %>% slice( x %in% c("B","D") & y%in% c("b","d")   ),
  baseR = dat[x %in% c("B","D")&y%in%c("b","d")]
)


Cars93[, .(mean = mean(Price), IQR = IQR(Price), median = median(Price)), by=Type ]

#고성능 컴퓨팅
data(Cars93, package="MASS")
Cars93

set.seed(123)

system.time(lm(Price ~ Horsepower+Weight + Type + Origin, Cars93))

library("robustbase")

system.time(lmrob(Price ~ Horsepower+Weight+Type+Origin, Cars93))

ptm <- proc.time()
ptm

lmrob(Price ~ Horsepower+Weight+Type+Origin, Cars93)

proc.time() - ptm

s1 <- system.time(replicate(100,lm(Price~ Horsepower+Weight+Type+Origin, Cars93)))[3]
s2 <- system.time(replicate(100, lmrob(Price~ Horsepower+Weight+Type+Origin, Cars93)))[3]

(s2-s1)/s1

Rprof("Prestige.lm.out")
invisible(replicate(100,lm(Price~ Horsepower+Weight+Type+Origin, Cars93)))
Rprof(NULL)
summaryRprof("Prestige.lm.out")$by.self

#install.packages("profr")
require(profr)
parse_rprof("Prestige.lm.out")

#install.packages("Hmisc")

library(microbenchmark)
library(plyr)
library(dplyr)
library(data.table)
library(Hmisc)

data(Cars93, package="MASS")

Cars93 %>% 
  group_by(Type, Origin) %>% 
  summarise(mean = mean(Horsepower))

meanFor <- function(x){
  sum <- 0
  for(i in 1:length(x)) sum <- sum+x[i]
  sum/length(x)
}

##그룹 수준의 통계값
myfun1 <- function(x, grl, gr2, num){
  x[,gr1] <- as.factor(x[,gr1])
  x[,gr2] <- as.factor(x[,gr2])
  l1 <- length(levels(x[,gr1]))
  l2 <- length(levels(x[,gr1]))
  gr <- numeric(l1*l2)
  c1 <- c2 <- character(l1*l2)
  ii <- jj <- 0
  for(i in levels(x[,gr1])){
    for(j in levels(x[,gr2])){
      ii <- ii + 1
      c1[ii] <- i
      c2[ii] <- j
      vec <- x[x[,gr2]== j & x[,gr1]==i, num]
      if(length(vec)>0)gr[ii] <- meanFor(vec)
    }
  }
  df <- data.frame(cbind(c1, c2))
  df <- cbind(df, gr)
  colnames(df) <- c(gr1, gr2, paste("mean(",num,")"))
}


##mean()을 이용한 그룹 수준의 통계값

myfun2 <- function(x, gr1, gr2, num){
  x[,gr1] <- as.factor(x[,gr1])
  x[,gr2] <- as.factor(x[,gr2])
  l1 <- length(levels(x[,gr1]))
  l2 <- length(levels(x[,gr1]))
  gr <- numeric(l1*l2)
  c1 <- c2 <- character(l1*l2)
  ii <- jj <- 0
  for(i in levels(x[,gr1])){
    for(j in levels(x[,gr2])){
      ii < ii + 1
      c1[ii] <- i
      c2[ii] <- j
      gr[ii] <- mean(x[x[,gr2]==j & x[,gr1] == i , num])
    }
  }
  df <- data.frame(cbind(c1,c2))
  df <- cbind(df,gr)
  colnames(df) <- c(gr1, gr2, paste("means(",num,")"))
  df
}

Cars93dt <- data.table(Cars93)

op <- microbenchmark(
  MYFUN1 = myfun1(x=Cars93, gr1="Type", gr2="Origin", num="Horsepower"),
  MYFUN2 = myfun2(x=Cars93, gr1="Type", gr2="Origin", num="Horsepower"),
  PLYR = ddply(Cars93, .(Type, Origin), summarise, output = mean(Horsepower)),
  AGGR = aggregate(Horsepower ~ Type + Origin, Cars93, mean),
  BY = by(Cars93$Horsepower, list(Cars93$Type, Cars93$Origin),mean),
  SUMMARIZE=summarize(Cars93$Horsepower,list(Cars93$Type, Cars93$Origin),mean),
  TAPPLY = tapply(Cars93$Horsepower, interaction(Cars93$Type, Cars93$Origin), mean),
  DPLYR = summarise(group_by(Cars93, Type, Origin), mean(Horsepower)),
  DATATABLE = Cars93dt[,aggGroup1.2 := mean(Horsepower), by = list(Type, Origin)],
  times=1000L
)
