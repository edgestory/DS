library(stringr)
juso <- c("서울시 강북구 번1동", "전라남도 순천시 석현동", "경기도 안산시 단원구")

aa <- str_split(juso," ")
seoul <- aa[[1]]
length(seoul)

aa[[1]][2]

names(aa) <- c("서울", "전남","경기")
aa

unlist(aa)
aa2 <- unlist(aa)
length(aa2)

##apply, sapply, lapply, tapply
sapply(aa, length)

first <- function(x){
  x[1]
}

sapply(aa,first)
lapply(aa,first)

index <- function(x, i){
  x[i]
}

index(aa[[1]],2)
sapply(aa,index,2)
sapply(aa,index,3)


dd <- c()
for(i in 1 :length(aa)){
  aa[[i]][1]
  dd<-c(dd,aa[[i]][1])
  cat("\n",i)
}
dd
