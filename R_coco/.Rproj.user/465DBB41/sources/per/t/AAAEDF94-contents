library(RJSONIO)
library(stringr)

i <-1
j <-1
final_data <- NULL

for(j in 1:5){
for(i in 1:2){
  
date <- Sys.Date() -j
date2 <-gsub("-","",date)
url <- paste0("https://sports.news.naver.com/kbaseball/news/list.nhn?isphoto=N&date=",date2,"&page=",i)

b <- readLines(url, encoding = "UTF-8")
fromJSON(b) -> b2

a1<-sapply(b2$list,function(x){x$oid})
a2<-sapply(b2$list,function(x){x$aid})
a3<-sapply(b2$list,function(x){x$title})

final_data<- rbind(final_data, cbind(a1,a2,a3))

cat("\n",date2,"-",j,i)

}
}

final_data
write.csv(final_data,"baseball_news.csv",row.names=F)


con_url <- paste0("https://sports.news.naver.com/news.nhn?oid=",final_data[,1],"&aid=",final_data[,2])
con_url

k <- 1
con <- c()

for(k in 1:length(con_url)){
  b<-readLines(con_url[k],encoding = "UTF-8")
  b2<-b[which(str_detect(b,"id=\"newsEndContents\">")): which(str_detect(b,"news_end_btn"))]
  b3<-paste(b2,collapse = " ")
  b3 <- gsub("<.*?>","",b3)
  b3 <- gsub("\t|&gt;|&#160|&lt;","",b3)
  con <- c(con,b3)
  cat("\n",k)
}

cbind(final_data,con) -> baseball_data
colnames(baseball_data) <- c("oid","aid","head","cont")

write.csv(baseball_data, "baseball.csv",row.names = F)





#####################################
##네이트 정치 뉴스 크롤링

library(stringr)

final_url <- c()
final_tit <- c()

j<-0

for(j in 0:5){
ddate <- Sys.Date()-j
ddate2 <- gsub("-","",ddate)

for(i in 1: 5){
  
  url <- paste0("https://news.nate.com/recent?mid=n0100&type=c&date=", ddate2 ,"&page=",i)
  b <- readLines(url, encoding="euc-kr")
  
  nurl <- paste0("http:/", str_sub(b[str_detect(b,"class=\"lt1\">")],14,end=-16))
  
  b2 <- b[str_detect(b,"<strong class=\"tit\">")]
  
  tit<-str_sub(str_extract(b2, "(?<=tit).*(?=</strong>)"),3,end=-2)
  
  final_url <- c(final_url, nurl)
  final_tit <- c(final_tit, tit)
  
  cat("\n",i,"-",j)
}

}

k<-1
final_con <- c()

for(k in 1:length(final_url)){
  b <- readLines(final_url[k], encoding = "euc-kr")
  
  aindex <- which(str_detect(b, "<div id=\"realArtcContents\">"))
  bindex <- which(str_detect(b, "<script language=\"javascript\" type=\"application/javascript\">" ) )
  b2 <- paste( b[aindex:bindex], collapse = " ")
  final_con[k] <- b2
  cat("\n",k)
}

final_url
