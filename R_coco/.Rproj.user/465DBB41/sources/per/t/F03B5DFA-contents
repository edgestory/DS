
## 네이버 지도 위도 경도 값 받아오기


serach_loc <- function(x){

keyword<-x
keyword <- iconv(keyword, from="CP949",to="UTF-8")
Encoding(keyword)
keyword2 <- URLencode(keyword)



url <- paste0("https://map.naver.com/v5/api/instantSearch?q=",keyword2,"&lang=ko&caller=pcweb&types=place,address&coords=37.55716613503612,126.93629264831544")

b <- readLines(url, encoding = "UTF-8")


#install.packages("RJSONIO")
library(RJSONIO)

b2 <- fromJSON(b)


c(b2$poi[[1]]$title,b2$poi[[1]]$x,b2$poi[[1]]$y)

}
serach_loc("사당역")






### 카페 리스트 크롤링

keyword <- "신촌카페"
keyword <- iconv(keyword, from="CP949",to="UTF-8")
Encoding(keyword)

keyword2 <- URLencode(keyword)

url <- paste0("https://m.map.naver.com/search2/searchMore.nhn?query=",keyword2,"&sm=clk&style=v5&page=1&displayCount=75&type=SITE_1" )

b <- readLines(url, encoding = "UTF-8")
b <- paste(b,collapse = " ")

library(RJSONIO)

b2<- fromJSON(b)
b2$result$site$list[[1]]$name
b2$result$site$list[[1]]$id

name <- sapply(b2$result$site$list,function(x){x$name} )
id <- sapply(b2$result$site$list,function(x){x$id} )
x <- sapply(b2$result$site$list,function(x){x$x} )
y <- sapply(b2$result$site$list,function(x){x$y} )
addr <- sapply(b2$result$site$list,function(x){x$address} )

data <- cbind(name, id, x, y, addr)
write.csv(data, paste0(keyword,".csv"),row.names = F)
