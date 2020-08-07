
url <- "http://www.angelinus.com/Shop/Shop_Ajax.asp?page=1"

#install.packages("XML")

library(XML)
b<-readHTMLTable(url,encoding="UTF-8")
b

final_data <- NULL

for(i in 1:10){
  url <-"http://www.angelinus.com/Shop/Shop_Ajax.asp?page=1"
  b<-readHTMLTable(url, encoding="UTF-8")
  
  class(b)
  b2 <- b$`조건에 맞춰 검색된 매장의 번호, 매장명, 주소, 매장 제공 서비스, 전화번호 리스트`
  cat("\n",i)
  final_data <- rbind(final_data, b2)
}
final_data <- final_data[-3]
dim(final_data)

colnames(final_data) <- c("번호","매장명","전화번호")

write.csv(final_data, "엔제리너스.csv",row.names=F)
