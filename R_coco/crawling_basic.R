library(stringr)

i <- 0
final_data <- NULL
for(i in 0:9){
  
  url <- paste0("https://www.clien.net/service/board/park?&od=T31&po=",i)
  
  b <- readLines(url, encoding="UTF-8") #EUC-KR
  length(b)
  
  b2 <-  b[str_detect(b, "subject_fixed")]
  title<-str_extract(b2, ( "(?<=\">).*(?=</span>)"  ) )
  
  b3<-b[str_detect(b, "<span class=\"hit\">")]
  hit <-  str_extract(b3,( "(?<=\">).*(?=</span>)"  ) )[c(-1,-2)]
  
  # b4<-str_split(b3,"hit\">")
  # ff<-function(x){
  #   x[2]
  # }
  # str_sub(sapply(b4,ff),end = -8) [c(-1,-2)]
  
  b5 <- b[which(str_detect(b, "subject_fixed"))-2]
  b6 <- str_sub(str_extract(b5, ( "(?<=href=\").*(?=data-role)"  )),end = -3 )
  url <- paste0("https://www.clien.net/",b6)
  
  data <- cbind(title, hit, url,page=c(i))
  final_data <- rbind(final_data, data)
  cat("\n",i)
  
  
}

dim(final_data)
head(final_data)


write.csv(final_data,"final_data.csv",row.names = F)
