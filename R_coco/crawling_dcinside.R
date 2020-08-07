library(stringr)


i <- 0
dc_data <- NULL
for(i in 1:10){
  
  
  url <- paste0("https://gall.dcinside.com/board/lists/?id=superidea&page=",i)
  
  b <- readLines(url, encoding = "UTF-8")
  
  index <- which(str_detect(b,"gall_tit ub-word"))[-1]
  b2<-b[index + 1]
  
  title<-str_trim(str_extract(b2, ("(?<=</strong>).*(?=</a>)") ))
  title
  
  con_url <- paste0("https://gall.dcinside.com/",
                    str_sub(str_extract(b2, ("(?<=a href=).*(?=em class)") ),3,end=-4)
  )
  hit_index<-which(str_detect(b,"gall_count"))[-1]
  hit <- as.numeric(str_extract(b[hit_index], ("(?<=gall_count\">).*(?=</td>)") ))
  
  rec_index<-which(str_detect(b,"gall_recommend"))[-1]
  rec <- as.numeric(str_extract(b[rec_index], ("(?<=gall_recommend\">).*(?=</td>)") ))
  
  data<-cbind(title, con_url,hit,rec)
  dc_data <- rbind(dc_data,data)
  cat("\n",i)
}
dim(dc_data)

write.csv(dc_data,"dc_data.csv",row.names = F)

data<-read.csv("dc_data.csv")
dim(data)

url <- as.character(data$con_url)

i <- 1
contents <- c()
for(i in 1:length(url)){
 b <-  readLines( url[i], encoding = "UTF-8")
 
 Sys.sleep(runif(1)*2)
 
 b2<-b[which(str_detect(b,"gallview_contents")):which(str_detect(b,"<!--  본문 우측 광고 -->"))]
 b3<-paste(b2,collapse = " ")
 con<-str_trim(gsub("<.*?>|\t|&nbsp","",b3))
 contents <- c(contents, con)
 cat("\n",i)
}

data2<-cbind(data,contents)
data2
write.csv(data2,"dc.csv",row.names = F)
