library(stringr)

#install.packages("htmltab")
library(htmltab)

url <- "https://finance.naver.com/item/sise_day.nhn?code=086980&page=1"



i <- 1
final_data <-  NULL
for(i in 1:10){
  url <- paste0("https://finance.naver.com/item/sise_day.nhn?code=086980&page=",i)
  b<-htmltab(url,encoding="UTF-8")
  b
  final_data <- rbind(final_data,b)
  
}

colnames(final_data) <- c("날짜","종가","전일비","시가","고가","저가","거래량")
head(final_data)

for(j in 2:ncol(final_data)){
  final_data[,j] <- as.numeric(gsub(",","",final_data[,j]))
  cat("\n",j)
}
final_data

write.csv(final_data,"쇼박스.csv",row.names=F)

code <- c("086980","091990","007070","053580")
k <- 1

for(k in 1:length(code)){

  i <- 1
  final_data <-  NULL
  for(i in 1:10){
    url <- paste0("https://finance.naver.com/item/sise_day.nhn?code=",code[k],"&page=",i)
    b<-htmltab(url,encoding="UTF-8")
    b
    final_data <- rbind(final_data,b)
    
  }
  
  colnames(final_data) <- c("날짜","종가","전일비","시가","고가","저가","거래량")
  head(final_data)
  
  for(j in 2:ncol(final_data)){
    final_data[,j] <- as.numeric(gsub(",","",final_data[,j]))
    cat("\n",j)
  }
  
  dir.create(paste0("C:\\Users\\Max\\Documents\\03.PROJ_Matrix\\04.R_study\\R_coco\\",code[k]))
 write.csv(final_data,paste0("C:\\Users\\Max\\Documents\\03.PROJ_Matrix\\04.R_study\\R_coco\\",code[k],"\\",code[k],".csv"),row.names = F) 
  
}

