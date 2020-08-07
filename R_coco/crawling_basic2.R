library(stringr)
getwd()

data <- read.csv("final_data.csv")
head(data)

url_list <- data[,3]

length(url_list)
content <- c()

for ( i in 1:length(url_list)){
  
## try_error
if(class(try(b<-readLines(as.character(url_list[i]), encoding = 'UTF-8'))) == "try-error"){
  b6 <- ""
  content <- c(content,b6)
 # next;
}else{
  
  
  b2<-b[which(str_detect(b,"post_content")):which(str_detect(b,"post_ccls"))]
  b3<-paste(b2, collapse = "")
  
  b4 <- gsub("<.*?>","",b3)
  b5 <- gsub("\t|&nbsp","",b4)
  b6 <- str_trim(b5)
  content <- c(content)
  cat("\n",i)
}
}

