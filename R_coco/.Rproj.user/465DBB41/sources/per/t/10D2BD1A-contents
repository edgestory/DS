url <- "https://www.bobaedream.co.kr/list?code=best"

html_text<-readLines(url, encoding = "UTF-8")

library(stringr)


detect_text<-html_text[str_detect(html_text, "<a class=\"bsubject\"")]

extract_text <- str_extract(detect_text,("(?<=href=).*(?=date)"))

url_list <- paste0("https://www.bobaedream.co.kr",str_sub(extract_text,2),"date")

title_vec <-c()
con_vec <- c()
j <- 1
for(j in 1:length(url_list)){
  text <- readLines(url_list[j], encoding = "UTF-8")
  
  title_vec[j] <- text[str_detect(text,"<title>")]
  
  con_vec[j] <- text[which(str_detect(text,"<div class=\"bodyCont\""))+1]
  
  cat("\n",j)
}


title<-str_sub(str_extract(title_vec,("(?<=<title>).*(?=</title>)") ),end=-12)
title

con_vec <-gsub("<.*?>","",con_vec)
con_vec <-gsub("&nbsp;","",con_vec)
con_vec

head(con_vec)

data <- cbind(title, con_vec)
head(data)

colnames(data) <- c("title","contents")
write.csv(data,"studay1_comunity_site.csv")

getwd()




url <- "http://www.todayhumor.co.kr/board/list.php?table=bestofbest&page=1"

umer_html <- readLines(url, encoding = "UTF-8")

subject_html <- umer_html[str_detect(umer_html,"<td class=\"subject\">")]

title <- str_sub(str_extract(subject_html,("(?<=target).*(?=</a>)")),10)

mega1 <- str_split(subject_html,"a href")
mega2 <- sapply(mega1, function(x){x[2]})
sapply(str_split(mega1,"target"),function(x){x[1]} )

url_lint 
            