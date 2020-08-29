

url <- 'https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EC%8B%A0%EC%B4%8C%EB%A7%9B%EC%A7%91&sm=tab_pge&srchby=all&st=sim&where=post&start=1'

b <- readLines(url, encoding='UTF-8')

library(stringr)

b2 <- b[str_detect(b,'tab_depth')]

b3 <- str_split(b2, "sh_blog_title _sp_each_url _sp_each_title")

b4 <- sapply(str_split(b3[[1]], 'target=_blank'),function(x){x[1]})

b5 <- str_sub(b4[2:length(b4)],9,end=-3)
b5

str_sub(str_extract(b5,("(?<=com/).*(?=Redirect)")),end=-2)
