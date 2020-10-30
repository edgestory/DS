library(tidyverse)

iris
as_tibble(iris)

tibble(
  x=1:5,
  y=1,
  z=x^2+y
)

tb <- tibble(
  ':)' = "스마일",
  ' ' = "스페이스",
  '2000' = '숫자자'
)

tb

tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

###7.3.1 화면출력
nycflights13::flights %>%
  print(n=10, width = Inf)

##옵션으로 설정도 가능
nycflights13::flights
#option(tibble.width=Inf)

nycflights13::flights  %>% 
  View()


###7.3.2 서브셋하기
df <- tibble(
  x=runif(5),
  y= rnorm(5)
)

df

df$x

df[['x']]
df['x']

df[[1]]


df %>% .$x

df %>% .[['x']]

###7.4 이전 코드와 상호작용

