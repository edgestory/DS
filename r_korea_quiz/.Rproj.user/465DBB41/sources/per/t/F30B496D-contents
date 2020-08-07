library(dplyr)
library(data.table)
library(ggplot2)

set.seed(1986)

data.frame(
  product = sample(rep(LETTERS[c(1:4)],c(30,40,30,20)))
) %>% 
  mutate(
    making.year = sample(rep(2017:2014,c(30,40,30,20) ) ),
    repair.year = making.year + sample(1:4)
  ) %>% 
  arrange(product, making.year, repair.year) -> rawdata


#상품별 집계

rawdata %>% 
  table() %>% 
  as.data.frame() %>%
  rename(count = Freq) %>% 
  arrange(product, repair.year, making.year) -> data


#제품-생산연도 그룹 별로 비중
data %>%
  group_by(product,making.year) %>% 
  mutate(
    sum_count = sum(count),
    rate = count/sum_count
  ) -> ratio.data

ratio.data


#비율을 100%형식으로 변경 하는 함수
format.ratio <- function(x){
  paste(floor(x*100),'%',sep='')
}

##ggplot2를 이용한 시각화
ratio.data %>% 
  ggplot(aes(x=making.year, y=repair.year, fill=rate))+
  geom_tile() +
  geom_text(
    aes(
      label=case_when(
        rate==0 ~ '',
        TRUE ~ format.ratio(rate)
      )
    )
  )+
  scale_fill_continuous(
    high = 'red',
    low='white',
    label=format.ratio
  )+
  facet_grid(.~paste(product,"제품"))+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(angle=0)
  )+
  labs(title='제품별 \n A/S비율', y='A/S연도', x='생산연도', fill = "A/S비율 \n")



#정답지

#상품별 집계(table 함수)후 data.frame화 -> 컬럼명 바꿔주고 정렬 
data <-
  rawdata %>%
  table(.) %>%
  as.data.frame(.) %>%
  setnames(c(names(.[,1:(ncol(.)-1)]),'count')) %>%
  arrange(product, repair.year, making.year)


# 비율 계산을 위한 total
total <-
  data %>%
  group_by(product, making.year) %>%
  summarise(total = sum(count)) %>%
  arrange(product, making.year)

ratio.data <-
  data %>%
  inner_join(total, by=c('product','making.year')) %>%
  mutate(ratio=count/total)

format.ratio <- function(x) paste(floor(x*100),'%',sep='')

# chart : legend title에 대한 vjust가 먹히지 않아서 줄바꿈 문자 '\n' 으로 해결
ratio.data %>%
  ggplot(aes(x=making.year, y=repair.year, fill=ratio)) +
  geom_tile() +
  geom_text(
    aes(label=
          case_when(
            ratio==0 ~ '', 
            TRUE ~ format.ratio(ratio)))) +
  scale_fill_continuous(
    high='red', 
    low='white', 
    label=format.ratio) +
  facet_grid(~ paste(product,'제품')) +
  theme_bw() +
  theme(
    panel.grid = element_blank(), 
    axis.title.y=element_text(angle=0)) +
  labs(title='제품별 A/S 비율', y='A/S 연도', x='생산연도', fill='A/S 비율\n')

