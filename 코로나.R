## Clear memory
rm(list=ls())


# 코로나19감염증으로 인한 시.도별 신규확진자, 신규사망자, 격리중인환자수, 격리해제환자수등에 대한 현황자료

library(XML)
library(stringr)

api <- "http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19SidoInfStateJson"
api_key <- "HUE2W9DXP2VX6%2BzrHha%2B4olzgVaDqZ%2FbNgFQuW2XaZpK%2BPax%2BVBP2ZBbVEq9kxfZBww4dwkbIaUOIl4MyEU6iw%3D%3D"

pageNo <- 1
numOfRows <- 10
startCreateDt <- 20200302
endCreateDt <- NULL
url <- str_c(api,
             "?serviceKey=", api_key,
             "&pageNo=", pageNo,
             "&numOfRows=", numOfRows,
             "&startCreateDt=", startCreateDt,
             "&endCreateDt", endCreateDt,
             sep="")


url

# XML 파일 살펴보기
xmlFile <- xmlParse(url)
xmlRoot(xmlFile)
selected.nodes <- getNodeSet(xmlFile, "//items/item")

# XML 문서를 데이터 프레임으로 변환
df <- xmlToDataFrame(selected.nodes)
df



View(df)



names(df) <- c("등록일시분초", "사망자수", "확진자수", "시도명(한글)", "시도명(중국어)", "시도명(영어)", "전일대비증감수", "격리해제수", "격리진행자수", "지역감염", "외부유입", "10만명당발생률", "고유번호", "기준일시", "수정일시분초")


# 대상 날짜수 지정
n.date = nrow(df) %/% 19
n.date


# 검역소의 확진자수 추이
lazzaretto = NULL
n.lazaretto = 1

for (i in 1:n.date) {
  
  n.serial <- 19 * (i - 1) + n.lazaretto
  
  print(paste("date ", df[n.serial,14]," being processed"))
  
  tab <- df[n.serial,]
  
  lazzaretto <- rbind(lazzaretto,tab)
  
  Sys.sleep(1)
  
}

# 서울의 확진자수 추이

seoul = NULL
n.seoul = 18

for (i in 1:n.date) {
  
  n.serial <- 19 * (i - 1) + n.seoul
  
  print(paste("date ", df[n.serial,14]," being processed"))
  
  tab <- df[n.serial,]
  
  seoul <- rbind(seoul,tab)
  
  Sys.sleep(1)
  
}


# 대한민국의 전체 확진자수 추이

total = NULL
n.total = 19

for (i in 1:n.date) {
  
  n.serial <- 19 * (i - 1) + n.total
  
  print(paste("date ", df[n.serial,14]," being processed"))
  
  tab <- df[n.serial,]
  
  total <- rbind(total,tab)
  
  Sys.sleep(1)
  
}



library(tidyverse)
library(ggplot2)

ggplot(data = seoul) + 
  geom_point(mapping = aes(x = 기준일시, y = 전일대비증감수))


increase.seoul = as.numeric(seoul[,7])



previous = NULL

for (i in 2:nrow(df)) {
  
  gap <- df[i-1,13] %>% as.numeric() - df[i,13] %>% as.numeric()
  
  if(gap!=1){
    print(paste("gap exist in", df[i,13]))
  }
  
  print(paste("gap ", gap, "in ", i))
  
  Sys.sleep(1)
  
}

ggplot(data=seoul, aes(y="확진자수")) 


plot(seoul$확진자수, type='l')
