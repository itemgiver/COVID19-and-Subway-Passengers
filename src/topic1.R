library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
subway2019 <- read_excel("../Desktop/R homework/2019_subway.xlsx")
subway2019 <- subway2019[,colSums(is.na(subway2019))<nrow(subway2019)]
subway2020 <- read.csv("../Desktop/R homework/2020_subway.csv", header = T)
subway2021 <- read.csv("../Desktop/R homework/2021_subway.csv", header = T)
column_name <- c("date","호선","역번호","station","구분","before6","time7",
                   "time8","time9","time10","time11","time12","time13",
                   "time14","time15","time16","time17","time18","time19",
                   "time20","time21","time22","time23","after24")
names(subway2019) <- column_name
names(subway2020) <- column_name
names(subway2021) <- column_name

# 주말 포함
# int_date <- c(0,1,2,3,4,5,6)
# 평일만
int_date <- c(0,1,2,3,4,5,6)

data2019 <- subway2019 %>%
  group_by(station) %>%
  filter(is.element(as.double(difftime(ymd(date), ymd("2019-01-01"))) %% 7, int_date) >= 0) %>%
  summarise(sum2019 = sum(before6) + sum(time7) + sum(time8) + sum(time9) + sum(time10) + 
              sum(time11) + sum(time12) + sum(time13) + sum(time14) + sum(time15) + 
              sum(time16) + sum(time17) + sum(time18) + sum(time19) + sum(time20) + sum(time21) + sum(time22)
            + sum(time23) + sum(after24))
# sum(time17) + sum(time18) + sum(time19) 바꿀 수 있음

data2020 <- subway2020 %>%
  group_by(station) %>%
  filter(is.element(as.double(difftime(ymd(date), ymd("2019-01-01"))) %% 7, int_date) >= 0) %>%
  summarise(sum2020 = sum(before6) + sum(time7) + sum(time8) + sum(time9) + sum(time10) + 
              sum(time11) + sum(time12) + sum(time13) + sum(time14) + sum(time15) + 
              sum(time16) + sum(time17) + sum(time18) + sum(time19) + sum(time20) + sum(time21) + sum(time22)
            + sum(time23) + sum(after24))
# sum(time17) + sum(time18) + sum(time19) 바꿀 수 있음

data2020 <- data2020 %>%
  filter(is.element(station,data2019$station))
data2019 <- data2019 %>%
  filter(is.element(station,data2020$station))

data_change <- merge(data2019, data2020, by='station')
data_change <- data_change %>%
  group_by(station) %>%
  summarise(change = sum2020 / sum2019)

tmp <- data_change[order(data_change$change, decreasing = TRUE),]

boxplot(main='Growth of Subway Users in 2020',
        xlab='Growth of Subway Users', ylab = 'By Station',
        col='orange', border='brown', tmp$change,horizontal=TRUE)
axis(side=1, at=seq(0,2,by=0.1))

plot(tmp$change, main='Growth of Subway Users by Station',
     xlab='Station', ylab='Growth of Subway Users')

write.csv(tmp, file="../Desktop/R homework/data.csv", row.names=TRUE)