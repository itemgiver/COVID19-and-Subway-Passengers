library(dplyr)
library(ggplot2)
library(ggmap)
library(stringr)


data<-read.csv("D://project/지하철역위치.csv",header=T)
sub<-read.csv("D://project/period_data.csv",header=T)

sub1<-sub[,c(2,7)]
sub2<-sub[,c(2,8)]




sub_high<-sub1[sub1$period1>0.9,]
r<-nrow(sub_high)

sub_high_pos<-data.frame()
for(i in 1:r) {
  k<-str_split(sub_high[i,1],"\\(")[[1]][1]
  sub_high_pos<-rbind(sub_high_pos,data[data$전철역명==k,c(2,3,8,9)])
}
colnames(sub_high_pos)<-c("전철역명","호선","x좌표","y좌표")



sub_low<-sub1[sub1$period1<0.7,]
r<-nrow(sub_low)

sub_low_pos<-data.frame()
for(i in 1:r) {
  k<-str_split(sub_low[i,1],"\\(")[[1]][1]
  sub_low_pos<-rbind(sub_low_pos,data[data$전철역명==k,c(2,3,8,9)])
}
colnames(sub_low_pos)<-c("전철역명","호선","x좌표","y좌표")




sub_9080<-sub1[sub1$period1<0.9 & sub1$period1>0.8,]
r<-nrow(sub_9080)

sub_9080_pos<-data.frame()
for(i in 1:r) {
  k<-str_split(sub_9080[i,1],"\\(")[[1]][1]
  sub_9080_pos<-rbind(sub_9080_pos,data[data$전철역명==k,c(2,3,8,9)])
}
colnames(sub_9080_pos)<-c("전철역명","호선","x좌표","y좌표")




sub_8070<-sub1[sub1$period1<0.8 & sub1$period1>0.7,]
r<-nrow(sub_8070)

sub_8070_pos<-data.frame()
for(i in 1:r) {
  k<-str_split(sub_8070[i,1],"\\(")[[1]][1]
  sub_8070_pos<-rbind(sub_8070_pos,data[data$전철역명==k,c(2,3,8,9)])
}
colnames(sub_8070_pos)<-c("전철역명","호선","x좌표","y좌표")






register_google(key="AIzaSyDZz6Y2yM5QPSchfJivsyWEpCnUHGf1tUc")

center <- c((mean(sub_high_pos$y좌표)+mean(sub_low_pos$y좌표)+mean(sub_9080_pos$y좌표)+mean(sub_8070_pos$y좌표))/4,
            (mean(sub_high_pos$x좌표)+mean(sub_low_pos$x좌표)+mean(sub_9080_pos$x좌표)+mean(sub_8070_pos$x좌표))/4)
#center<-c(127,37.5)

seoul <- get_map(center, zoom=11, maptype="roadmap")
ggmap(seoul) +
  geom_point(data=sub_high_pos,aes(x=y좌표,y=x좌표), size=2.5,col="black",alpha=0.7) +
  geom_point(data=sub_low_pos, aes(x=y좌표,y=x좌표),size=2.5,col="red",alpha=0.7) +
  geom_point(data=sub_9080_pos, aes(x=y좌표,y=x좌표),size=2.5,col="blue",alpha=0.7) +
  geom_point(data=sub_8070_pos, aes(x=y좌표,y=x좌표),size=2.5,col="green",alpha=0.7)
# 90이상 : black, 90~80 : blue, 80~70 : green, 70이하 : red

  

