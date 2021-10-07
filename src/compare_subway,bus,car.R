bus_2020_csv<-read.csv(file="D:/project/2020년 버스.csv",header=TRUE)
bus_2020_df<-data.frame(bus_2020_csv[,-c(2,3,4,5,6,55)])

bus_2019_csv<-read.csv(file="D:/project/2019년 버스.csv",header=TRUE)
bus_2019_df<-data.frame(bus_2019_csv[,-c(2,3,4,5,6,55)])

#bus_202101<-read.csv(file="D:/project/2021년_버스(01월).csv",header=TRUE)
#bus_202102<-read.csv(file="D:/project/2021년_버스(02월).csv",header=TRUE)
#bus_202103<-read.csv(file="D:/project/2021년_버스(03월).csv",header=TRUE)
#bus_202104<-read.csv(file="D:/project/2021년_버스(04월).csv",header=TRUE)

#bus_202101_df<-data.frame(bus_202101[,-c(2,3,4,5,6,55)])
#bus_202102_df<-data.frame(bus_202102[,-c(2,3,4,5,6,55)])
#bus_202103_df<-data.frame(bus_202103[,-c(2,3,4,5,6,55)])
#bus_202104_df<-data.frame(bus_202104[,-c(2,3,4,5,6,55)])

#bus_2021_df<-rbind(bus_202101_df,bus_202102_df,bus_202103_df,bus_202104_df)

month<-c("19년 1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월","20년 1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")

p_201901<-bus_2020_df[bus_2019_df$사용년월==201901,]
p_201902<-bus_2020_df[bus_2019_df$사용년월==201902,]
p_201903<-bus_2020_df[bus_2019_df$사용년월==201903,]
p_201904<-bus_2020_df[bus_2019_df$사용년월==201904,]
p_201905<-bus_2020_df[bus_2019_df$사용년월==201905,]
p_201906<-bus_2020_df[bus_2019_df$사용년월==201906,]
p_201907<-bus_2020_df[bus_2019_df$사용년월==201907,]
p_201908<-bus_2020_df[bus_2019_df$사용년월==201908,]
p_201909<-bus_2020_df[bus_2019_df$사용년월==201909,]
p_201910<-bus_2020_df[bus_2019_df$사용년월==201910,]
p_201911<-bus_2020_df[bus_2019_df$사용년월==201911,]
p_201912<-bus_2020_df[bus_2019_df$사용년월==201912,]

p_202001<-bus_2020_df[bus_2020_df$사용년월==202001,]
p_202002<-bus_2020_df[bus_2020_df$사용년월==202002,]
p_202003<-bus_2020_df[bus_2020_df$사용년월==202003,]
p_202004<-bus_2020_df[bus_2020_df$사용년월==202004,]
p_202005<-bus_2020_df[bus_2020_df$사용년월==202005,]
p_202006<-bus_2020_df[bus_2020_df$사용년월==202006,]
p_202007<-bus_2020_df[bus_2020_df$사용년월==202007,]
p_202008<-bus_2020_df[bus_2020_df$사용년월==202008,]
p_202009<-bus_2020_df[bus_2020_df$사용년월==202009,]
p_202010<-bus_2020_df[bus_2020_df$사용년월==202010,]
p_202011<-bus_2020_df[bus_2020_df$사용년월==202011,]
p_202012<-bus_2020_df[bus_2020_df$사용년월==202012,]

#p_202101<-bus_2021_df[bus_2021_df$사용년월==202101,]
#p_202102<-bus_2021_df[bus_2021_df$사용년월==202102,]
#p_202103<-bus_2021_df[bus_2021_df$사용년월==202103,]
#p_202104<-bus_2021_df[bus_2021_df$사용년월==202104,]

passenger<-c(sum(p_201901[,-1])%/%(31*10000),
             sum(p_201902[,-1])%/%(28*10000),
             sum(p_201903[,-1])%/%(31*10000),
             sum(p_201904[,-1])%/%(30*10000),
             sum(p_201905[,-1])%/%(31*10000),
             sum(p_201906[,-1])%/%(30*10000),
             sum(p_201907[,-1])%/%(31*10000),
             sum(p_201908[,-1])%/%(31*10000),
             sum(p_201909[,-1])%/%(30*10000),
             sum(p_201910[,-1])%/%(31*10000),
             sum(p_201911[,-1])%/%(30*10000),
             sum(p_201912[,-1])%/%(31*10000),
  
             sum(p_202001[,-1])%/%(31*10000),
             sum(p_202002[,-1])%/%(29*10000),
             sum(p_202003[,-1])%/%(31*10000),
             sum(p_202004[,-1])%/%(30*10000),
             sum(p_202005[,-1])%/%(31*10000),
             sum(p_202006[,-1])%/%(30*10000),
             sum(p_202007[,-1])%/%(31*10000),
             sum(p_202008[,-1])%/%(31*10000),
             sum(p_202009[,-1])%/%(30*10000),
             sum(p_202010[,-1])%/%(31*10000),
             sum(p_202011[,-1])%/%(30*10000),
             sum(p_202012[,-1])%/%(31*10000))
             
             #sum(p_202101[,-1])%/%(31*10000),
             #sum(p_202102[,-1])%/%(29*10000),
             #sum(p_202103[,-1])%/%(31*10000),
             #sum(p_202104[,-1])%/%(30*10000))

bus<-data.frame(month,passenger)









subway_csv<-read.csv(file="D:/project/지하철.csv",header=TRUE)
sub_df<-data.frame(subway_csv[,-c(2,3,52)])

p_s_201901<-sub_df[sub_df$사용월==201901,]
p_s_201902<-sub_df[sub_df$사용월==201902,]
p_s_201903<-sub_df[sub_df$사용월==201903,]
p_s_201904<-sub_df[sub_df$사용월==201904,]
p_s_201905<-sub_df[sub_df$사용월==201905,]
p_s_201906<-sub_df[sub_df$사용월==201906,]
p_s_201907<-sub_df[sub_df$사용월==201907,]
p_s_201908<-sub_df[sub_df$사용월==201908,]
p_s_201909<-sub_df[sub_df$사용월==201909,]
p_s_201910<-sub_df[sub_df$사용월==201910,]
p_s_201911<-sub_df[sub_df$사용월==201911,]
p_s_201912<-sub_df[sub_df$사용월==201912,]

p_s_202001<-sub_df[sub_df$사용월==202001,]
p_s_202002<-sub_df[sub_df$사용월==202002,]
p_s_202003<-sub_df[sub_df$사용월==202003,]
p_s_202004<-sub_df[sub_df$사용월==202004,]
p_s_202005<-sub_df[sub_df$사용월==202005,]
p_s_202006<-sub_df[sub_df$사용월==202006,]
p_s_202007<-sub_df[sub_df$사용월==202007,]
p_s_202008<-sub_df[sub_df$사용월==202008,]
p_s_202009<-sub_df[sub_df$사용월==202009,]
p_s_202010<-sub_df[sub_df$사용월==202010,]
p_s_202011<-sub_df[sub_df$사용월==202011,]
p_s_202012<-sub_df[sub_df$사용월==202012,]

#p_s_202101<-sub_df[sub_df$사용월==202101,]
#p_s_202102<-sub_df[sub_df$사용월==202102,]
#p_s_202103<-sub_df[sub_df$사용월==202103,]
#p_s_202104<-sub_df[sub_df$사용월==202104,]


passenger_sub<-c(sum(p_s_201901[,-1])%/%(31*10000),
                 sum(p_s_201902[,-1])%/%(28*10000),
                 sum(p_s_201903[,-1])%/%(31*10000),
                 sum(p_s_201904[,-1])%/%(30*10000),
                 sum(p_s_201905[,-1])%/%(31*10000),
                 sum(p_s_201906[,-1])%/%(30*10000),
                 sum(p_s_201907[,-1])%/%(31*10000),
                 sum(p_s_201908[,-1])%/%(31*10000),
                 sum(p_s_201909[,-1])%/%(30*10000),
                 sum(p_s_201910[,-1])%/%(31*10000),
                 sum(p_s_201911[,-1])%/%(30*10000),
                 sum(p_s_201912[,-1])%/%(31*10000),
                 
                 sum(p_s_202001[,-1])%/%(31*10000),
                 sum(p_s_202002[,-1])%/%(29*10000),
                 sum(p_s_202003[,-1])%/%(31*10000),
                 sum(p_s_202004[,-1])%/%(30*10000),
                 sum(p_s_202005[,-1])%/%(31*10000),
                 sum(p_s_202006[,-1])%/%(30*10000),
                 sum(p_s_202007[,-1])%/%(31*10000),
                 sum(p_s_202008[,-1])%/%(31*10000),
                 sum(p_s_202009[,-1])%/%(30*10000),
                 sum(p_s_202010[,-1])%/%(31*10000),
                 sum(p_s_202011[,-1])%/%(30*10000),
                 sum(p_s_202012[,-1])%/%(31*10000))
             
                 #sum(p_s_202101[,-1])%/%(31*10000),
                 #sum(p_s_202102[,-1])%/%(29*10000),
                 #sum(p_s_202103[,-1])%/%(31*10000),
                 #sum(p_s_202104[,-1])%/%(30*10000))

subway<-data.frame(month,passenger_sub)








car_csv<-subway_csv<-read.csv(file="D:/project/교통량.csv",header=TRUE)
car_df<-data.frame(car_csv)

passenger_car<-((car_df[,2]+car_df[,3])*2)%/%10

car<-data.frame(month,passenger_car)








mydata<-data.frame(month,bus=bus$passenger,sub=subway$passenger_sub,car=car$passenger_car)




library(ggplot2)

#plot_bus<-ggplot(data=bus,mapping=aes(x=c(1:16),y=passenger))+geom_point()+geom_line()
#plot_sub<-ggplot(data=subway,mapping=aes(x=c(1:16),y=passenger_sub))+geom_point()+geom_line()

#plot_bus+labs(x="Month",y="Passenger") + scale_x_continuous(breaks=c(1:16),labels=month)+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))
#par(new=T)
#plot_sub+labs(x="Month",y="Passenger") + scale_x_continuous(breaks=c(1:16),labels=month)+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))



#plot_data<-ggplot(data=mydata,mapping=aes(x=c(1:16)))+geom_point()+geom_line()
#plot_data+labs(x="Month",y="Passenger") + scale_x_continuous(breaks=c(1:16),labels=month)+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))





plot(c(1:24),mydata$bus,xlab="Month",ylab="Passenger",axes=F,ylim=c(500,1800),type="o",lty=1,pch=19,main="Compare Bus, Subway, Car",col="blue",cex=1.5)
par(new=T)
plot(c(1:24),mydata$sub,xlab="Month",ylab="Passenger",axes=F,ylim=c(500,1800),type="o",lty=1,pch=19,col="red",cex=1.5)
par(new=T)
plot(c(1:24),mydata$car,xlab="Month",ylab="Passenger",axes=F,ylim=c(500,1800),type="o",lty=1,pch=19,col="green",cex=1.5)

axis(1,at=1:24,lab=month)
axis(2,at=seq(500,2000,by=100))





month2<-c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")



plot(c(1:12),mydata$sub[c(1:12)],xlab="Month",ylab="Passenger",axes=F,ylim=c(500,1800),type="o",lty=1,pch=19,main="Compare Subway",col="blue",cex=1.5)
par(new=T)
plot(c(1:12),mydata$sub[c(13:24)],xlab="Month",ylab="Passenger",axes=F,ylim=c(500,1800),type="o",lty=1,pch=19,col="red",cex=1.5)
axis(1,at=1:12,lab=month2)
axis(2,at=seq(500,1800,by=100))




plot(c(1:12),mydata$bus[c(1:12)],xlab="Month",ylab="Passenger",axes=F,ylim=c(500,1100),type="o",lty=1,pch=19,main="Compare Bus",col="blue",cex=1.5)
par(new=T)
plot(c(1:12),mydata$bus[c(13:24)],xlab="Month",ylab="Passenger",axes=F,ylim=c(500,1100),type="o",lty=1,pch=19,col="red",cex=1.5)
axis(1,at=1:12,lab=month2)
axis(2,at=seq(500,1100,by=100))




plot(c(1:12),mydata$car[c(1:12)],xlab="Month",ylab="Passenger",axes=F,ylim=c(1200,1800),type="o",lty=1,pch=19,main="Compare Car",col="blue",cex=1.5)
par(new=T)
plot(c(1:12),mydata$car[c(13:24)],xlab="Month",ylab="Passenger",axes=F,ylim=c(1200,1800),type="o",lty=1,pch=19,col="red",cex=1.5)
axis(1,at=1:12,lab=month2)
axis(2,at=seq(1200,1800,by=100))




