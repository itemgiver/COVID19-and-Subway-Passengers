data1<-read.csv("D://project/time_2019_subway.csv",header=T)
data2<-read.csv("D://project/time_2020_subway.csv",header=T)

sub_2019<-data1[,-c(1,2,3,5,26)]
sub_2020<-data2[,-c(1,2,3,5)]


sum_2019<-c(0,
            sum(sub_2019[,2]),
            sum(sub_2019[,3]),
            sum(sub_2019[,4]),
            sum(sub_2019[,5]),
            sum(sub_2019[,6]),
            sum(sub_2019[,7]),
            sum(sub_2019[,8]),
            sum(sub_2019[,9]),
            sum(sub_2019[,10]),
            sum(sub_2019[,11]),
            sum(sub_2019[,12]),
            sum(sub_2019[,13]),
            sum(sub_2019[,14]),
            sum(sub_2019[,15]),
            sum(sub_2019[,16]),
            sum(sub_2019[,17]),
            sum(sub_2019[,18]),
            sum(sub_2019[,19]),
            sum(sub_2019[,20]),
            sum(sub_2019[,21]))






sum_2020<-c(0,
            sum(sub_2020[,2]),
            sum(sub_2020[,3]),
            sum(sub_2020[,4]),
            sum(sub_2020[,5]),
            sum(sub_2020[,6]),
            sum(sub_2020[,7]),
            sum(sub_2020[,8]),
            sum(sub_2020[,9]),
            sum(sub_2020[,10]),
            sum(sub_2020[,11]),
            sum(sub_2020[,12]),
            sum(sub_2020[,13]),
            sum(sub_2020[,14]),
            sum(sub_2020[,15]),
            sum(sub_2020[,16]),
            sum(sub_2020[,17]),
            sum(sub_2020[,18]),
            sum(sub_2020[,19]),
            sum(sub_2020[,20]),
            sum(sub_2020[,21]))


rank_2019<-sub_2019[,order(sum_2019,decreasing = T)]
rank_2020<-sub_2020[,order(sum_2020,decreasing = T)]
