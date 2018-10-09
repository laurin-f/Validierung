path<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/Martins_Messungen/"
dat<-read.csv(paste0(path,"for_Laurin.csv"))
head(dat)
unique(dat$MessstellenId)
unique(dat$ParameterId)
unique(dat$Bem)
unique(dat$Einheit)

library(lubridate)
dat$date<-parse_date_time(dat$Datum,"dbyHMS")
dat$h<-format(dat$date,"%Y%m%d%H")
dat$d<-format(dat$date,"%Y%m%d")

dat$p<-dat$Messwert
dat$p[dat$Bem!="Niederschlag"]<-NA
p<-dat$Messwert[dat$Bem=="Niederschlag"]

dat$v5<-dat$Messwert
dat$v5[dat$Bem!="Vaisala 5cm" | dat$Einheit!= "ppm"]<-NA
v5<-dat$Messwert[dat$Bem=="Vaisala 5cm" & dat$Einheit== "ppm"]

dat$v25<-dat$Messwert
dat$v25[dat$Bem!="Vaisala 25cm" | dat$Einheit!= "ppm"]<-NA
v25<-dat$Messwert[dat$Bem=="Vaisala 25cm" & dat$Einheit== "ppm"]

plot(v5,type="l")
points(dat$v25,col=2)

sum.na<-function(x){sum(x, na.rm = T)}

p_hourly<-tapply(dat$p,dat$h,sum.na)
p_daily<-tapply(dat$p, dat$d, sum.na)

hour<-parse_date_time(unique(dat$h),"YmdH")
day<-parse_date_time(unique(dat$d),"Ymd")

plot(hour,p_hourly,type="p",col=2)
plot(day,p_daily,type="p")
plot(sort(p_hourly[p_hourly!=0]))
plot(sort(p_daily[p_daily!=0]))
hist(p_daily[p_daily>5],20)
hist(p_hourly[p_hourly>3],20)
plot(density(p_hourly[p_hourly>5]))
points(dat$p~dat$date,type="h")
p<-dat$Messwert[dat$Bem=="Niederschlag"]

plot(p)



