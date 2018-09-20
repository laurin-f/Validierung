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

sum.na<-function(x){sum(x, na.rm = T)}

p_hourly<-tapply(dat$p,dat$h,sum.na)
p_daily<-tapply(dat$p, dat$d, sum.na)

hour<-parse_date_time(unique(dat$h),"YmdH")
day<-parse_date_time(unique(dat$d),"Ymd")

plot(hour,p_hourly,type="h",col=2)
plot(day,p_daily,type="l")
points(dat$p~dat$date,type="h")
p<-dat$Messwert[dat$Bem=="Niederschlag"]

plot(p)

t<-60*24#zeit in minuten
b<-sqrt(5*t-(t/24)^2)#mm regenhöhe starkregen

