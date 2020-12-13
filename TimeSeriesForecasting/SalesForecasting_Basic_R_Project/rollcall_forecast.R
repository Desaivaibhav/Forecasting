
#Date       : 07-09-2017
#Name       : Vaibhav Desai


rm(list=ls())

rollcall <- read.csv(file="D:/vaibhav/ADP/RollcallData.csv", header=TRUE, sep=",")
#View(rollcall)
#install.packages("forecast")
#install.packages("lubridate")

library('forecast')
library('ggplot2')
library('tseries')
library('lubridate')
#rollcall$YTD.SALES
#nrow(rollcall$YTD.SALES)
rollcall<-rollcall[1:365,]

rollcall$id<-seq.int(nrow(rollcall))

weeks<-1:365
#start.date<-as.date("2009/07/06")
rollcall$weekdt<-mdy("July 6 2009")+ weeks(0:(nrow(rollcall)-1))

rollcall$weekdt <- as.Date(rollcall$weekdt,
                    "%Y/%m/%d")

ggplot(rollcall,aes(weekdt,WTD.SALES))+ list(geom_line(),scale_x_date('week'), ylab("sales"),xlab(""))


mf=meanf()

  
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(rolcallf, xreg=fourier(rolcallf, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}


rollcallbats <-tbats(rolcallf)
fc2<-forecast(rollcallbats, h=104)
plot(fc2, ylab="Sales")
summary(fit)


bestfit$coef

plot(rolcallf)
ARIMAfit = auto.arima(rolcallf, approximation=FALSE,trace=FALSE)
summary(ARIMAfit)
fc <-forecast(bestfit,xreg=fourier(rolcallf,K=12,h=104))