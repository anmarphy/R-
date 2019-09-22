###La serie contiene datos faltantes pero si la fecha

rm(list = ls())
library(ggplot2)
library(zoo)
library(dplyr)

air <- read.csv("~/air.csv", )
colnames(air)<-c("Fecha", "Pasajeros")
#air$dt <-as.Date(air$Fecha)
air$anho<-format(as.Date(air$Fecha, format="%d/%m/%Y"),"%Y")

air%>%select(anho,Pasajeros)%>%group_by(anho)%>%summarise(n())
nrow(air)-0.8*nrow(air)

air$Examina<-c(rep("Training",115), rep("Testing",29))
air$index<-c(1:144)
sum(is.na(air)) ##Datos faltantes

air$Pasajeros<-na.approx(air$Pasajeros)
ggplot(air, aes(index, Pasajeros, color=Examina))+geom_line()

demand <- ts(air$Pasajeros, start = c(1949, 1), frequency = 12)
ap.hw<-HoltWinters(demand, seasonal = "mult")
plot(ap.hw)

ap.predict <-predict(ap.hw, n.ahead = 4*12) ##4 año,12 Meses
ts.plot(demand,ap.predict,lty=1:2)


library(forecast)
AutoArimaModel=auto.arima(demand)
AutoArimaModel ##Modelo propuesto ARIMA(1,1,2)

forecast(AutoArimaModel,48)
plot(forecast(AutoArimaModel,48))



####La serie contiene datos faltantes (valor y fecha)
rm(list = ls())
library(ggplot2)
library(zoo)
library(dplyr)

air <- read.csv("~/air2.csv", )
colnames(air)<-c("Fecha", "Pasajeros")
#air$dt <-as.Date(air$Fecha)
air$anho<-format(as.Date(air$Fecha, format="%d/%m/%Y"),"%Y")

air%>%select(anho,Pasajeros)%>%group_by(anho)%>%summarise(meses=n())%>%arrange(meses)
air2<-air%>%filter(!anho=="NA")
air2%>%select(anho,Pasajeros)%>%group_by(anho)%>%summarise(meses=n())%>%arrange(meses)
fechas_comple<-seq(as.Date("1/01/1949",format="%d/%m/%Y"), as.Date("1/12/1960",format="%d/%m/%Y"), by="month")
completo<-as.data.frame(fechas_comple)
completo$fechas_comple

air$Fecha
air2$newdate <- strptime(as.character(air2$Fecha), "%d/%m/%Y")
air2$txtdate <- format(air2$newdate, "%Y-%m-%d")
completo$txtdate <- format(completo$fechas_comple, "%Y-%d-%m")
head(completo)
head(air2)

nuevo<-left_join(completo,air2, by="txtdate")
air3<-nuevo%>%select(fechas_comple,Pasajeros,anho)
air3$Pasajeros<-na.approx(air3$Pasajeros)


demand <- ts(air3$Pasajeros, start = c(1949, 1), frequency = 12)
ap.hw<-HoltWinters(demand, seasonal = "mult")
plot(ap.hw)
