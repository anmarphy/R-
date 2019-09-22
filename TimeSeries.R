rm(list = ls())
air<-AirPassengers
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

plot.ts(air) ##Grafica de la serie de tiempo
plot(stl(air, "per"))

plot(decompose(air)) ##Aditivo
plot(decompose(air, type="mult")) 
#En este caso un modelo multiplicativo es mas apropiado que uno aditivo
#pq la varianza y tendencia de la serie aumenta con el tiempo
plot(decompose(air))

sd(air[7:138]-ap_de$trend[7:138]) #retirando la tendencia
sd(ap_de$random[7:138]) #Se reduce notoriamente por el cambio estacional, muy efectivo


plot.ts(ap_de$random[7:138]) ##Esta es la que se va a modelar
acf(ap_de$random[7:138]) ##Se pierden los primeros seis y los últimos 6
#La funcion coseno que se observa es caracteristica de modelo 
#autorregresivo de orden 2

#En los datos observados sobresalia que el efecto estacional aumentaba
#junto con la tendencia por eso s vuelve a colocar en el Holt-Winters
ap.hw<-HoltWinters(air, seasonal = "mult")
plot(ap.hw)

ap.predict <-predict(ap.hw, n.ahead = 4*12) ##4 año,12 Meses
ts.plot(air,ap.predict,lty=1:2)

##Veamos los parámetros del modelo
ap.hw$alpha
ap.hw$beta
ap.hw$gamma
#Los valores extrapolados se basan en la tendencia observada en el modelo a
#asumiendo que la tendencia va a continuarl


##Dejando la serie con la misma varianza
plot(log(air))
plot(decompose(log(air)))
loga<-decompose(log(air))
acf(loga$random[7:138],lag.max = 40)
pacf(loga$random[7:138])
##la p puede ser 1 al 12
## la q 1 al 7
##Recordemos que lo que se esta armando son las parejas (p,q)


train_series=air[1:100]
test_series=air[101:131]


library(forecast)
AutoArimaModel=auto.arima(air)
AutoArimaModel ##Modelo propuesto ARIMA(1,1,2)

forecast(AutoArimaModel,31)
plot(forecast(AutoArimaModel,31))
