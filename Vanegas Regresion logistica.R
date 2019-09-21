
source("macros.txt")

#######################################################################################################################
############################################            Ejemplo 1          ############################################
############################################ Mortalidad de los escarabajos ############################################
#######################################################################################################################

############# Escarabajos fueron expuestos a disulfuro de carbono gaseoso durante un periodo de 5 horas.
############# Ocho experimentos se llevaron a cabo con diferentes concentraciones de disulfuro de carbono.
############# El objetivo principal del estudio es describir la probabilidad de que los escarabajos mueran.

###################### Lectura de los datos ######################
totales <- c(59, 60, 62, 56, 63, 59, 62, 60)
exitos  <- c(6, 13, 18, 28, 52, 53, 61, 60)
dosis <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
y <- cbind(exitos,totales-exitos)
y
###################### Gráfico de los datos ######################
mort <- 100*exitos/totales
plot(dosis,mort,xlim=range(dosis),ylim=range(mort),cex=0.5,lwd=3,xlab="Dosis de disulfuro de carbono",ylab="Tasa de mortalidad (%)",col="blue")

###################### Estimación de los modelos con funciones ######################
###################### de enlace logit, probit y cloglog ######################
fit1 <- glm(y ~ dosis, family=binomial(link="logit"))
summary(fit)
fit2 <- glm(y ~ dosis, family=binomial(link="probit"))
fit3 <- glm(y ~ dosis, family=binomial(link="cloglog"))

fit0  <- glm(y ~ 1, family=binomial(link="logit"))
anova(fit0,fit1,test="LRT")
summary(fit1)
summary(fit2)
summary(fit3)

coef(fit3)

vcov=vcov(fit3)
sqrt(diag(vcov))

AIC(fit1)
AIC(fit2)
AIC(fit3)

deviance(fit1)
deviance(fit2)
deviance(fit3)
###################### Gráfico de los modelos estimados con funciones ######################
###################### de enlace logit, probit y cloglog ######################
dosisg <- seq(min(dosis),max(dosis),length=100)
plot(dosisg,predict(fit1,data.frame(dosis=dosisg),type="response"),xlim=range(dosis),ylim=range(fitted(fit1)),col="black",type="l",xlab="",ylab="",main="")
par(new=TRUE)
plot(dosisg,predict(fit2,data.frame(dosis=dosisg),type="response"),xlim=range(dosis),ylim=range(fitted(fit1)),col="blue",type="l",xlab="",ylab="",main="")
par(new=TRUE)
plot(dosisg,predict(fit3,data.frame(dosis=dosisg),type="response"),xlim=range(dosis),ylim=range(fitted(fit1)),col="red",type="l",xlab="",ylab="",main="")
par(new=TRUE)
plot(dosis,exitos/totales,xlim=range(dosis),ylim=range(fitted(fit1)),cex=0.5,lwd=3,xlab="Dosis de disulfuro de carbono",ylab="Probabilidad de muerte",main="")
legend(locator(1),legend=c("logit","probit","cloglog"),col=c("black","blue","red"),lty=1)

###################### Valores del desvio y el AIC ######################
AIC(fit1)
AIC(fit2)
AIC(fit3)
deviance(fit1)
deviance(fit2)
deviance(fit3)

###################### Resumen del modelo seleccionado ######################
summary(fit3)

################################# Diagnóstico del modelo seleccionado #################################
###################### Análisis de sensibilidad ######################
dC(fit3, identify=1)
fit4 <- glm(y ~ dosis, family=binomial(link="cloglog"), subset=-c(5))
100*(coef(fit4)-coef(fit3))/coef(fit3)

###################### Gráfico de residuos con bandas de confianza ######################
bc(fit3, rep=100, alpha=0.99)

################################# Estimando DL100p #################################
p <- 0.8
p=0.5
DL100p <- (log(-log(1-p)) - coef(fit3)[1])/coef(fit3)[2]
DL100p
D <- rbind(-1/coef(fit3)[2], -(log(-log(1-p)) - coef(fit3)[1])/(coef(fit3)[2])^2)
varesDL100p <- t(D)%*%vcov(fit3)%*%D
li <- DL100p - qnorm(0.975)*sqrt(varesDL100p)
ls <- DL100p + qnorm(0.975)*sqrt(varesDL100p)
cbind(li,ls)


################################# Grafico DL100p #################################
plot(dosisg,predict(fit3,data.frame(dosis=dosisg),type="response"),xlim=range(dosis),ylim=range(fitted(fit1)),col="red",type="l",xlab="",ylab="",main="")
par(new=TRUE)
plot(dosis,exitos/totales,xlim=range(dosis),ylim=range(fitted(fit1)),cex=0.5,lwd=3,xlab="Dosis de disulfuro de carbono",ylab="Probabilidad de muerte",main="")
abline(v=DL100p, lty=3)
abline(h=p, lty=3)


#######################################################################################################################
############################################            Ejemplo 2          ############################################
############################################            Gestantes          ############################################
#######################################################################################################################

############# Un grupo de gestantes fumadoras son clasificadas según los siguientes factores:
############# edad (<30 o >=30 años), número promedio de cigarrillos consumidos por dia (<5 o >=5 cigarrillos), tiempo de gestación (<=260
############# o >260 dias) y el estado del bebé (sobrevivió o no sobrevivió). El objetivo principal del estudio es explicar la probabilidad
############# de que el bebé sobreviva dadas las variables de explicación Edad, Cigarrillos y Gestación.

###################### Lectura de los datos ######################
gestantes <- read.table("gestantes.txt",head=TRUE)
gestantes
y <- cbind(gestantes$exito,gestantes$fracaso)
y
###################### Estimación del modelo ######################
fit1 <- glm(y ~ edad + cigarrillos + gestacion, data=gestantes, family=binomial(link="logit"))
summary(fit1)
###################### Significancia de cigarrillos ######################
######################     usando el test de LR     ######################
fit2 <- glm(y ~ edad + gestacion, data=gestantes, family=binomial(link="logit"))
summary(fit2)
anova(fit2,fit1, test="LRT")

###################### Significancia de la interacción ######################
######################     usando el test de LR        ######################
fit3 <- glm(y ~ edad + gestacion + edad*gestacion, data=gestantes, family=binomial(link="logit"))
summary(fit3)
anova(fit2,fit3, test="LRT")

################################# Diagnóstico del modelo seleccionado #################################
###################### Análisis de sensibilidad ######################
dC(fit2, identify=1)
fit4 <- glm(y ~ edad + gestacion, data=gestantes, family=binomial(link="logit"), subset=-c(1))
100*(coef(fit4)-coef(fit2))/coef(fit2)
summary(fit4)

###################### Resumen del modelo seleccionado ######################
fit5 <- glm(y ~ gestacion, data=gestantes, family=binomial(link="logit"))
summary(fit5)

###################### Intervalo de Confianza para OR ######################
li <- exp(coef(fit5)[2] - qnorm(0.975)*sqrt(diag(vcov(fit5)))[2]) 
ls <- exp(coef(fit5)[2] + qnorm(0.975)*sqrt(diag(vcov(fit5)))[2]) 
cbind(li,ls)

###################### Gráfico de residuos con bandas de confianza ######################
bc(fit5, rep=100, alpha=0.99)