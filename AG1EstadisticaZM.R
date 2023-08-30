# Cargar dataset

setwd("/Users/zoleida.morales/Documents/Master Bigdata/06_EstadisticaAvanzada/Practica1")
wd <- getwd(); file <- paste(wd,
                             "Base_proyecto.csv",sep="/")

# Para leer los datos anteriores
Proyecto<-read.csv(file, head=TRUE)[,-1]

#Exploremos los datos
library(readxl)
library(dplyr)
library(tidyr)

dim(Proyecto)
head(Proyecto, n=5)
glimpse(Proyecto)

Proyecto1 <- select(Proyecto, ValorAsegurado, ValorPrimaAnual, 
                    ValorAseguradoVehiculo)/100000
glimpse(Proyecto1)


Proyecto1_cor <-cor(Proyecto1, method = 'pearson')
round_corr <-round(Proyecto1_cor,digits = 1)
round_corr

summary(Proyecto1)



#Divide la pantalla en 2 columnas
par(mfrow=c(1,2))

plot(x=Proyecto1$ValorAsegurado, y=Proyecto1$ValorPrimaAnual)
plot(x=Proyecto1$ValorAseguradoVehiculo, y=Proyecto1$ValorPrimaAnual)


# Regresion lineal
library(MASS)
library(ISLR)

lm.fit <- lm(ValorPrimaAnual~ValorAsegurado, data = Proyecto1)
summary(lm.fit)
(lm.fit)
coef(lm.fit)

# Calculamos intervalos
confint(lm.fit)
predict(lm.fit,data.frame(ValorAsegurado=(c(5,10,15))),
        interval="prediction")

# Validamos los supuestos
par(mfrow=c(2,2))
plot(lm.fit)


lm.fit <- lm(ValorPrimaAnual~ValorAseguradoVehiculo, data = Proyecto1)
summary(lm.fit)
(lm.fit)
coef(lm.fit)

# Calculamos intervalos
confint(lm.fit)
predict(lm.fit,data.frame(ValorAseguradoVehiculo=(c(5,10,15))),
        interval="prediction")

# Validamos los supuestos
par(mfrow=c(2,2))
plot(lm.fit)


# Graficamos el modelo
par(mfrow=c(1,2))

plot(Proyecto1$ValorAsegurado,Proyecto1$ValorPrimaAnual)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="purple")

plot(Proyecto1$ValorAseguradoVehiculo,Proyecto1$ValorPrimaAnual)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="orange")



# REGRESION LINEAL MULTIPLE

lm.fit <- lm(ValorPrimaAnual~ValorAsegurado+
             ValorAseguradoVehiculo, data = Proyecto1)
summary(lm.fit)
lm.fit <- lm(ValorPrimaAnual~., data=Proyecto1)
summary(lm.fit)


lm.fit <- lm(ValorPrimaAnual~ValorAsegurado
             +ValorAseguradoVehiculo+TipoPoliza, 
             data = Proyecto)
summary(lm.fit)

