library(forecast)
library(highcharter)
library(dplyr)
rm(list=ls())


series<-read.csv("serieTemporal.csv",h=TRUE)

serie.ventas<-ts(select(series,VENTAS),start=c(2004,1),freq=12)
serie.indicador<-ts(select(series,INDICADOR),start=c(2004,1),freq=12)
serie.mult<-ts(select(series,VENTAS,INDICADOR),start=c(2004,1),freq=12)


hchart(serie.indicador)
hchart(serie.mult)

# forecast por defecto realiza un suavizamiento exponencial
forecast(serie.indicador) %>% hchart()

dec<-decompose(serie.ventas)

# descomposicion de una serie
plot(dec)
hchart(dec$seasonal)
hchart(dec$trend)

# Autocorrelación y autocorrelación cruzada

acf(serie.indicador,100)
acf(serie.ventas)

pacf(serie.indicador)
pacf(serie.ventas)

# podemos graficarlos con highchart

hchart(acf(serie.indicador))
hchart(acf(serie.ventas))

hchart(pacf(serie.indicador))
hchart(pacf(serie.ventas))



# Podríamos ajustar para la serie del indiador un modelo MA(1)

sma1001.001<-arima(serie.indicador,order = c(2,1,1) , seasonal = list(order = c(1,0,1),period = 12))
acf(sma1001.001$residuals) %>% hchart()
#Aún se observa autocorrelación en los errores. Posiblemente 

forecast(sma1001.001,10) -> pred.ts1 
attributes(pred.ts1)
pred.ts1$mean
pred.ts1$lower
pred.ts1$upper

pred.ts1 %>% hchart()


# forecast::autoarima


autoarima.mod<-forecast::auto.arima(
  serie.indicador,
  ic = "aic",
  #xreg  =  
  allowdrift = T,
  allowmean = TRUE,
  #d = 1,
  D = 1,
  # max.p = 7,
  # max.q = 7,
  # max.P = 12,
  # max.Q = 12,
  max.order=12,
  max.d = 1
)

forecast(autoarima.mod) %>% hchart()

# Podemos incluir regresores a una serie temporal mediante el argumento xreg.
N<-dim(series)[1]

serie.ventas2<-ts(select(series,VENTAS) ,start=c(2004,1),end=c(2016,12),freq=12)
xreg<-select(series,INDICADOR)[1:(N-4),]
xreg.test<-select(series,INDICADOR)[(N-3):N,]

autoarima.reg<-forecast::auto.arima(
  serie.ventas2,
  ic = "aic",
  xreg  = xreg ,
  allowdrift = T,
  allowmean = TRUE,
  #d = 1,
  D = 1,
  # max.p = 7,
  # max.q = 7,
  # max.P = 12,
  # max.Q = 12,
  max.order=12,
  max.d = 1
)

pred.reg<-forecast(autoarima.reg,xreg=xreg.test) 
serie.ventas

test<-ts(serie.ventas[(N-3):N], start = c(2017,1) ,freq=12)

highchart() %>% hc_add_series(pred.reg) %>% hc_add_series(test)

highchart() %>% hc_add_series(pred.reg) %>% hc_add_series(pred.reg$x) %>% hc_add_series(test)

hchart(pred.reg)











