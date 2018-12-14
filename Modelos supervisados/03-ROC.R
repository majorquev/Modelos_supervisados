#### plotROC #####
library(plotROC)#install.packages("plotROC")
library(ggplot2)

# Analizamos ejemplo básico #
D.ex <- rbinom(50, 1, .5)
rocdata <- data.frame(D = c(D.ex, D.ex), 
                      M = as.numeric(c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)) ) ,
                      Z = c(rep("A", 50), rep("B", 50)))

ggplot(rocdata, aes(m = M, d = D)) + geom_roc()


getwd()
setwd("C:/Users/Usuario/Dropbox/Cursos/R/Topicos_basicosR/Parte 2-Introducción a ML con R")
# cÓMO CARGAR UN ARCHIVO RDS EN SESIÓN readRDS()
arbol2<-readRDS("arbol2.rds")
arbol2.pod<-readRDS("arbol2_pod.rds")

### opcional para archivos csv: read.csv(), write.csv(nombre_objeto, "objeto.csv")

# Graficamos la curva de ROC para ambos modelos en la data de entrenamiento para ambos árboles..
rocdata <- data.frame(
  D = c(train$AHD,train$AHD)
  ,M = c( as.data.frame(predict(arbol2,newdata = train,type="prob"))$Yes, as.data.frame(predict(arbol2.pod,newdata = train,type="prob"))$Yes )
  ,Z = c( rep("arbol2",dim(train)[1]),rep("arbol2.pod",dim(train)[1])) 
)


roc<-ggplot(rocdata, aes(m = M, d = D,color=Z)) + geom_roc()
roc
calc_auc(roc)



# Recordemos que nos intersa evaluar el desempeño en la data de test
rocdata <- data.frame(
   D = c(test$AHD,test$AHD)
  ,M = c( as.data.frame(predict(arbol2,newdata = test,type="prob"))$Yes, as.data.frame(predict(arbol2.pod,newdata = test,type="prob"))$Yes )
  ,Z = c( rep("arbol2",dim(test)[1]),rep("arbol2.pod",dim(test)[1])) 
)


roc<-ggplot(rocdata, aes(m = M, d = D,color=Z)) + geom_roc()
roc

calc_auc(roc)

