#############################
## Bagging y random forest ##
#############################
library(plotROC) # graficar curvas ROC
library(randomForest) #install.packages("randomForest") #  bagging y Random Forest
library(dplyr)#; install.packages("dplyr") # manipualación de tablas
library(e1071)#; install.packages("e1071") # Implemta algunos algoritmos de ML (particularmente SVM)
library(mvtnorm)# install.packages("mvtnorm") # Permite generar muestras aleatorias de distribuciones normales multivariadas
library(plot3D)#install.packages('plot3D') # gráficos 3D

setwd("C:/Users/Usuario/Dropbox/Cursos/R/Topicos_basicosR/Parte 2-Introducción a ML con R")
heart<- read.csv("C:/Users/Usuario/Dropbox/Cursos/R/Topicos_basicosR/datasets/Heart.csv", header =TRUE)

heart$Sex <- as.factor(heart$Sex)
heart$ChestPain <- as.factor(heart$ChestPain)
heart$RestECG <- as.factor(heart$RestECG)
heart$ExAng <- as.factor(heart$ExAng)
heart$Slope <- as.factor(heart$Slope)
heart$Thal <- as.factor(heart$Thal)
#############################
#### División de la data ####
#############################

# Recordar ques este es un paso que de hoy en adelante deben considerar en sus modelos.
heart<-na.omit(heart)
set.seed(123)
N<-dim(heart)[1]
id<-sample(1:N, size = round(0.7*N)) # defino (aleatoriamente) los índices de las filas que formarán parte de la data de entrenamiento

train<-heart[id,] ;dim(train)
test<-heart[-id,] ;dim(test)

## Entrenamiento Bagging
# Recordar que Bagging es un caso particular de Random Forest (se escoje el total 'p' de covariables para realizar los splits en cada iteración.)
bagg.heart <-
  randomForest(AHD ~ .,
               data = train ,
               ntree = 500,
               mtry = 13,
               maxnodes=4,
               importance = TRUE)
# observar que en el output tenemos la estimación del error de test dado por OOB.
# La matriz de confusión se determina con base en las observaciones que quedan "out of bag" en todas las iteraciones. (Se espera que para un número alto de árboles la mayoría de las observaciones de entrenamiento queden en algún momento "out of bag")
bagg.heart



# Si variamos el parámetro mtry a un valor menor a p, estamos ajustando un random Forest.
rf.heart <-
  randomForest(
    AHD ~ .,
    data = train ,
    mtry = 4,
    ntree = 500 ,
    maxnodes=4,
    importance = TRUE
  )

rf.heart

### Comparación de modelos###
arbol2.pod<-readRDS("arbol2_pod.rds")

pred_arbol2.pod <-as.data.frame(predict(arbol2.pod,newdata=test,type="prob"))$Yes
pred_bagg       <-as.data.frame(predict(bagg.heart,newdata=test,type="prob"))$Yes
pred_rf         <-as.data.frame(predict(rf.heart,newdata=test,type="prob"))$Yes

### Visualicemos ROC ###

resultados <-
  data.frame(
    clase_real = test$AHD,
    pred_arbol2.pod = pred_arbol2.pod,
    pred_bagg = pred_bagg,
    pred_rf = pred_rf
  )

# View(resultados)

resultados2 <- melt_roc(data = resultados , d="clase_real", m=c("pred_arbol2.pod", "pred_bagg","pred_rf"))
curvas<-ggplot(resultados2, aes(d = D, m = M, color = name)) + geom_roc()
curvas

calc_auc(curvas)



# Regresión logística -----------------------------------------------------

glm.mod<-glm(AHD ~ . , family = binomial(link = "logit"),data = train)
glm.mod
summary(glm.mod)
glm.mod2<-glm(AHD ~ Ca + ChestPain + Sex + RestBP  , family = binomial(link = "logit"),data = train)

glm_pred<-predict(glm.mod,newdata = test,type = "response" )
glm_pred2<-predict(glm.mod2,newdata = test,type = "response" )

resultados <-
  data.frame(
    clase_real = test$AHD,
    pred_arbol2.pod = pred_arbol2.pod,
    pred_bagg = pred_bagg,
    pred_rf = pred_rf,
    pred_glm = glm_pred,
    pred_glm2 = glm_pred2 
  )

resultados2 <- melt_roc(data = resultados , d="clase_real", m=c("pred_arbol2.pod", "pred_bagg","pred_rf","pred_glm","pred_glm2"))
curvas<-ggplot(resultados2, aes(d = D, m = M, color = name)) + geom_roc()
curvas


# Observamos que los métodos bagging y Rf tienen los mejores desempeños.
calc_auc(curvas)


# Adaboost ----------------------------------------------------------------
# Comenzaremos con un ejemplo de regresión, luego, con la librería caret seguiremos con los casos de clasificación.

Prestamos <- read.csv("Prestamos_data.csv")
Prestamos$NivelEstudio<-as.factor(Prestamos$NivelEstudio)
Prestamos$MesPrestamo<-as.factor(Prestamos$MesPrestamo)

Prestamos$NivelEstudio<-as.factor(Prestamos$NivelEstudio)
Prestamos$MesPrestamo<-as.factor(Prestamos$MesPrestamo)
set.seed (1)

N2<-dim(Prestamos)[1]
ind.train<-sample(1:N2,size=round(N2*0.7),replace=TRUE)

train <- Prestamos[ind.train,] 
test <- Prestamos[-ind.train,] 


boost.prestamos <-
  gbm(
    Prestamo ~ .,
    data = train,
    distribution = "gaussian",
    n.trees = 5000 ,
    interaction.depth = 4
  )

pred.boost<-predict(boost.prestamos,newdata = test,n.trees = 5000)
(MSE.boost<-sqrt(sum((pred.boost-test$Prestamo)^2)/dim(test)[1]))


# Guardamos predicciones en un data.frame

res<-data.frame(PrestamoReal = test$Prestamo,pred.boost=pred.boost)



# Podemos entrenar un Random Forest y comparar ambos desempeños------------

rf.prestamos <-
  randomForest(
    Prestamo ~ .,
    data = train ,
    mtry = 4,
    ntree = 500 ,
    maxnodes=4,
    importance = TRUE
  )

pred.rf<-predict(rf.prestamos,newdata = test,n.trees = 5000)
(MSE.rf<-sqrt(sum((pred.rf-test$Prestamo)^2)/dim(test)[1]))

res$pred.rf<-pred.rf

MSE.boost;MSE.rf





