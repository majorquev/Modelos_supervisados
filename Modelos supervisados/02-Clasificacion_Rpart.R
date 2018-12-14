library(rpart)#librería para generar árboles de clasifiación y regresión con el método 
library(ISLR) #Introduction to statistical learning (versión intermedia) --> versión 2: Statistical Learning.
library(visNetwork) #install.packages("visNetwork")# Visualización de grafos.
# install.packages("visNetwork",dependencies = TRUE) # instala las librerías dependientes automáticamente


############################################
###### Recuerde cambiar su directorio ######
############################################

#####################
## Leemos la tabla ##
#####################
heart<- read.csv("C:/Users/Usuario/Dropbox/Cursos/R/Topicos_basicosR/datasets/Heart.csv", header =TRUE)
head(heart)
names(heart)

unique(heart$Fbs)
typeof(heart$Sex)

heart$Sex <- as.factor(heart$Sex)
heart$ChestPain <- as.factor(heart$ChestPain)
heart$RestECG <- as.factor(heart$RestECG)
heart$ExAng <- as.factor(heart$ExAng)
heart$Slope <- as.factor(heart$Slope)
heart$Thal <- as.factor(heart$Thal)

#####################


#############################
#### División de la data ####
#############################

# Recordar ques este es un paso que de hoy en adelante deben considerar en sus modelos.
heart<-na.omit(heart)

N<-dim(heart)[1]

# Filtrar data para quitar NA's
dplyr::filter(heart,!is.na(Ca) | !is.na(Thal) )

#La función na.omit omite NA en vectors, listas o data.frames
na.omit(c(1,2,3,NA,4,5))

exam<-data.frame(x=c(1,NA,3),y=c(3,2,1))
na.omit(exam)


1== 0+1
is.na(NA)

set.seed(6000)
id<-sample(1:N, size = round(0.7*N)) # defino (aleatoriamente) los índices de las filas que formarán parte de la data de entrenamiento

train<-heart[id,] ;dim(train)
test<-heart[-id,] ;dim(test)


summary(heart)

summary(train)
summary(test)



##########################
### Ajuste de un árbol ###
##########################

#por defecto los "split" se basan en el índice de gini
arbol0<-rpart(formula = AHD ~ .,data = train)

arbol0$frame
plot(arbol0)
text(arbol0, use.n=TRUE)
visTree(arbol0, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)

#ramificando por ganancia de información
arbol1<- rpart(formula= AHD~.,data = train,parms=list(split='information'))
visTree(arbol1, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)

summary(arbol1)
##############################
## importancia de variables ##
##############################

names(arbol1)
arbol1$variable.importance



######################
#### Predicciones ####
######################

p1<-predict(arbol1,newdata=test,type ="prob") #ojo que las predicciones se realizan en la data de test
p2<-as.character(predict(arbol1,newdata=test,type ="class"))

cbind(p1,p2) #juntar columnas
A<-cbind(as.character(test$AHD),p2)
colnames(A) <- c("Clase_real","clase_predicha")
A<-as.data.frame(A)
table(A)

visTree(arbol1, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)


arbol1$cptable
plotcp(arbol1)

## SOBREAJUSTE ##
arbol2<- rpart(formula= AHD~.,data = train,parms=list(split='information'),control = rpart.control(minsplit=2,cp=0.0001))
visTree(arbol2, height = "800px", nodesPopSize = TRUE, maxNodeSize = 30)

arbol2$cptable


# Como criterio visual podemos podar el árbol al valor del cp a partir del cual el xerror (error de test estimado por validación cruzada) no mejore significativamente.
# En breve se recordará el método de validación cruzada para la estimación del error de test asociado a un modelo de aprendizaje supervisado.
plotcp(arbol2)


?prune.rpart
arbol2.pod<-prune(arbol2,cp=0.048)
visTree(arbol2.pod, height = "800px", nodesPopSize = TRUE, maxNodeSize = 30)


# 1. Formato RDS: Nos permite guradar info cargada en meroria preservando la estructura de la sesión.
# 2. saveRDS(): guarda en el directorio de trabajo, objetos definidos en la sesion , en formato RDS

saveRDS(arbol2,"arbol2.rds")
saveRDS(arbol2.pod,"arbol2_pod.rds")

rm(arbol2.pod)

# cÓMO CARGAR UN ARCHIVO RDS EN SESIÓN readRDS()

arbol2<-readRDS("arbol2.rds")
arbol2.pod<-readRDS("arbol2_pod.rds")








