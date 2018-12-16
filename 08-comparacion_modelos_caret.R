#########
library(gbm)#install.packages("gbm") #Generalized Boosted Regression Modeling
library(dplyr)
library(caret)
# install.packages("caret")
Prestamos <- read.csv("Prestamos_data.csv")
Prestamos <- Prestamos[-1,]


inTrain <- createDataPartition(Prestamos$Prestamo, p = 0.7, list = FALSE)
train   <- Prestamos[inTrain,] %>% select(-ID,-X)
test    <- Prestamos[-inTrain,] %>% select(-ID,-X)



# Random forest -----------------------------------------------------------
# 
trControl <- trainControl(number = 50,search = "grid")
rf.tuned.prestamos <- train(
  x=dplyr::select(train,- Prestamo),
  y=train$Prestamo,
  method = "rf",
  metric="RMSE",
  tuneLength = 5,
  trControl = trControl,
  scaled = FALSE,
  na.action = na.omit
)


pred.rf<-predict(rf.tuned.prestamos,newdata = test) 
res<-data.frame(Prestamoreal = test$Prestamo, PrestamoPredicho = pred.rf)

(MSE.rf<-sqrt(sum((res$PrestamoPredicho-res$Prestamoreal)^2)/dim(test)[1]))

summary(res$Prestamoreal)


# Adaboost ----------------------------------------------------------------

boost.prestamos <-
  gbm(
    Prestamo ~ .,
    data = train,
    distribution = "gaussian",
    n.trees = 5000 ,
    interaction.depth = 4
  )

# Comparar desempeños mediante MSE para ambos métodos


pred.boost<-predict(boost.prestamos,newdata = test,n.trees = 5000)

res$PrestamoPredicho.boost<-pred.boost

(MSE.boost<-sqrt(sum((res$PrestamoPredicho.boost-res$Prestamoreal)^2)/dim(test)[1]))

MSE.boost;MSE.rf

# Adaboost tuned ----------------------------------------------------------
trControl <- trainControl(number = 50,search = "grid")
gbm.tuned.prestamos <- train(
  x=select(train,- Prestamo),
  y=train$Prestamo,
  method = "gbm", 
  distribution = "gaussian",
  metric="RMSE",
  tuneLength = 5,
  trControl = trControl
  # scaled = FALSE,
  # na.omit = TRUE
)

gbm.tuned.prestamos$results

PrestamoPredicho.boost.tuned<-predict(gbm.tuned.prestamos$finalModel,newdata = test,n.trees = 250) 
res$PrestamoPredicho.boost.tuned<-PrestamoPredicho.boost.tuned
(MSE.boost.tuned<-sqrt(sum((res$PrestamoPredicho.boost.tuned-res$Prestamoreal)^2)/dim(test)[1]))

MSE.boost;MSE.rf;MSE.boost.tuned


####################
####################
#################### 

# Neural network ----------------------------------------------------------

tc <- trainControl(method = "boot", number = 50)
nnet.tuned.prestamos <- train(
  x=select(train,- Prestamo),
  y=train$Prestamo,
  method = "nnet",
  metric="RMSE",
  tuneLength = 4,
  trainControl = tc
)
PrestamoPredicho.nnet.tuned<-predict(nnet.tuned.prestamos$finalModel,newdata = test) 
res$PrestamoPredicho.nnet.tuned<-PrestamoPredicho.nnet.tuned
(MSE.nnet.tuned<-sqrt(sum((res$PrestamoPredicho.nnet.tuned-res$Prestamoreal)^2)/dim(test)[1]))

MSE.boost;MSE.rf;MSE.boost.tuned;MSE.nnet.tuned




