library("caret") # install.packages("caret")
# otra opción es H20.
# 
setwd("C:/Users/Usuario/Dropbox/Cursos/R/Topicos_basicosR/Parte 2-Introducción a ML con R")

heart<- read.csv("C:/Users/Usuario/Dropbox/Cursos/R/Topicos_basicosR/datasets/Heart.csv", header =TRUE)

heart$Sex <- as.factor(heart$Sex)
heart$ChestPain <- as.factor(heart$ChestPain)
heart$RestECG <- as.factor(heart$RestECG)
heart$ExAng <- as.factor(heart$ExAng)
heart$Slope <- as.factor(heart$Slope)
heart$Thal <- as.factor(heart$Thal)

heart<-na.omit(heart)

# split data --------------------------------------------------------------

inTrain <- createDataPartition(heart$AHD, p = 0.7, list = FALSE)
train <- heart[inTrain,]
test <- heart[-inTrain,]

summary(train$Sex)
summary(heart$Sex)

View(train)

dim(heart)

# Podemos detectar multicolinealidad --------------------------------------


# definir cols
cols<-which(!(sapply(train, FUN = class) == "factor"))


descrCorr <- cor(train[,cols],use = "na.or.complete")
highCorr <- findCorrelation(descrCorr, 0.3)
trainDescr <- train[,cols][,-highCorr]
testDescr <- test[,cols][, -highCorr]

# Escalamiento y centrado de una matriz -----------------------------------

# Utilice el comando scale
scale(trainDescr)


# Ahora con preProcess
# ?preProcess
xTrans <- preProcess(trainDescr)
trainDescr <- predict(xTrans, trainDescr)
testDescr <- predict(xTrans, testDescr)

xTrans <- preProcess(trainDescr,method = "pca",thresh = 0.8)
dim(trainDescr)
# trainDescr <- train[, -highCorr]
# testDescr <- test[, -highCorr]


# Entrenamiento y tuning --------------------------------------------------
# función estrella : train()

?train
?trainControl

as.factor(trainDescr$AHD)-> trainClass
trainDescr$AHD<-NULL

bootControl <- trainControl(number = 200)
set.seed(2)
rfFit <- train(
  x=select(train,-AHD),
  y=train$AHD,
  method = "rf",
  tuneLength = 5,
  trControl = bootControl,
  scaled = FALSE,
  na.action = na.omit
)

summary(rfFit)
rfFit$results
rfFit$bestTune
rfFit$finalModel

pred<-predict(rfFit,newdata = test)

table(test$AHD,pred)


# 
# predict(svmFit,testDescr)
# table(testDescr$AHD, predict(svmFit,testDescr))
# confusionMatrix(predict(svmFit,testDescr), testDescr$AHD)
# 
# names(getModelInfo())

