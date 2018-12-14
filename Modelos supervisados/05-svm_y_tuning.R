# SVM
library(dplyr)#; install.packages("dplyr") # manipualación de tablas
library(e1071)#; install.packages("e1071") # Implemta algunos algoritmos de ML (particularmente SVM)
library(mvtnorm)# install.packages("mvtnorm") # Permite generar muestras aleatorias de distribuciones normales multivariadas
library(plot3D)

# Datos simulados
# Suponga X_i, i = 1,...,10, iid N(0,1) y consideremos una muestra de 2000 observaciones para cada una de ellas


p<-3
X<-c()

for(i in 1:p){#i=1
  aux<-rnorm(700)
  X<-cbind(aux,X)
}

X<-as.data.frame(X)
colnames(X)<-c("X1","X2","X3")

  
paste0("X",seq(1,p))


X %>% mutate(Y = ifelse(
  X1 + X2 + X3  >= qchisq(p = 0.5, df = p) , 1, -1)
    ) -> X

X$id<-seq(1:dim(X)[1])

prop<-count(dplyr::filter(X,Y==1))/dim(X[1])


X.str<- filter(X,Y==1)
size <- unlist(round(0.7*dim(X.str)[1]))

X.str2<- filter(X,Y==-1)
size2 <- unlist(round(0.7*dim(X.str2)[1]))

id.train<-c(sample(X.str$id,size =size) , sample(X.str2$id,size =size2))

X.train<-X[id.train,]
X.test<-X[-id.train,]

count(dplyr::filter(X.train,Y==1))/dim(X.train[1])
## plot3D

scatter3D(
  X.test$X1,
  X.test$X2,
  X.test$X3,
  colvar = X.test$Y,
  col=c("green","red"),
  phi = 0,
  bty = "g",
  pch=20,
  #pch = c(rep(20,dim(M1)[1]),rep(8,dim(M.test)[1])),
  cex = 1.5,
  colkey = FALSE
)


# e1071 -->libsvm

svm.mod<-svm(factor(Y)~X1+X2+X3
    ,data=X.train
    ,scale= TRUE
    ,type = "C-classification"
    ,cost= 6# variable C del modelo de optimización
    ,kernel = "linear"
    ,cross = 10
    ,probability = TRUE
    )

attributes(svm.mod)
svm.mod$coefs
svm.mod$fitted
svm.mod$SV
svm.mod$decision.values
svm.mod$accuracies
mean(svm.mod$accuracies)


scatter3D(
  X.test$X1,
  X.test$X2,
  X.test$X3,
  colvar = X.test$Y,
  col=c("green","red"),
  phi = 0,
  bty = "g",
  #pch=8,
  pch = ifelse(predict(svm.mod,newdata =  X.test)==1,8,20),
  cex = 1.5,
  colkey = FALSE
)


## Qué ocurre con un random Forest ? 

rf.mod<-randomForest(factor(Y)~X1+X2+X3,
             data=X.train,
             mtry = 2,
             ntree = 500 ,
             nodesize=1,
             importance = TRUE)



scatter3D(
  X.test$X1,
  X.test$X2,
  X.test$X3,
  colvar = X.test$Y,
  col=c("green","red"),
  phi = 0,
  bty = "g",
  #pch=8,
  pch = ifelse(predict(rf.mod,newdata =  X.test)==1,8,20),
  cex = 1.5,
  colkey = FALSE
)

pred.svm<-attr(predict(svm.mod,newdata =  X.test,probability = TRUE),"probabilities")[,1]
pred.rf<-as.data.frame(predict(rf.mod,newdata =  X.test,type="prob"))[,2]

res<-data.frame(
  Y=factor(X.test$Y)
  ,pred.svm = pred.svm
  ,pred.rf = pred.rf)


res2<-melt_roc(data = res,d="Y",m=c("pred.svm","pred.rf"))

ggplot(res2, aes(d=D,m=M,color=name)) + geom_roc()



###################################################################
## Fronteras no lineales

set.seed(666)
sigma<-matrix(c(0.5,.1,0.1,0.5 ),ncol=2)

df<-as.data.frame(rmvnorm(300, mean = c(0,0), sigma = sigma))
colnames(df)<-c("X1","X2")
df$Y<-1


ggplot(data = df) + aes(x=X1,y=X2) + geom_point()


f<-function(x){
  return(list(a=sqrt(25-x^2),a2=-sqrt(25-x^2)))
}


mu1<-seq(from=-5, to=5,by = 0.3)
mu2<-f(mu1)
mu<-data.frame(x=mu1,y=mu2)
  
  
bloques<-c()
  
  for(i in mu1){#i=mu1[1]
    bloques<-rbind(bloques,rmvnorm(5,mean = c(i,f(i)$a),sigma=matrix(c(.5,0,0,.5),ncol=2) )  )
    bloques<-rbind(bloques,rmvnorm(5,mean = c(i,f(i)$a2),sigma=matrix(c(.5,0,0,.5),ncol=2) )  )
    
  }
  

bloques<-as.data.frame(bloques)
colnames(bloques)<-c("X1","X2")
bloques$Y=-1
df<-bind_rows(df,bloques)
df$Y<-as.factor(df$Y)


ggplot(df) + aes(x=X1,y=X2,color=Y) + geom_point()


N<-dim(df)[1]
id.train <- sample(1:N,size=round(0.7*N))

df.train<-df[id.train,]
df.test<-df[-id.train,]


#variemos parámetros
svm.mod2<-svm(Y~.
    ,data = df.train
    ,kernel = "linear"
    ,scale= TRUE
    ,type = "C-classification"
    ,cross = 10
    ,probability = TRUE
    ,cost = 0.1
    )


scatter2D(
  df.test$X1,
  df.test$X2,
  colvar = as.numeric(predict(svm.mod2,newdata =  df.test)),
  col=c("green","red"),
  pch=20,
  cex = 1.5,
  colkey = FALSE
)


# Probamos con nu-classification ------------------------------------------
svm.mod2<-svm(Y~.
              ,data = df.train
              ,kernel = "linear"
              ,gamma  = .5
              ,scale= TRUE
              ,type = "nu-classification"
              ,cross = 10
              ,probability = TRUE
              ,nu = .9
              
)
svm.mod2$accuracies

scatter2D(
  # df.train$X1,
  # df.train$X2,
  df.test$X1,
  df.test$X2,
  colvar = as.numeric(predict(svm.mod2
                              # ,newdata =  df.train
                              ,newdata =  df.test
  )),
  col=c("green","red"),
  pch=20,
  cex = 1.5,
  colkey = FALSE
)


# Finalmente modificamos el kernel ----------------------------------------

svm.mod2<-svm(Y~.
              ,data = df.train
              ,kernel = "radial"
              ,gamma  = .5
              ,scale= TRUE
              ,type = "nu-classification"
              ,cross = 10
              ,probability = TRUE
              ,nu = .9
              
)
svm.mod2$accuracies

scatter2D(
  # df.train$X1,
  # df.train$X2,
  df.test$X1,
  df.test$X2,
  colvar = as.numeric(predict(svm.mod2
                              # ,newdata =  df.train
                              ,newdata =  df.test
                              )),
  col=c("green","red"),
  pch=20,
  cex = 1.5,
  colkey = FALSE
)


# Tuneo con librería e1071 ------------------------------------------------
obj <- tune(svm, Y~., data = df.train, 
            ranges = list(gamma = 2^(-1:1), nu = seq(0.1,0.8,by=0.1)),
            tunecontrol = tune.control(sampling = "cross"),
            kernel = "radial",
            type = "nu-classification",
            probability = TRUE
            
)

obj
attributes(obj)
obj$best.parameters
obj$best.performance
obj$performances
obj$best.model

svm.tuned<-obj$best.model
saveRDS(svm.tuned,"svm.tuned.rds")

scatter2D(
  df.test$X1,
  df.test$X2,
  colvar = as.numeric(predict(svm.tuned
                              ,newdata =  df.test
  )),
  col=c("green","red"),
  pch=20,
  cex = 1.5,
  colkey = FALSE
)



# Podemos tunear el modelo modificando la función de pérdida

error.fun <- function(tv,pv){
  #tv<-c(1,1,1,1,-1,-1,-1,-1,-1,-1)
  #pv<-c(1,-1,1,-1,-1,-1,-1,-1,-1,1)
  #conf<-table(tv,pv)
return(sum(tv[which(tv==-1)]==pv[which(tv==-1)])/length(tv[which(tv==-1)]))
}



# Esta misma librería también nos permite tunear otros modelos, en --------

obj.rf<-tune.randomForest(factor(Y)~.
                  ,data = df.train,
                  mtry = 1:2,
                  # ranges = list(
                  ntree = 500 ,
                  nodesize=1:10,
                  # ),
                  importance = TRUE,
                  tune.control(sampling = "cross"
                               ,cross = 10
                               ,error.fun = error.fun
                               )
                  )




rf.tuned<-obj.rf$best.model
obj.rf$performances


scatter2D(
  df.test$X1,
  df.test$X2,
  colvar = as.numeric(predict(rf.tuned
                              ,newdata =  df.test
  )),
  col=c("green","red"),
  pch=20,
  cex = 1.5,
  colkey = FALSE
)












