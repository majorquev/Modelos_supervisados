library(ggplot2)
library(dplyr)

set.seed(99)
x <- rnorm(100,sd = 1)
e <- rnorm(100,sd = 3)

y <- 1 + x^3  +e




x2 <- x^2
x3 <- x^3
x4 <- x^4
x5 <- x^5
x6 <- x^6
x7 <- x^7
x8 <- x^8
x9 <- x^9
x10 <- x^10
x11 <- x^11


df <- data.frame(x = x,
                 y= y,
                 x2 = x2 ,
                 x3 = x3 ,
                 x4 = x4 ,
                 x5 = x5 ,
                 x6 = x6 ,
                 x7 = x7 ,
                 x8 = x8 ,
                 x9 = x9 ,
                 x10= x10,
                 x11= x11
                 
)

train <- sample_frac(df,.7)
test <- anti_join(df,train)

ggplot(df) +
  aes(x,y) +
  geom_point()

ggplot(train) +
  aes(x,y) +
  geom_point(col = "red") +
  geom_point(mapping = aes(x,y),data = test, color ="green")


m1 <- lm(y ~ x, data = train)
m2 <- lm(y ~ x + x2, data = train)
m3 <- lm(y ~ x + x2 + x3, data = train)
m4 <- lm(y ~ x + x2 + x3 + x4 , data = train)
m5 <- lm(y ~ x + x2 + x3 + x4 + x5, data = train)
m6 <- lm(y ~ x + x2 + x3 + x4 + x5 +x6 + x7 + x8+ x9, data = train)
m7 <- lm(y ~ x + x2 + x3 + x4 + x5 +x6 + x7 + x8+ x9 + x10 + x11, data = train)
mkn1 <- knnreg(train["x"], train$y, k = 1)

results <- data.frame(x=rep(train$x,6),
                      y=rep(train$y,6),
                      pred_train = c(predict(m1),
                                     predict(m2),
                                     predict(m3),
                                     predict(m6),
                                     predict(m7), 
                                     predict(mkn1,train["x"]) ),
                      model = c(rep("m1",70),
                                rep("m2",70),
                                rep("m3",70),
                                rep("m6",70),
                                rep("m7",70),
                                rep("kn1",70)))

ggplot(results) +
  geom_point(aes(x = x , y =y)) +
  geom_line(aes(x = x, y = pred_train,col = model)) + 
  facet_wrap(~ model)


# Generamos predicciones y calculamos el errorcuadrático medio en  --------

p1_test <- predict(m1, test)
p2_test <- predict(m2, test)
p3_test <- predict(m3, test)
p6_test <- predict(m6, test)
p7_test <- predict(m7, test)
pkn1_test <- predict(mkn1,test["x"]) 

p1_train <- predict(m1, train)
p2_train <- predict(m2, train)
p3_train <- predict(m3, train)
p6_train <- predict(m6, train)
p7_train <- predict(m7, train)
pkn1_train <- predict(mkn1,train["x"])


# MAE (error absoluto medio)
mean(abs(p1_test - test$y) )
mean(abs(p1_train - train$y) )

# MSE (error cuadrático medio)
mean((p1_test - test$y)^2)
mean((p1_train - train$y)^2)

# RMSE (root mean square error)(raiz del error cuadrático medio)
sqrt(mean((p1_test - test$y)^2))
sqrt(mean((p1_train - train$y)^2))


sqrt(mean((pkn1_test - test$y)^2))
sqrt(mean((pkn1_train - train$y)^2))

# Errores de test y train -------------------------------------------------

# MSE train 
mse_train <- c(
  mean((p1_train - train$y)^2),
  mean((p2_train - train$y)^2),
  mean((p3_train - train$y)^2),
  mean((p6_train - train$y)^2),
  mean((p7_train - train$y)^2),
  mean((pkn1_train - train$y)^2)
  )

# MSE test
mse_test <- c(mean((p1_test - test$y)^2),
              mean((p2_test - test$y)^2),
              mean((p3_test - test$y)^2),
              mean((p6_test - test$y)^2),
              mean((p7_test - test$y)^2),
              mean((pkn1_test - test$y)^2)
              )

errores <- data.frame(mse_tain = mse_train,
           mse_test = mse_test,
           modelo = c(1,2,3,6,7,Inf)) 
errores %>% 
  ggplot() +
  geom_line(aes(x = modelo, y = mse_test),col = "red" ) +
  geom_line(aes(x = modelo, y = mse_train),col = "green" ) 
