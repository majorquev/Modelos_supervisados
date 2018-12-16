library(ggplot2)
library(class)
x<-seq(0,4,by=0.01)


f<-function(x){
  t<-(-x-1)*(x-3)*(x-4)
return(t)
  }

plot(x,f(x),type="l")


set.seed(123)
x1<-runif(100,0,4)
y1<-f(x1)+ rnorm(100,sd=1.5)




train <- data.frame(
  x1 = x1,
  y1 = y1,
  x2 = x1 ^ 2,
  x3 = x1 ^ 3,
  x4 = x1 ^ 4,
  x5 = x1 ^ 5,
  x6 = x1 ^ 6,
  x7 = x1 ^ 7,
  x8 = x1 ^ 8,
  x9 = x1 ^ 9,
  x10 = x1 ^ 10,
  x11 = x1 ^ 11
)



plot(train$x1,train$y1)


x1t<-runif(50,0,4)
test<-data.frame(
  x1 = x1t,
  y1 = f(x1t),
  x2 = x1t ^ 2,
  x3 = x1t ^ 3,
  x4 = x1t ^ 4,
  x5 = x1t ^ 5,
  x6 = x1t ^ 6,
  x7 = x1t ^ 7,
  x8 = x1t ^ 8,
  x9 = x1t ^ 9,
  x10 = x1t ^ 10,
  x11 = x1t ^ 11
  
)


mod1<-lm(y1~x1,data=train)
mod2<-lm(y1~x1+x2+x3,data=train)
mod3<-lm(y1~x1+x2+x3+x4+x5,data=train)
mod4<-lm(y1~x1+x2+x3+x4+x5+x6+x7,data=train)
mod5<-lm(y1~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=train)
mod6<-lm(y1~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,data=train)


(e1<-sqrt(sum((train$y1-predict(mod1))^2)/(dim(train)[1])))
(e2<-sqrt(sum((train$y1-predict(mod2))^2)/(dim(train)[1])))
(e3<-sqrt(sum((train$y1-predict(mod3))^2)/(dim(train)[1])))
(e4<-sqrt(sum((train$y1-predict(mod4))^2)/(dim(train)[1])))
(e5<-sqrt(sum((train$y1-predict(mod5))^2)/(dim(train)[1])))
(e6<-sqrt(sum((train$y1-predict(mod6))^2)/(dim(train)[1])))


ggplot(train) + geom_point(aes(x1,y1))+geom_line(aes(x1,predict(mod1) ),color="red")+
  geom_line(aes(x1,predict(mod2) ),color="blue")+
  geom_line(aes(x1,predict(mod6) ),color="green")


(e1t<-sqrt(sum((test$y1-predict(mod1,newdata = test))^2)/(dim(test)[1])))
(e2t<-sqrt(sum((test$y1-predict(mod2,newdata = test))^2)/(dim(test)[1])))
(e3t<-sqrt(sum((test$y1-predict(mod3,newdata = test))^2)/(dim(test)[1])))
(e4t<-sqrt(sum((test$y1-predict(mod4,newdata = test))^2)/(dim(test)[1])))
(e5t<-sqrt(sum((test$y1-predict(mod5,newdata = test))^2)/(dim(test)[1])))
(e6t<-sqrt(sum((test$y1-predict(mod6,newdata = test))^2)/(dim(test)[1])))


err<-data.frame(p=c(1,3,5,7,9,11),e=c(e1,e2,e3,e4,e5,e6),et=c(e1t,e2t,e3t,e4t,e5t,e6t))

ggplot(err) + geom_line(aes(p,e),color="red") + geom_line(aes(p,et),color="blue")



