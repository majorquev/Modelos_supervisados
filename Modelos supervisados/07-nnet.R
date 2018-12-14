library(ggplot2)
library(mlbench)
library(caret)
library(nnet)

rm(list=ls())
set.seed(666)
spiral <- mlbench.spirals(300, 1, sd = 0.06)
spiral <- cbind(spiral$x, label = spiral$classes)
colnames(spiral)[c(1, 2)] <- c("x1", "x2")
spiral <- as.data.frame(spiral)
spiral$label <- factor(spiral$label)
target <- class.ind(spiral$label)


ggplot(spiral) + aes(x = x1, y = x2, col = label) + geom_point()

x <- seq(-1, 1, 0.02)
y <- rep(x, each = length(x))
grid <- data.frame(x1 = rep(x, length(x)), x2 = y)
ggplot(grid) + aes(x = x1, y = x2) + geom_point(size = .01)


# Entrenamos red usando nnet ----------------------------------------------

model.nn <- nnet(
  # label~.,data=spiral,
  x = spiral[, c("x1", "x2")],
  y = target,
  size = 10,
  #entropy = FALSE,
  softmax = TRUE,
  decay = 0,
  maxit = 100,
  trace = TRUE
)


pred <- predict(model.nn, newdata = grid, type = "class")



ggplot() + geom_point(data = spiral, aes(x = x1, y = x2, col = label)) +
  geom_point(data = grid, aes(x = x1, y = x2, col = pred), size = 0.01)


# Tuneamos usando caret ---------------------------------------------------

mf<-getModelInfo()
names(mf)

mf$nnet$parameters

tc <- trainControl(method = "boot", number = 50)
nnet.tuned <- train(
  x = spiral[, c("x1", "x2")],
  y = spiral$label,
  method = "nnet",
  trainControl = tc
)

pred2 <- predict(nnet.tuned$finalModel, newdata = grid, type = "class")
ggplot() + geom_point(data = spiral, aes(x = x1, y = x2, col = label)) +
  geom_point(data = grid, aes(x = x1, y = x2, col = pred2), size = 0.01)



# Y si comparamos las fronteras de decición con un Random Forest ----------
tc <- trainControl(method = "boot", number = 100)
rf.tuned <- train(
  x = spiral[, c("x1", "x2")],
  y = spiral$label,
  method = "rf",
  trainControl = tc
)
pred3 <- predict(rf.tuned$finalModel, newdata = grid, type = "class")
ggplot() + geom_point(data = spiral, aes(x = x1, y = x2, col = label)) +
  geom_point(data = grid, aes(x = x1, y = x2, col = pred3), size = 0.01)


# Y si comparamos las fronteras de decición con svm ----------
tc <- trainControl(method = "boot", number = 100)
svm.tuned <- train(
  x = spiral[, c("x1", "x2")],
  y = spiral$label,
  method = "svmRadial",
  trainControl = tc
)
pred4 <- predict(svm.tuned, newdata = grid)
ggplot() + geom_point(data = spiral, aes(x = x1, y = x2, col = label)) +
  geom_point(data = grid, aes(x = x1, y = x2, col = pred4), size = 0.01)


# Y si comparamos las fronteras de decición con logística ----------
tc <- trainControl(method = "boot", number = 100)
glm.tuned <- train(
  x = spiral[, c("x1", "x2")],
  y = spiral$label,
  method = "regLogistic",
  trainControl = tc
)
pred4 <- predict(glm.tuned$finalModel, newx = grid,proba = TRUE)$probabilities[,2]>=0.5
ggplot() + geom_point(data = spiral, aes(x = x1, y = x2, col = label)) +
  geom_point(data = grid, aes(x = x1, y = x2, col = pred4), size = 0.01)



