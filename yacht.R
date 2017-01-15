setwd("~/Documents/Universitat/Màster/Q1/CSI/Machine Learning/Project")

#Read dataset
data <- read.table(file = "yacht_hydrodynamics.data")
names(data) <- c('LonPos', 'PrismCoeff', 'LenDisRatio', 'BeamDRatio', 'LenBRatio', 'FroudNum', 'Resistance')
N <- nrow(data)

#Scaling data
data <- scale(data)
data <- as.data.frame(data)

#Learn and train datasets
set.seed(1)
learn <- sample(1:N, round(2*N/3))
nlearn <- length(learn)
ntest <- N - nlearn
data.learn <- data[learn,]
data.test <- data[-learn,]

#################
#RIDGE REGRESSION
#################
library(MASS)
library(car)

model.ridge <- lm.ridge(Resistance ~ ., data=data.learn, lambda = seq(0,10,0.1))

plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]

colors <- rainbow(8)

matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(data.learn)[-9], pos=4, col=colors)

model.ridgereg.FINAL <- lm.ridge(Resistance ~ ., data=data.learn, lambda = lambda.ridge)

(beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL))

(pred.ridgereg <- sum((data.learn$Resistance - beta.ridgereg.FINAL[1] - as.matrix(data.learn[,1:6])%*%beta.ridgereg.FINAL[2:7])^2)/nlearn)*100
(pred.ridgereg <- sum((data.test$Resistance - beta.ridgereg.FINAL[1] - as.matrix(data.test[,1:6])%*%beta.ridgereg.FINAL[2:7])^2)/ntest)*100

#################
#LASSO REGRESSION
#################
library(lars)

model.lasso <- lars(as.matrix(data.test[,1:6]), as.numeric(data.test$Resistance), type="lasso")

lambda.lasso <- c(model.lasso$lambda, 0)

beta.lasso <- coef(model.lasso)

colors <- rainbow(8)

beta.scale <- attr(model.lasso$beta, "scaled:scale")
beta.rescaled <- beta.lasso
for(j in 1:9) beta.rescaled[j,] <- beta.rescaled[j,]*beta.scale

matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta.lasso)), col=colors)
text(rep(-0, 9), beta.rescaled[9,], colnames(data), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

(beta.lasso <- beta.lasso[4,])

(pred.lasso <- sum((data.learn$Resistance - predict(model.lasso, as.matrix(data.learn[,1:6]), s=4, type="fit")$fit)^2)/nlearn)*100
(pred.lasso <- sum((data.test$Resistance - predict(model.lasso, as.matrix(data.test[,1:6]), s=4, type="fit")$fit)^2)/ntest)*100

##############
#DECISION TREE
##############
library(rpart)

tree <- rpart(data.learn$Resistance ~ ., data=data.learn, method = "anova")

tree
plot(tree)
text(tree, use.n=T, pretty=1)

tree.predictions <- predict(tree) 

mean.square.error <- sum((data.learn$Resistance - tree.predictions)^2)/nlearn
mean.square.error*100

tree.predictions <- predict(tree, newdata=data.test)
mean.square.error <- sum((data.test$Resistance - tree.predictions)^2)/ntest
mean.square.error*100

##############
#RANDOM FOREST
##############
library(randomForest)

rf <- randomForest(Resistance ~ ., data = data.learn, ntree = 500, proximity = FALSE)

print(rf)

plot(rf)
legend("topright", legend=c("OOB"),    
       pch=c(1), col=c("black"))

rf <- randomForest(Resistance ~ ., data = data.learn, ntree = 100, proximity = FALSE)

#Importance of variables
importance(rf)
varImpPlot(rf)
varUsed(rf)

rf.predictions <- predict(rf)
mean.square.error <- sum((data.learn$Resistance - rf.predictions)^2)/nlearn
mean.square.error*100

rf.predictions <- predict(rf, newdata=data.test)
mean.square.error <- sum((data.test$Resistance - rf.predictions)^2)/ntest
mean.square.error*100
