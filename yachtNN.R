setwd("~/Documents/Universitat/Màster/Q1/CSI/Machine Learning/Project")
#[WEB] https://github.com/SIMIDAT/Redes-Neuronales/blob/master/RedesNeuronales.md
#[WEB] https://gist.github.com/abresler/d2c324b44d7319b58309
#[TO DO] Try to normalize data before training neural network

#Read dataset
data <- read.table(file = "yacht_hydrodynamics.data")
names(data) <- c('LonPos', 'PrismCoeff', 'LenDisRatio', 'BeamDRatio', 'LenBRatio', 'FroudNum', 'Resistance')
N <- nrow(data)

#Scaling data
data <- scale(data)
data <- as.data.frame(data)

#Adding train column
set.seed(1)
learn <- sample(1:N, round(2*N/3))
nlearn <- length(learn)
ntest <- N - nlearn
data.learn <- data[learn,]
data.test <- data[-learn,]

library(neuralnet)

#Neural networn training
nn <- neuralnet(Resistance ~ LonPos + PrismCoeff + LenDisRatio + BeamDRatio + LenBRatio + FroudNum, data = data.learn, hidden = 10, rep = 3, threshold = 0.1, stepmax = 1e7)
plot(nn, rep = 'best')
print(nn)

#variable weights in the output decision
par(mfrow = c(3, 2))
gwplot(nn, selected.covariate = 'LonPos', rep = 'best')
gwplot(nn, selected.covariate = 'PrismCoeff', rep = 'best')
gwplot(nn, selected.covariate = 'LenDisRatio', rep = 'best')
gwplot(nn, selected.covariate = 'BeamDRatio', rep = 'best')
gwplot(nn, selected.covariate = 'LenBRatio', rep = 'best')
gwplot(nn, selected.covariate = 'FroudNum', rep = 'best')

#Neural network validation
output <- compute(nn, data.test[1:6], rep = 1)
result <- data.frame(Real = data.test$Resistance, Predicted = output$net.result, Error = (data.test$Resistance - output$net.result)/data.test$Resistance)

par(mfrow=c(1,1))
plot(data.test$Resistance, col = 'red', ylab = 'Resistance', main = 'Real vs Prediction')
points(output$net.result, col = 'blue')
legend('topright', legend=c('Real','NN'), pch=18, col=c('red','blue'))

plot(result$Error, ylab = 'Error', main = 'Neural Network error', type = 'l', col = 'black')

nn.predictions <- predict(nn)
mean.square.error <- sum((data.learn$Resistance - rf.predictions)^2)/nlearn
mean.square.error*100

nn.predictions <- predict(nn, newdata=data.test)
mean.square.error <- sum((data.test$Resistance - nn.predictions)^2)/ntest
mean.square.error*100

#[TO DO] How to calculate neural network accuracy