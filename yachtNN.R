setwd("~/Documents/Universitat/Màster/Q1/CSI/Machine Learning/Project")
#[WEB] https://github.com/SIMIDAT/Redes-Neuronales/blob/master/RedesNeuronales.md
#[WEB] https://gist.github.com/abresler/d2c324b44d7319b58309
#[TO DO] Try to normalize data before training neural network

#Read dataset
yacht.data <- read.table(file = "yacht_hydrodynamics.data")
names(yacht.data) <- c('LonPos', 'PrismCoeff', 'LenDisRatio', 'BeamDRatio', 'LenBRatio', 'FroudNum', 'Resistance')
N <- length(yacht.data)

#Scaling dataset
maxs <- apply(yacht.data, 2, max) 
mins <- apply(yacht.data, 2, min)
scaled <- as.data.frame(scale(yacht.data, center = mins, scale = maxs - mins))

#Adding train column
set.seed(1)
yacht.data["train"] <- NA
yacht.data$train <- sample(c('T', 'V'), size = N, replace = TRUE, prob = c(0.7, 0.3))

#Train and test data
train.data <- yacht.data[yacht.data$train == 'T',]
validation.data <- yacht.data[yacht.data$train == 'V',]

library(neuralnet)

#Neural networn training
nn <- neuralnet(Resistance ~ LonPos + PrismCoeff + LenDisRatio + BeamDRatio + LenBRatio + FroudNum, data = train.data, hidden = 5, rep = 3, threshold = 0.1, stepmax = 1e7)
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
output <- compute(nn, validation.data[1:6], rep = 1)
result <- data.frame(Real = validation.data$Resistance, Predicted = output$net.result, Error = (validation.data$Resistance - output$net.result)/validation.data$Resistance)

par(mfrow=c(1,1))
plot(validation.data$Resistance, col = 'red', ylab = 'Resistance', main = 'Real vs Prediction')
points(output$net.result, col = 'blue')
legend('topright', legend=c('Real','NN'), pch=18, col=c('red','blue'))

plot(result$Error, ylab = 'Error', main = 'Neural Network error', type = 'l', col = 'black')

mean(result$Error)

#[TO DO] How to calculate neural network accuracy