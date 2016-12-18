setwd("~/Documents/Universitat/Màster/Q1/CSI/Machine Learning/Project")

#Read dataset
yacht.data <- read.table(file = "yacht_hydrodynamics.data")
names(yacht.data) <- c('LonPos', 'PrismCoeff', 'LenDisRatio', 'BeamDRatio', 'LenBRatio', 'FroudNum', 'Resistance')
N <- length(yacht.data)

#Adding train column
set.seed(1)
yacht.data["train"] <- NA
yacht.data$train <- sample(c('T', 'V'), size = N, replace = TRUE, prob = c(0.7, 0.3))

#Train and test data
train.data <- yacht.data[yacht.data$train == 'T',]
validation.data <- yacht.data[yacht.data$train == 'V',]

library(randomForest)

rf <- randomForest(train.data$Resistance ~ ., data = train.data[1:6], ntree = 150, proximity = FALSE)

print(rf)

plot(rf)
legend("topright", legend=c("OOB"),    
       pch=c(1), col=c("black"))

#Importance of variables
importance(rf)
varImpPlot(rf)

varUsed(rf)
pred <- predict(rf, newdata = validation.data)
RFtable <- table(Truth = validation.data$Resistance, Pred = pred)
(test.error <- 100*(1-sum(diag(RFtable))/length(validation.data)))
