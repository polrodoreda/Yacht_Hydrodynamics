setwd("~/Documents/Universitat/Màster/Q1/CSI/Machine Learning/Project")

#Read dataset
yacht.data <- read.table(file = "yacht_hydrodynamics.data")
names(yacht.data) <- c('LonPos', 'PrismCoeff', 'LenDisRatio', 'BeamDRatio', 'LenBRatio', 'FroudNum', 'Resistance')