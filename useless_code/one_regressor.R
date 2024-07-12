library(rms) 
library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)
library(GGally)
library(nnet)

# Loading dataset, only the columns we are interesed in
stars <- read.csv("Stars.csv",head=TRUE)
stars <- stars[,c("A_M","Type")]

set.seed(123)

# Define the splitting ratio
splitRatio <- 0.8

# Create a randomized index
sampleIndex <- sample(1:nrow(stars), size = splitRatio * nrow(stars))

# Split the data
trainData <- stars[sampleIndex, ]
testData <- stars[-sampleIndex, ]

trainData$Type <- as.factor(trainData$Type)
testData$Type <- as.factor(testData$Type)

model <- multinom(trainData$Type ~ trainData$A_M, data=trainData)
summary(model)
aic <- AIC(model)
bic <- BIC(model)

# Train and Test Error
pred <- predict(model,newdata=trainData)

conf_matrix <- table(pred,trainData$Type)
print(conf_matrix)

training_error <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Training Error Rate:", training_error))
