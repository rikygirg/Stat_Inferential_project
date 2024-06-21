# Basic logistic regression using only the numerical features of the dataset, 
# namely Temperature,Radius,Luminosity and Absolute Magnitude

# Importing libraries
library(rms) 
library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)
library(GGally)
library(nnet)

# Loading dataset, only the columns we are interesed in
stars <- read.csv("Stars.csv",head=TRUE)
stars <- stars[,c("Temperature","L","R","A_M","Type")]

# Visualization of the features, there are some visible clusters
ggpairs(stars,col=1:4)

# First very basic idea, use all the numerical features as predictors.
# Let's fit a multinomial logistic regression to the data

stars$Type <- as.factor(stars$Type)

model <- multinom(Type ~ stars$Temperature + stars$L + stars$R + stars$A_M, data=stars)
summary(model)

# Goodness of Fit
aic <- AIC(model)
bic <- BIC(model)

# Train and Test Error
pred <- predict(model,newdata=stars)

conf_matrix <- table(pred,stars$Type)
print(conf_matrix)

training_error <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Training Error Rate:", training_error))

