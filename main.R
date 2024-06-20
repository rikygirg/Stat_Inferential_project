library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)
library(GGally)

data = read.csv("Stars.csv")
str(data)

df <- data.frame(data["R"],data["L"])
plot(df, xlab = "Radius", ylab = "Luminosity", main = "Luminosity vs Radius")

# Maybe two categories?
idx = which(data["R"]<250)

ggpairs(data,columns = 1:4)
