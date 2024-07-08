# Hertzsprung-Russel Diagram

library(MASS)
library(GGally)
library(RColorBrewer)
library(car)
library(plotly)
library(dplyr)


stars <- read.csv("new_stars.csv")
attach(stars)

df <- data.frame(lum = log(stars$L), temp = log(stars$Temperature), radius = log(stars$R), class = stars$Type)

#ggpairs(stars,col=c(1:4),aes(col=Type))

ggplot(data=df,mapping=aes(x=temp, y=lum, color=class))+
         geom_point() + 
         scale_color_brewer(palette="Set1") + # You can change the palette as needed
         labs(title="Hertzsprung-Russell Diagram",
              x="Tempearature",
              y="Luminosity",
              color="Stellar Class")


ggplot(data=df,mapping=aes(x=temp, y=radius, color=class))+
  geom_point() + 
  scale_color_brewer(palette="Set1") + # You can change the palette as needed
  labs(title="Temperature vs Radius",
       x="Temperature",
       y="Radius",
       color="Stellar Class")

# i suggest we remove hypergiants from our analysis
stars_nohyper <- stars[which(stars$Type!="Hyper Giants"),]
df_nohyper = df[df$class!="Hyper Giants",]

ggplot(data=df_nohyper,mapping=aes(x=temp, y=radius, color=class))+
  geom_point() + 
  scale_color_brewer(palette="Set1") + # You can change the palette as needed
  labs(title="Temperature vs Radius",
       x="Temperature",
       y="Radius",
       color="Stellar Class")

# sigma = 5.67*10^-8
# L = 4*pi*R^2*sigma*T^4

mod3 <- lm(L ~ R+Temperature,data=df)
summary(mod3)

# Per il momento, limitiamoci alla main sequence
M <- stars[which(stars$Type=="Main Sequence"),]

pi = 3.141592
sigma = 5.67*10^-8

mod1 <- lm(L ~ R+Temperature,data=M)
summary(mod1)

Rquad = R^2
Tempquart = Temperature^4

mod4 <- lm(L~Rquad:Tempquart,data=M)

mod2 <- lm(L~I(R^2):I(Temperature^4)+0,data=M)
summary(mod2)

mod <- lm(L ~ (R^2)*(Temperature^4)+0,data = M)
summary(mod)

plot(mod$fitted.values,mod$residuals)

shapiro.test(mod$residuals)
par(mfrow=c(1,2))
plot(mod$fitted,mod$residuals)
qqnorm(mod$residuals)
qqline(mod$residuals,col='red')

M <- M %>%
  mutate(fitted = fitted(mod),
         R_squared = R^2,
         Temp_fourth = Temperature^4)

R_seq <- seq(min(M$R), max(M$R), length.out = 100)
Temp_seq <- seq(min(M$Temperature), max(M$Temperature), length.out = 100)
grid <- expand.grid(R = R_seq, Temperature = Temp_seq)

# Add transformation columns
grid <- grid %>%
  mutate(R_squared = R^2,
         Temp_fourth = Temperature^4,
         fitted = predict(mod, newdata = grid))

# Create the 3D plot
plot_ly() %>%
  add_trace(data = M, x = ~R^2, y = ~Temperature^4, z = ~L, type = 'scatter3d', mode = 'markers', 
            marker = list(color = 'blue', size = 5), name = 'Real Values') %>%
  add_trace(x = ~grid$R_squared, y = ~grid$Temp_fourth, z = ~grid$fitted, type = 'mesh3d', 
            opacity = 0.5, name = 'Fitted Surface') %>%
  layout(title="3D Plot of Luminosity vs Temp and R")
