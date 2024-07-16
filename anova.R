library(MASS)
library(GGally)
library(RColorBrewer)
library(car)

stars <- read.csv("new_stars.csv")
attach(stars)

my_colors = brewer.pal(6, 'Set2')
type_order = c("White Dwarf","Red Dwarf","Brown Dwarf","Main Sequence","Super Giants","Hyper Giants")

df <- data.frame(lum = log(stars$L), temp = log(stars$Temperature), radius = log(stars$R), class = stars$Type)
df$class <- factor(df$class,type_order)

boxplot(lum ~ class, data = df, col=my_colors, main="Log Luminosity per Class")
abline(h=mean(log(stars$R)))

# struttura delle classi
tapply(df$lum,df$class,length)
tapply(df$lum,df$class,mean)

# Ipotesi di validita' ANOVA
Ps <- tapply(df$lum, df$class, function(x) (shapiro.test(x)$p)) #normalita' un po' dubbia
Ps

leveneTest(df$radius, df$class)
bartlett.test(df$radius, df$class)

# Sia Levene sia bartlett rifiutano omoschedasticita' tra gruppi

# Welch ANOVA test rifiuta l'ipotesi che le medie della luminosita' sia uguale tra gruppi
oneway.test(df$lum ~ df$class, var.equal = FALSE)
