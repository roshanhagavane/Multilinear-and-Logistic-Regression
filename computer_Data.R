Computer_Data <- read.csv("D:/Study Material/DataScience/MultiLinear Regression/Computer_Data.csv")
View(Computer_Data)
attach(Computer_Data)
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot

summary(Computer_Data)

mydata<- Computer_Data[1:6259,c(1:6,10,11)]# 5 point summary
View(mydata)
# 7. Find the correlation b/n Output (MPG) & (HP,VOL,SP)-Scatter plot
pairs(mydata)
plot(mydata)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(mydata)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(mydata))


# The Linear Model of interest
model.comdata <- lm(price~speed+hd+ram+screen+ads+trend,data=mydata)
summary(model.comdata)




# So there exists a collinearity problem b/n volume and weight
### Scatter plot matrix along with Correlation Coefficients

library(psych)
pairs.panels(mydata)



# Evaluate model LINE assumptions 
plot(model.comdata)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model.comdata,id.n = 5)
qqPlot(model.comdata,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

