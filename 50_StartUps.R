Startups <- read.csv("D:/Study Material/DataScience/MultiLinear Regression/50_Startups.csv")
View(Startups)
attach(Startups)
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot
mydata<- Startups[1:10,c(1:3,5)]
View(mydata)
summary(mydata) # 5 point summary

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
model.startup <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)
#model.car <- lm(MPG~VOL+HP+SP+WT,data=Cars)
summary(model.startup)

# Prediction based on only Volume 
model.startupV<-lm(Profit~Administration)
summary(model.startupV) # Volume became significant

# Prediction based on only Weight
model.startupMS<-lm(Profit~Marketing.Spend)
summary(model.startupMS) # Weight became significant

# Prediction based on Volume and Weight
model.startupVMS<-lm(Profit~Administration+Marketing.Spend)
summary(model.startupVMS) # Both became Insignificant

# So there exists a collinearity problem b/n volume and weight
### Scatter plot matrix along with Correlation Coefficients

library(psych)
pairs.panels(mydata)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.startup)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.startup,id.n=3) # index plots for infuence measures
influencePlot(model.startup,id.n=3) # A user friendly representation of the above





## Variance Inflation factor to check collinearity b/n variables 
vif(model.startup)
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.startup,id.n=2,id.cex=0.7)

#avPlots(finalmodel,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "wt" variable

## Final model
finalmodel<-lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model.startup,id.n = 5)
qqPlot(finalmodel,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

