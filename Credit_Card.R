
View(bank)
attach(bank)

creditcard <- read.csv("D:/Study Material/DataScience/logistic Regression/creditcard.csv")
View(creditcard)
sum(is.na(creditcard))
attach(creditcard)
contrasts(factor(creditcard$card))
# Linear regression technique can not be employed
# Logistic Regression 
str(creditcard)
logit<-glm(factor(card)~reports+age+income+share+expenditure+factor(owner)+factor(selfemp)+dependents+months+majorcards+active,family=binomial,data = creditcard)
summary(logit)

logit1<-glm(factor(card)~reports+age+income+share+expenditure+factor(owner)+factor(selfemp)+dependents+months+majorcards+active,family=binomial,data = creditcard)
summary(logit1)
exp(coef(logit1))
table(creditcard$card)

# Confusion matrix table 
prob <- predict(logit1,type=c("response"),creditcard)
prob
confusion<-table(prob>0.5,creditcard$card)
probo <- prob>0.5
table(probo)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
