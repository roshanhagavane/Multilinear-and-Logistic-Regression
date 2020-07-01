bank <- read.csv("D:/Study Material/DataScience/logistic Regression/bank-full.csv", sep=";")
 View(bank)
attach(bank)


sum(is.na(bank))
contrasts(factor(bank$marital))
  # Linear regression technique can not be employed
# Logistic Regression 
str(bank)
logit<-glm(factor(y)~factor(education)+factor(marital)+factor(default)+factor(housing)+factor(loan)+factor(month)+age+balance+day+duration+pdays+previous,family=binomial,data = bank)
summary(logit)

logit1<-glm(factor(y)~factor(education)+factor(marital)+factor(default)+factor(housing)+factor(loan)+factor(month)+age+balance+day+duration+pdays+previous,family=binomial,data = bank)

summary(logit1)

exp(coef(logit1))
table(bank$y)

# Confusion matrix table 
prob <- predict(logit1,type=c("response"),bank)
prob
confusion<-table(prob>0.5,bank$y)
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
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
