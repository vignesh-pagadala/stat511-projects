#Regression - Corn Example
#(Simple linear) regression is used to model the linear relationship between 
#a numerical response variable and a single numerical predictor. 
#In this example, corn yield is the response and fertilizer (X) is the predictor.

Corn <- read.csv("~/Dropbox/STAT512/Lectures/R_Stuff/Corn.csv")
Corn <- read.csv("C:/Users/sharp/Dropbox/STAT512/Lectures/R_Stuff/Corn.csv")

str(Corn)
#Scatterplot
plot(Yield ~ X, data = Corn)
#Overlay fitted regression line
abline(lm(Yield ~ X, data = Corn))
#Regression
Fit <- lm(Yield ~ X, data = Corn)
Fit
summary(Fit)
#Confidence Intervals
confint(Fit, level = 0.95)
#Diagnostic plots
plot(Fit)

