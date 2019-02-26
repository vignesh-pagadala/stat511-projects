#Load the dplyr and emmeans packages
#Remember packages need to be installed before first use!
library(dplyr)
library(emmeans)

#Two approaches to importing the data
Rice <- read.csv("~/Dropbox/STAT512/Lectures/R_Stuff/Rice.csv")
Rice <- read.csv("")
Rice <- read.csv(file.choose())
str(Rice)
boxplot(weight ~ trt, data = Rice, main = "Boxplots")

#Use dplyr package to calculate summary statistics by trt.
#First  we use the group_by function.
#Resulting object is a "tibble".
RiceGrpd <- group_by(Rice, trt)
RiceGrpd
#Then pass the grouped data to the summarise function.
#Summary statistics are automatically computed "by group".
SumStats <- summarise(RiceGrpd, 
                      n = n(),
                      mean = mean(weight), 
                      sd = sd(weight),
                      SE = sd/sqrt(n))
SumStats
#In practice, we can combine the two steps.
SumStats <- summarise(group_by(Rice, trt), 
                      n = n(),
                      mean = mean(weight), 
                      sd = sd(weight),
                      SE = sd/sqrt(n))

#One-way ANOVA
#Note: trt should be defined as.factor! See str() output above.
Fit <- lm(weight ~ trt, data = Rice)
#summary() output is not of direct interest here. 
summary(Fit)
#anova() output is typically of more interest in ANOVA settings.
anova(Fit)
#Diagnostic plots
par(mfrow = c(2, 2))
plot(Fit)
#Use emmeans package to get pairwise comparisons with Tukey adjustment.
emmeans(Fit, pairwise ~ trt)
