library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
InData <- read.csv("~/Desktop/Course Work/STAT512/Homework_8/Biomass.csv")
str(InData)
InData$Water <- as.factor(InData$Water)

#1
SumStats <- summarize(group_by(InData, Type, Herb, Water),
                      n = n(),
                      Biomass = mean(Biomass),
                      sd = sd(Biomass))
#SumStats
qplot(x = Herb, y = Biomass, colour = Water, group = Water, data = SumStats) +
  geom_line() +
  facet_grid(. ~ Type)
#2
options(contrasts=c("contr.sum","contr.poly"))
Model1 <- lm(Biomass ~ Type*Herb*Water, data = InData)
Anova(Model1, type = 3)
#3
#Based on the plot of Resids vs Fitted, there is strong evidence of unequal variance.
#(Based on the QQplot, there is also some evidence against normality. But this is actually driven by the
#  unequal variance.)
par(mfrow=c(1,2))
plot(Model1, which = c(1:2))
#4
emout1 <- emmeans(Model1, pairwise ~ Water|Herb*Type)
emout1$contrasts
#5
Model2 <- lm(Biomass ~ Herb*Water, data = InData[InData$Type == "Forb",])
Anova(Model2, type =3)
#6
par(mfrow = c(1,2))
plot(Model2, which = c(1:2))
#7
emout2 <- emmeans(Model2, pairwise ~ Water|Herb)
emout2
#8
n = 3 #for each treatment combination mean
qt(0.975, df = 12)*sqrt(2*(0.63/12)/3)
qt(0.975, df = 12)*0.187

#or peeling MSResid out of ANOVA table with R
#numerator is SSResid, denominator is df resid from ANOVA table
MSResid=(Anova(Model2, type = 3)[[1]][5])/(Anova(Model2, type = 3)[[2]][5])
MSResid
qt(.975,12)*sqrt(2*MSResid/n)
#9
emout3 <- emmeans(Model2, pairwise ~ Water)
emout3$contrasts
#10
n=9 #because we are comparing Water across 3 levels of Herb (each with 3)
qt(0.975, df = 12)*sqrt(2*(0.63/12)/9)
qt(0.975, df = 12)*0.108
#or this method using same MSResid from ANOVA
qt(.975,12)*sqrt(2*MSResid/n)
#11
# The power is higher for the main effect comparison (#10) becuase the LSD (ME) is smaller.

#12
Model3 <- lm(Biomass ~ Herb*Water, data = InData[InData$Type == "Grass",])
Anova(Model3, type =3)

#13
par(mfrow = c(1,2))
plot(Model3, which = c(1:2))

#14
emout4 <- emmeans(Model3, pairwise ~ Water|Herb)
emout4$contrasts

#15
# No, because there is evidence of an interaction between Water and Herb. In particular, the estimated
# difference in mean response for Water 1 vs 2 changes sign when comparing across Herb. When there is
# evidence of an interaction, it does not make sense to look at main effects.

#16



#17



#18





