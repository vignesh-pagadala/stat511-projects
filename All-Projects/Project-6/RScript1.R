library(dplyr)
library(ggplot2)
library(car)
library(emmeans)

berries <- read.csv("~/Desktop/Course Work/STAT512/Homework_6/Irrigation.csv")
str(berries)
berries$Farm <- as.factor(berries$Farm)

## ---- 1 ----##

#A.
SumStats <- summarize(group_by(berries, Method),
                      n = n(),
                      mean = round(mean(Weight),1),
                      sd = round(sd(Weight),3),
                      se = round(sd/sqrt(n),3))
SumStats

#B.
ggplot(SumStats, aes(x = Method, y = mean)) +
  geom_bar(stat = "identity") + geom_errorbar(aes(ymin = mean-se, ymax=mean+se), width = 0.2)

#C.
Model1 <- lm(Weight ~ Method + Farm, data = berries)
par(mfrow=c(1,2))
plot(Model1, which = c(1,2))

#D.
Anova(Model1, type = 3)
anova(Model1)

#E.
# We can conclude there is a difference between means for the methods.
# F = 61.28, p-value < 0.0001.

#F.
#We can conclude that the blocking was effective.
#F = 4.29, p-value = 0.0008.

#G.
emout <- emmeans(Model1, pairwise ~ Method)
emout
CLD(emout)

#H.

# The simple means and lsmeans are the same for this analysis due to balance (no missing data).
# Even with balance, the simple and model based SE’s will not be the same.
# Note: A “model-based” SE assuming a common variance and accounting for blocking is returned by emmeans:
#  SE = sigma/sqrt(n) = sqrt(MSResid)/sqrt(n) = sqrt(61867/36)/sqrt(10) = 13.109

#I.

# dfResid is higher for the one-way ANOVA (45 vs 36). MSResid is higher for the one-way ANOVA (2848 vs 1719).

Model2 <- lm(Weight ~ Method, data = berries)
anova(Model2)

## ---- 2 ---- ##
Grass <- read.csv("~/Desktop/Course Work/STAT512/Homework_6/GrassMiss.csv")
str(Grass)
Grass$Block <- as.factor(Grass$Block)

#A.
aggregate(Y ~ Trt, data = Grass, FUN = mean)

#B.
options(contrasts=c("contr.treatment","contr.poly"))
Model1 <- lm(Y ~ Trt + Block, data = Grass)
Anova(Model1, type = 3)

#C.
# The emmeans for the control and for N50wP are different because each of those treatment groups have a 
# missing value.
emm_options(opt.digits = F)
emmeans(Model1, ~ Trt)

#D.
#No, simple means and emmeans are the NOT same for this analysis due to missing data.
emmeans(Model1, pairwise ~ Trt)$contrasts

#E.
# Hand Calculate
# Block 3, N50wP:
#  2.428= 2.02059 + 0.394 + 0.01365 Block 5, Ctrl:
#  2.0882 = 2.02059 + 0 + 0.06765
#summary(Model1)
coeffs <- summary(Model1)$coeff
#emmean TrtN50wP Block 3
coeffs[1]+coeffs[5]+coeffs[7]

coeffs[1]+coeffs[9]

#Or predict function
Temp <- data.frame(Grass, Yhat = predict(Model1, newdata = Grass))
#Temp

#F.

# LSmean for N50wP Hand Calculate 2.4476 = (2.414588 + 2.410588 + 2.428235 + 2.502588 + 2.482235)/5
aggregate(Yhat ~ Trt, data = Temp, FUN = mean)[5,2]
