library(emmeans)
library(car)
library(ggplot2)

InData <- read.csv("/home/vignesh_pagadala/Desktop/Course Work/STAT512/Homework_5/CKheart.csv", header = TRUE)
InData
CKheart <- InData
Model1 <- glm(cbind(withHA, withoutHA) ~ CK, family = binomial(link = "logit"), data = InData)
summary(Model1)

CKheart$rate <- CKheart$withHA/(CKheart$withHA + CKheart$withoutHA)
NewData <- seq(10, 500, 1)
phat <- predict(Model1, list(CK = NewData), type = "response")
plot(rate ~ CK, data = CKheart)
lines(phat ~ NewData) 

exp(Model1$coef)
exp(confint(Model1))

NullModel <- glm(cbind(withHA, withoutHA) ~ 1, family = binomial(link = "logit"), data = CKheart)
1-logLik(Model1)/logLik(NullModel)

library(MASS)
probs <- seq(0.1, 0.9, 0.05)
ld <- dose.p(Model1, cf = 1:2, p = probs)
ld

BirthData <- read.csv("/home/vignesh_pagadala/Desktop/Course Work/STAT512/Homework_5/birthweight.csv")
str(BirthData)

BirthData$race <- as.factor(BirthData$race)
BirthData$smoke <- as.factor(BirthData$smoke)
str(BirthData)

