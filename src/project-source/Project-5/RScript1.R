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

Table1 <- with( table(race, low), data = BirthData)
prop.table(Table1, 1)

Table2 <- with( table(smoke, low), data = BirthData)
prop.table(Table2, 1)
chisq.test(Table2)

Model1 <- glm(low ~ smoke, family=binomial, data = BirthData)
library(emmeans)
emmeans(Model1, ~ smoke, type = "response")

library(MuMIn)
library(car)

FullModel <- glm(low ~ ., family=binomial, data = BirthData)
options(na.action = "na.fail")
dredge(FullModel, rank="AIC")

Model2 <- glm(low ~ mwt + race + smoke, family = binomial, data = BirthData)
summary(Model2)

Anova(Model2, type = 3)

exp(Model2$coef)
exp(confint(Model2))

emmeans(Model2, pairwise ~ smoke, type = "response")

library(ResourceSelection)
hoslem.test(Model2$y, fitted(Model2), g = 10)
