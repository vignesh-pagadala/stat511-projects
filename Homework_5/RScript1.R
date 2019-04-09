library(emmeans)
library(car)
library(ggplot2)

#1
InData <- read.csv("/home/vignesh_pagadala/Desktop/STAT512/Homework 5/CKheart.csv", header = TRUE)
InData

InData$percentHA <- InData$withHA/(InData$withHA + InData$withoutHA)
plot(percentHA ~ CK, data = InData)

Model1 <- glm(cbind(withHA, withoutHA) ~ CK, family = binomial(link = "logit"), data = InData)
summary(Model1)
