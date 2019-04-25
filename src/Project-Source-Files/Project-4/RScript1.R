























library(emmeans)
library(car)
#1
library(ggplot2)
InData <- read.csv("/home/vignesh_pagadala/Desktop/STAT512/Homework 4/ex16-23.csv", header = TRUE)
p <- qplot(FisherAlpha, Ppt, shape = factor(Age), color = factor(Age), data = InData)
p + geom_smooth(method = lm, se = FALSE)

#2
int_model <- lm(FisherAlpha ~ Age*Ppt, data = InData)
ancovamodel <- Anova(int_model, type = 3)
ancovamodel

#3 
plot(int_model)

#4
InData$Age.f <- factor(InData$Age)
levels(InData$Age.f)

Model3 <- lm(FisherAlpha ~ Age.f*Ppt, data = InData)
summary(Model3)

#5
emmeans(Model3, pairwise ~ Age.f)

#6
emmeans(Model3, pairwise ~ Age.f, Ppt = 2500)
emmeans(Model3, pairwise ~ Age.f, Ppt = 3000)

#7
n_model <- lm(FisherAlpha ~ Age + Ppt, data = InData)
nancovamodel <- Anova(n_model, type = 3)

ancovamodel





#1
InData <- read.csv("/home/vignesh_pagadala/Desktop/STAT512/Homework 3/Grain.csv", header = TRUE)
InData
library(ggplot2)
qplot(Days, Yield, data = InData)
#2
model <- lm(Yield ~ Days, data=InData)
summary(model)
plot(model)
#3
quad_model <- lm(Yield ~ Days + I(Days^2), data = InData)
quad_model
summary(quad_model)
plot(quad_model)
#4
cubic_model <- lm(Yield ~ Days + I(Days^2) + I(Days^3), data=InData)
summary(cubic_model)
plot(cubic_model)
#5
library(car)
c1 <- matrix(c(0, 1, 0, 0,
               0, 0, 1, 0,
               0, 0, 0, 1), nrow = 3, byrow = TRUE)
lht(cubic_model, c1, rhs = c(0, 0, 0))
#6
#7
NewData <- read.csv("/home/vignesh_pagadala/Desktop/STAT512/Homework 3/DrugTest.csv", header = TRUE)
NewData
boxplot(PreTreatment ~ Drug, data = NewData, main = "PreTreatment vs Drug")
boxplot(PostTreatment ~ Drug, data = NewData, main = "PostTreatment vs Drug")
p <- qplot(PreTreatment, PostTreatment, shape = factor(Drug), color = factor(Drug), data = NewData)
p + geom_smooth(method = lm, se = FALSE)
#8
drugmodel <- lm(PostTreatment ~ Drug, data = NewData)
onewayanova <- anova(drugmodel)
onewayanova
library(emmeans)
emmeans(drugmodel, pairwise ~ Drug)
#9 
sdrug_model <- lm(PostTreatment ~ Drug + PreTreatment, data = NewData)
ancovamodel <- Anova(sdrug_model, type = 3)
ancovamodel
emmeans(sdrug_model, pairwise ~ Drug + PreTreatment)
#10
#11
newdrug_model <- lm(I(PostTreatment - PreTreatment) ~ Drug, data = NewData)
owanova <- anova(newdrug_model)
owanova
emmeans(newdrug_model, pairwise ~ Drug)
# -----------------------------------------------

pairs(InData, gap = 0, pch = "o")
plot(Days, Yield)
# Q2. Full Model
full_model <- lm(BodyFat ~ Triceps + Thigh + Midarm, data=InData)
summary(full_model)

# Q3. LHT - NULL test
library(car)
c1 <- matrix(c(0, 1, 0, 0,
               0, 0, 1, 0,
               0, 0, 0, 1), nrow = 3, byrow = TRUE)
lht(full_model, c1, rhs = c(0, 0, 0))

# Q4. B1 NULL test
c1 <- c(0, 1, 0, 0)
lht(full_model, c1, rhs = c(2.0))

# Q5. B2, B3 NULL test
c1 <- matrix(c(0, 0, 1, 0,
               0, 0, 0, 1), nrow = 2, byrow = TRUE)
lht(full_model, c1, rhs = c(0, 0))

# Q6. Eliminating predictors
temp_model <- lm(BodyFat ~ Triceps + Midarm, data=InData)
summary(temp_model)

plot(temp_model)


nd <- data.frame(Triceps = 20, Midarm = 25)
predict(temp_model, nd, interval = "confidence")

predict(temp_model, nd, interval = "prediction")

outlierTest(temp_model)
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

# Q2. Pearson Correlation Coefficient
cor(InData, method = c('pearson'))

# Q3. Linear regression between time and every other variable.

# Q3.(a) Time as a response to Protein
protein_model <- lm(TIME ~ PROTEIN, data = InData)
protein_model
summary(protein_model)

# Q3.(b) Time as a response to Antibio
antibio_model <- lm(TIME ~ ANTIBIO, data = InData)
antibio_model
summary(antibio_model)

# Q3.(c) Time as a response to Supplem
supplem_model <- lm(TIME ~ SUPPLEM, data = InData)
supplem_model
summary(supplem_model)

# Q4. Multiple regression
complete_model <- lm(TIME ~ PROTEIN + ANTIBIO + SUPPLEM, data=InData)
complete_model
summary(complete_model)




# Q5. Residuals versus Fitted Values
plot(complete_model)

# Q9. Hypothesis Test
library(car)
c1 <- c(1, 0, 0, 0)
lht(complete_model, c1, rhs = c(0))
