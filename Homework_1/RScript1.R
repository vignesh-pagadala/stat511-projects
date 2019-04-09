InData <- read.csv("/home/vignesh_pagadala/Desktop/STAT512/Homework 1/OTT_Final/ASCII-comma/CH12/ex12-53.txt", quote = " ' ", row.names = 1)
InData
# This is a test comment.

# Q1. Pairwise Scatterplot
pairs(InData, gap = 0, pch = "o")

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
