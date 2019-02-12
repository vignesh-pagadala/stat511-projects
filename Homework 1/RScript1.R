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
