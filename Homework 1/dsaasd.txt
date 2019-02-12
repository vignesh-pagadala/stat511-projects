PROTEIN = c(10, 10, 10, 10, 10, 10, 15, 15, 15, 15, 15, 15, 20, 20, 20, 20, 20, 20) 
ANTIBIO = c(1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2) 
SUPPLEM = c(3,5,7,3,5,7,3,5,7,3,5,7,3,5,7,3,5,7) 
TIME = c(88,82,81,82,83,75,80,80,75,77,76,72,79,74,75,74,70,69)
InData = data.frame(PROTEIN, ANTIBIO, SUPPLEM, TIME) 
InData

library(car)
complete_model <- lm(TIME ~ PROTEIN + ANTIBIO + SUPPLEM, data=InData)
complete_model

c1 <- c(0, 0,0,1)
lht(complete_model, c1, rhs = c(0))
