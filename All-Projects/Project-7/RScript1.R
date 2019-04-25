library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
PCBdata <- read.csv("~/Desktop/Course Work/STAT512/Homework_7/PCB.csv")
str(PCBdata)
PCBdata$species <- as.factor(PCBdata$species)

#### 1 ####
#A.
SumStats <- summarize(group_by(PCBdata, sex, species),
                      n = n(),
                      mean = mean(pcb),
                      sd = sd(pcb))
SumStats

qplot(x = sex, y = mean, group = species, color = species, data = SumStats) +
  geom_line()

#B.
options(contrasts=c("contr.sum","contr.poly"))
Model1 <- lm(pcb ~ group, data = PCBdata)
Anova(Model1, type = 3)

#C.
emmeans(Model1, pairwise ~ group)


#D.
Model2 <- lm(pcb ~ sex*species, data = PCBdata)
Anova(Model2, type =3)

#E.
emmeans(Model2, pairwise ~ sex*species)

#F.
emmeans(Model2, pairwise ~ species)

#### 2 ####
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
options(contrasts=c("contr.sum", "contr.poly"))
InData <- read.csv("~/Desktop/Course Work/STAT512/Homework_7/Ex15-14.csv", header = TRUE)
str(InData)
InData$Roadway <- as.factor(InData$Roadway)

#A.
# Blocking structure: Randomized Complete Bock.
# Treatment Structure: 2-way factorial
# Concentration (High,Low) by Treatment (CaCL, NaCl, Sand)

#B.
SumStats <- summarize(group_by(InData, Treatment, Concentration),
                      n
                      = n(),
                      mean = mean(cracks),
                      sd
                      = sd(cracks))
SumStats
qplot(x = Treatment, y = mean, colour = Concentration, group = Concentration, data = SumStats) + geom_line()

#C.
RoadModel <- lm(cracks ~ Roadway + Treatment*Concentration, data = InData)
par(mfrow = c(1,2))
plot(RoadModel, which =1);plot(RoadModel, which =2)
Anova(RoadModel, type = 3)

#D.
# Blocking was effective (F = 64.462, p < 0.001)

#E.
emmeans(RoadModel, pairwise ~ Concentration|Treatment)
# Average number of cracks for High is significantly higher than Low for each of the 3 Treatments.

#F.
emmeans(RoadModel, pairwise ~ Treatment|Concentration)

# Discussion: At Low concentration, average number of cracks for Sand is significantly lower than both CaCl
# and NaCl. There is not a statistically significant difference between CaCl and NaCl.


