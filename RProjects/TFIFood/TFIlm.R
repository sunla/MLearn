setwd("~/Documents/R/TFIFood/data/")
library(caret)
library(ggplot2)

prod <- read.table("train.csv", sep = "," , header = TRUE)
test <- read.table("test.csv", sep = ",", header = TRUE)

str(prod)
summary(prod)
names(prod)
prod$Id <- NULL
prod$Open.Date <- NULL

correlationmatrix <- cor(prod[,6:37])
correlationmatrix
highlycorrelated <- findCorrelation (correlationmatrix, cutoff = 0.7)
sort(highlycorrelated) #1  4  7  8  9 10 12 13 14 15 16 18 19 20 23 24 25 26 28 30 31 32
formula <- revenue ~ P2 + P3 + P5 + P6 + P11 + P17 + P21 + P22 + P27 + P29 + City.Group + Type

library(gridExtra)

gp1 <- qplot(x= P2, data = prod , binwidth= 1, color = I('black'), fill = I('#099DD9'))
gp3 <- qplot(x= P3, data = prod , color = I('black'), fill = I('#099DD9'))
gp5 <- qplot(x= P5, data = prod , color = I('black'), fill = I('#099DD9'))
gp6 <- qplot(x= P6,  data = prod , color = I('black'), fill = I('#099DD9'))
gp11 <- qplot(x= P11,  data = prod , color = I('black'), fill = I('#099DD9'))
gp17 <- qplot(x= P17, data = prod , color = I('black'), fill = I('#099DD9'))
gp21 <- qplot(x= P21,  data = prod , color = I('black'), fill = I('#099DD9'))
gp22 <- qplot(x= P22,  data = prod , color = I('black'), fill = I('#099DD9'))
gp27 <- qplot(x= P27, data = prod , color = I('black'), fill = I('#099DD9'))
gp29  <- qplot(x= P29, data = prod , color = I('black'), fill = I('#099DD9'))

grid.arrange(gp1,gp3,gp5,gp6,gp11,gp17,gp21,gp22,gp27,gp29, ncol = 3)

lmall <- lm(revenue ~ ., prod)
lmmodel <- lm(formula, prod)
prod$predlog10 <- predict(lmmodel, prod)
prod$pred <- predict(lmall, prod)
lmall$residuals

plot(log10(prod$revenue), log10(prod$pred))
summary(lmmodel)
summary(lmall)

#plotting residuals ####
ggplot(data=prod, aes(x=predlog10, y=predlog10-log10(revenue))) + geom_point(alpha=0.2,color = "black") +
  geom_smooth(aes(x=predlog10, y=predlog10-log10(revenue)), color = "black")

ggplot(data=prod, aes(x=pred, y=pred-revenue)) + geom_point(alpha=0.2,color = "black") +
  geom_smooth(aes(x=pred, y=pred-revenue), color = "black")
