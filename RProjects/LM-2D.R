# example 7.1 of section 7.1.1 
# (example 7.1 of section 7.1.1)  : Linear and logistic regression : Using linear regression : Understanding linear regression 
# Title: Loading the PUMS data 
library(ggplot2)
psub <- read.csv("sat.csv")
dtrain <- subset(psub,univ_GPA >= 3)
dtest <- subset(psub,univ_GPA < 3)
summary(psub)
model <- lm(log(univ_GPA,base=10) ~ high_GPA,data=psub)
model1 <- lm(univ_GPA ~ high_GPA,data=psub)
summary(model1)
plot(psub$high_GPA,psub$univ_GPA)
abline(coef(model1))
plot(fitted(model1), resid(model1))
cor(psub$univ_GPA,psub$high_GPA)
dtest$predLogPINCP <- predict(model,newdata=dtest)
dtrain$predLogPINCP <- predict(model,newdata=dtrain)

ggplot(data=psub,aes(x=high_GPA+match_SAT,y=univ_GPA))) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(method = "lm") +
  geom_line(aes(x=log(PINCP,base=10),
                y=log(PINCP,base=10)),color="blue",linetype=2) +
  scale_x_continuous(limits=c(4,5)) +
  scale_y_continuous(limits=c(3.5,5.5))

ggplot(data=psub,aes(x=high_GPA+math_SAT,y=univ_GPA)) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(method = "lm")

ggplot(psub, aes(x=high_GPA, y=univ_GPA)) + geom_point() + geom_smooth(method="lm") + labs(x="High GPA", y="Univ GPA")

ggplot(data=dtest,aes(x=predLogPINCP,
                      y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=predLogPINCP,
                  y=predLogPINCP-log(PINCP,base=10)),
              color="black")

dtest[1:3,"PINCP"]
rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
rsq(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rsq(log(dtest$PINCP,base=10),predict(model,newdata=dtest))

rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }
rmse(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rmse(log(dtest$PINCP,base=10),predict(model,newdata=dtest))

coefficients(model)
summary(model)
summary(model)$coefficients