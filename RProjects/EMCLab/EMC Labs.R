### EMC - K-means clustering ####

stateincome <- read.csv("income.csv", sep = ',', header = T)
income <- as.matrix(stateincome$income)
summary(income)
income <- sort(income)
summary(income)
head(income)
km <- kmeans(income,5,15)
km
plot(income,col = km$cluster)
points(km$centers, col = 1:3, pch = 8)
wss <- numeric(15)
for (i in 1:15) wss[i] <- sum(kmeans(income,centers = i)$withinss)
plot(1:15,wss,type="b", xlab="Number of clusters", ylab="Within groups sum of squares")

### EMC - Apriori ####
require(arules)
data(Groceries)
head(Groceries)
setwd("/Users/rajeshmalpani/workspace/Data Science/EMC/")
#arules::write(Groceries, file = "MBAdata.csv",  format="single", quote=TRUE, sep = ",", col.names = NA)
Groceries
Groceries@itemInfo
Groceries@data@i[,select = ]Groceries@data@i == 27)
summary(Groceries@itemInfo)
summary(Groceries@transactionInfo)

#table(Groceries@data@i == 25)

image(Groceries)
rules <- apriori(Groceries,parameter=list(sup=0.001,conf=0.5,target="rules"))
rules
inspect(rules)

subrules <- rules[quality(rules)$confidence > 0.8]
subrules
inspect(subrules)


rules_high_lift <- head(sort(rules, by="lift"), 3) 
inspect(rules_high_lift)

### EMC - Linear ####

x <- runif(100, 0, 10)
y <- 5 + 6*x + rnorm(100)   
plot(x,y)
d <- lm(y ~ x)
str(d)
summary(d)

print(d)
par(mfrow=c(2,2)) 
plot(d)
d$fitted.values

ypred <- predict(d)
par(mfrow=c(1,1)) 
plot(y,y, type="l", xlab="true y", ylab="predicted y") 
points(y, ypred)
y[50]; ypred[50];

d1 <- summary(d)
d1
cat("OLS gave slope of ", d1$coefficients[2,1], "and an R-sqr of ", d1$r.squared, "\n")


x1 <- runif(100) 
y1 = 5 + 6*x1 + 0.1*x1*x1 + rnorm(100)
m <- lm(y1 ~ x1)

x2 <- runif(100) 
y2 = 5 + 6*x2 + 0.1*x2*x2 + rnorm(100)

y2pred <- predict(m,data.frame(x2))
plot(y2,y2, type="l", xlab="true y", ylab="predicted y") 
points(y2, y2pred)

summary(d); summary(m)

### EMC - Logistic #### 

turnout <- read.table("turnout.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
summary(turnout)
head(turnout)
turnout$X <- NULL

dim(turnout)
dim(turnout[turnout$educate>0, ])
# cleanup educate = 0

turnout <- turnout[turnout$educate>0, ]
dim(turnout)
summary(turnout)
unique(sort(turnout$educate))

turnout$price <- cut(turnout$educate, breaks = c(0, 6,13,19), labels = c("10","20","30"))
summary(turnout)

table(turnout$vote) 
with(turnout, table(price,vote)) 
unique(turnout$price) 

turnout$price <- as.numeric(turnout$price)
cor.mat <- cor(turnout[,c("age","educate", "income", "price")]) 
cor.mat

head(turnout[, as.numeric(price)])
head(as.factor(turnout$price))

mylogit <- glm(vote ~ income + age + as.factor(price), data=turnout,family=binomial(link="logit"), na.action=na.pass)
summary(mylogit)
str(mylogit)
confint(mylogit)
exp(mylogit$coefficients)
plot(mylogit)

turnout$pricefactor = relevel(as.factor(turnout$price), "30")
mylogit2 = glm(vote ~ income + age + pricefactor , data= turnout,family=binomial(link="logit"), 
               na.action=na.pass) 
summary(mylogit2)
require(ROCR)

pred = predict(mylogit, type="response")
predObj = prediction(pred, turnout$vote)

rocObj = performance(predObj, measure="tpr", x.measure="fpr") # creates ROC curve obj 
aucObj = performance(predObj, measure="auc") # auc object
auc = aucObj@y.values[[1]]
plot(rocObj, main = paste("Area under the curve:", auc))

turnout$pred <- predict(mylogit,newdata=turnout, type='response')

table(truth=turnout$vote,prediction=turnout$pred>0.5)

price <- c(10,20,30) 
age <- c(mean(turnout$age)) 
income <- c(mean(turnout$income)) 
newdata1 <- data.frame(income,age,price) 
newdata1
newdata1$PurchaseP <- predict (mylogit,newdata=newdata1,type="response")
newdata1
plot(mylogit)

newdata2 <- data.frame(age=seq(min(turnout$age),max(turnout$age),2), income=mean(turnout$income),price=30)
newdata2$AgeP<-predict(mylogit,newdata=newdata2,type="response") 
newdata2
cbind(newdata2$age,newdata2$AgeP)
plot(newdata2$age,newdata2$AgeP)
ggplot(newdata2, aes(x=age, y=AgeP)) + geom_point() + geom_smooth(method="glm")

newdata3 <- data.frame(age=mean(turnout$age), income=seq(20, 90, 10),price=30)
newdata3$IncomeP<-predict(mylogit,newdata=newdata3,type="response") 
newdata3
cbind(newdata3$income,newdata3$IncomeP)
plot(newdata3$income,newdata3$IncomeP)
ggplot(newdata3, aes(x=income, y=IncomeP)) + geom_point() + geom_smooth(method="glm")



newdata4 <- data.frame ( age= round(runif(10,min(turnout$age),max(turnout$age))),
                           income=round(runif(10,min(turnout$income),max(turnout$income))), 
                         price = round((runif(10,10,30)/10))*10) 
newdata4$Prob <- predict(mylogit,newdata=newdata4,type="response") 
newdata4

### EMC - Naive ####


library("e1071")
sample <- read.table("sample1.csv",header=TRUE,sep=",")
traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,])
traindata
testdata
tprior <- table(traindata$Enrolls)
tprior <- tprior/sum(tprior)
tprior
ageCounts <-table(traindata[,c("Enrolls", "Age")])
ageCounts
ageCounts <- ageCounts/rowSums(ageCounts)
incomeCounts <- table(traindata[,c("Enrolls", "Income")])
incomeCounts <- incomeCounts/rowSums(incomeCounts)
jsCounts <- table(traindata[,c("Enrolls", "Jobsatisfaction")])
jsCounts<-jsCounts/rowSums(jsCounts)
desireCounts <- table(traindata[,c("Enrolls", "Desire")])
desireCounts <- desireCounts/rowSums(desireCounts)

pyes <- ageCounts["Yes","<=30"]*
  incomeCounts["Yes","Medium"]*
  jsCounts["Yes","Yes"]*
  desireCounts["Yes","Fair"]*
  tprior["Yes"]

pno <- ageCounts["No","<=30"]*
  incomeCounts["No","Medium"]*
  jsCounts["No","Yes"]*
  desireCounts["No","Fair"]*
  tprior["No"]

pyes
pno
max(pyes,pno)

model <- naiveBayes(Enrolls ~.,traindata)
model

results <- predict (model,testdata)
results

### EMC - Decision Tree #### 

# Classification Tree example
require(rpart); require(rpart.plot)
setwd("/Users/rajeshmalpani/workspace/Data Science/EMC/data")
play_decision <- read.table("golf.csv",header=TRUE,sep=",")
play_decision
names(play_decision) <- c("Outlook", "Temperature", "Humidity", "Wind", "Play")
summary(play_decision)

fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind, method="class", data=play_decision, control=rpart.control(minsplit=1))
summary(fit)
rpart.plot(fit, type=4, extra=1)
fit$variable.importance
fit$
newdata <- data.frame(Outlook="rain",Temperature=73,Humidity=82,Wind=FALSE); newdata
predict(fit,newdata=newdata,type="prob")
predict(fit,newdata=newdata,type="class")
predict(fit,newdata=newdata,type="vector")

# Try a Regression Tree for Bike data ####

### EMC - Time Series #### 

msales <- read.table("sales.csv", header = TRUE, sep = ",")
head(msales)
names(msales) <- c("date", "sales")

year <- substr(msales$date, 0, 4)
msales <- cbind(msales, year)

month <- substr(msales$date, 6, 7)
msales <- cbind(msales, month)

summary(msales)
min(msales$year); max(msales$year)

msales <- msales[order(year, month), ]
dim(msales)

asales <- c(rep(0,77))
sales <- c(rep(0,72))
csales <- c(rep(0,5))

asales[1:77] <- msales[1:77,2]
sales[1:72] <- msales[1:72,2] 
csales[1:5] <- msales[73:77,2]

asales <- ts(asales, start=1965, frequency=12)
sales <- ts(sales,start=1965,frequency=12)
csales <- ts(csales,start=1971,frequency=12)
plot(sales,type="l")

summary(sales); sales

par(mfrow=c(1,1)) 
acf(sales) 
pacf(sales)
mean(sales)
var(sales)


> #Difference the series and plot it
sales1 <- diff(sales) 
m <- length(sales1) 
par(mfrow=c(1,1)) 
plot(1:m,sales1,type="l")
> #Plot ACF and PACF on the same graph
par(mfrow=c(1,1))
acf(sales1) 
pacf(sales1)

sales.fit <- arima (sales, order=c(1,1,0), seasonal = list(order=c(1,1,0),period=12), include.mean=FALSE)
sales.fit
sales.predict <- predict (sales.fit, n.ahead=5)
sales.predict
csales

par(mfrow=c(1,1)) 
plot (asales,xlim=c(1965,1971)) 

lines (sales.predict$pred,col="blue") 
lines (sales.predict$pred+2*sales.predict$se,col="red") 
lines (sales.predict$pred-2*sales.predict$se,col="red")



forbar <- matrix(x,ncol=12,byrow=TRUE) 
colnames(forbar)<- msales[1:12,4] 
rownames(forbar)<- c("Actual","Predicted") 
barplot(forbar,beside=TRUE, + main="Actual Vs Predicted", 
        + ylab="Monthly Sales", 
        + col=rainbow(2), 
        + xlab="Months 1971")


