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
ageCounts
incomeCounts <- table(traindata[,c("Enrolls", "Income")])
incomeCounts <- incomeCounts/rowSums(incomeCounts)
incomeCounts
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
