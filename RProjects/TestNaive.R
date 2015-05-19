library("e1071")
library("dplyr")
emctest <- read.table("TestNaive.csv", sep =",", header = TRUE, colClasses=rep("character", 4))
emctest %>% group_by(emctest[,1]) %>% tally()
emctest <- emctest[1:6,1:4]
emctest
summary(emctest)

yprob <- table(emctest$Y)
yprob <- yprob/sum(yprob)
yprob
x1prob <- table(emctest[,c("X1", "Y")])
x1prob <- x1prob/rowSums(x1prob)
x1prob
x2prob <- table(emctest[,c("X2", "Y")])
x2prob <- x2prob/rowSums(x2prob)
x2prob
x3prob <- table(emctest[,c("X3", "Y")])
x3prob <- x3prob/rowSums(x3prob)
x3prob

x1testprob <- table(emctest[,c("Y", "X1")])
x1testprob <- x1testprob/rowSums(x1testprob)
x1testprob


# prediction for value x(1,0,0)
py1 <- x1prob["1","1"] *
  x2prob["1","0"] *
  x3prob["1","0"] *
  yprob["1"]

py0 <- x1prob["0","1"] *
  x2prob["0","0"] *
  x3prob["0","0"] *
  yprob["0"]

py1; py0; 
4/54; 1/54
max(py1, py0)

# prediction for value x(0,0,1)
py10 <- x1prob["1","0"] *
  x2prob["1","0"] *
  x3prob["1","1"] *
  yprob["1"]

py10 / (.5 * 2/6 * .5)

py00 <- x1prob["0","0"] *
  x2prob["0","0"] *
  x3prob["0","1"] *
  yprob["0"]

py10; py00; max(py10, py00)

model <- naiveBayes(Y ~.,emctest)
model
yprob ; x1prob ; x2prob ; x3prob
testdata <- data.frame("0","0","1")
names(testdata) <- c("X1", "X2", "X3")
summary(testdata)
emctest[1,]
results <- predict (model,testdata)
results

emctest1 <- emctest[, c(as.character("X1"), as.character("X2"), as.character("X3"), as.character("Y"))]
emctest1
model <- naiveBayes(Y ~.,emctest1)
model
results <- predict (model,testdata)
results
