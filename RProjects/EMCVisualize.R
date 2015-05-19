custdata <- read.table("custdata.tsv", sep="\t", header = T)
dim(custdata)
head(custdata)
head(custdata, n= 10)
tail(custdata, n= 5)
summary(custdata)
names(custdata)
typeof(custdata)
cor(custdata)
ls()
var(custdata$income)
plot(density(custdata$income))
range(custdata$income)
sd(custdata$income)
tc <- mean(custdata$income, trim = .10)
tc
rm(testcust)
subcust <- subset(custdata, custdata$income > 10000 & custdata$income < 500000)
breaks <- c(-1, 23000, 52000, 82000, 250000, 999999)
labels <- c("Poverty", "LowerMid", "UpperMid", "Wealthy", "Rich")
wealth <- cut(custdata$income, breaks, labels)
custdata <- cbind(custdata, wealth)
head(custdata)
tail(custdata)
custdata[is.na(custdata$wealth),]
wt <- table(wealth)
percent <- (wt/sum(wt)*100)
percent
wt <- rbind(wt,percent)
table(wealth)
wt
plot(wt)
nt <- table(wealth, custdata$num.vehicles)
plot(nt)

library(MASS)
with(subcust, {
  hist(income, main="Distribution of Household Income",
       freq=FALSE)
  lines(density(income), lty=2, lwd=2)
  # line type (lty) 2 is dashed
  xvals = seq(from=min(income), to=max(income), length=100)
  param = fitdistr(income, "lognormal")
  lines(xvals, dlnorm(xvals, meanlog=param$estimate[1], sdlog=param$estimate[2]), col="blue")
})

logincome = log10(subcust$income)
hist(logincome, main="Distribution of Household Income", freq=FALSE)
# line type lty(2) is a dashed line lines(density(logincome), lty=2, lwd=2)
xvals = seq(from=min(logincome), to=max(logincome), length=100)
param = fitdistr(logincome, "normal")
lines(xvals, dnorm(xvals, param$estimate[1], param$estimate[2]), lwd=2, col="blue")

custdata$num.vehicles[is.na(custdata$num.vehicles)] <- 0
custdata$income[custdata$income == -8700] <- 0
custdata <- custdata[custdata$income >= 0 ]
with(custdata,cor(income,num.vehicles))
with(custdata,cor(log(income),num.vehicles))

boxplot(income ~ as.factor(num.vehicles), data=subcust, range=0, outline=F, log="y",xlab="# cars", ylab="Income")
boxplot(num.vehicles ~ wealth, data = custdata,main="Room by Wealth", Xlab="Category", ylab="# rooms")

### Generate the data 

offers = sample(c("nooffer","offer1","offer2"), size= 500 , replace=T)
purchasesize = ifelse(offers=="nooffer", rlnorm (500,meanlog=log(25)), 
                      ifelse(offers=="offer1", rlnorm(500, meanlog=log(50)),
                             rlnorm(500, meanlog=log(55))))
offertest = data.frame(offer=offers,purchase_amt=purchasesize)
summary(offertest)

aggregate(x=offertest$purchase_amt,
          by=list(offertest$offer), FUN="mean")
boxplot(purchase_amt ~ as.factor(offer), data = offertest, log="y")

model <- lm(purchase_amt ~ offer, data = offertest)
summary(model)

TukeyHSD(aov(model))

library(lattice)
densityplot(~ purchase_amt, group = offers, data = offertest, auto.key = T)

densityplot(~purchase_amt | offers, data=offertest) 
densityplot(~log10(purchase_amt) | offers, data=offertest)

library(ggplot2)
ggplot(data= offertest, aes(x= as.factor(offer), y=purchase_amt)) + 
  geom_point(position = "jitter", alpha = 0.2) + geom_boxplot(alpha = 0.1 , outlier.size = 0) +
  scale_y_log10()

ggplot(data=offertest) + geom_density(aes(x=purchase_amt, colour=as.factor(offers)))
ggplot(data=offertest) + geom_density(aes(x=purchase_amt, colour=as.factor(offers))) + scale_x_log10()

x = rnorm(10)
y = rnorm(10,2)
pooled.var = function(x, y) {
  nx = length(x)
  ny = length(y)
  stdx = sd(x)
  stdy = sd(y)
  num = (nx-1)*stdx^2 + (ny-1)*stdy^2
  denom = nx+ny-2  # degrees of freedom
  (num/denom) * (1/nx + 1/ny)
}

mx = mean(x)
my = mean(y)
mx - my

pooled.var(x,y)
tstat = (mx - my)/sqrt(pooled.var(x,y))
tstat
dof = length(x) + length(y) - 2
tailarea = pt(tstat, dof)
tailarea
pvalue = 2*tailarea
t.test(x, y, var.equal=T)
