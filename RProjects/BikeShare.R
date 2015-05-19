Btrain <- read.table("train.csv",sep=",",header = T)
dim(Btrain)
plot(Btrain)
head(Btrain)
?cor
cor(Btrain, use="complete.obs", method="kendall")
hour <- format(Btrain$datetime , format="%k")
month <- format(Btrain$datetime , format="%m")
month
Btrain$datetime
