library("rpart")
load("~/Downloads/GCDData.RData")
model<-rpart(Good.Loan~
               Duration.in.month+
               Installment.rate.in.percentage.of.disposable.income+
               Credit.amount +
               Other.installment.plans,
             data=d,
             control=rpart.control(maxdepth=4),
             method="class")
