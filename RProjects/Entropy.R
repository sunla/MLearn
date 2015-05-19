library("entropy")
library("mclust")
library("FSelector")
mystring <- c(1,2,3,1,3,5,4,2,1,3,2,4,2,2,3,4,4)
mystring
myfreqs <- table(mystring)/length(mystring)
table(mystring)
length(mystring)
myfreqs
?table
# vectorize
myvec <- as.data.frame(myfreqs)[,2]
myvec
# H in bit
-sum(myvec * log2(myvec))

myvec * log2(myvec)


test

test <- -sum(myvec * log2(myvec))
mystring <- c(1,2,3,1,3,5,4,2,1,3,2,4,2,2,3,4,4)
entropy.empirical(myfreqs, unit="log2")

custdata <- read.table("custdata.tsv", sep = '\t', header = T)
weights <- information.gain(health.ins~., custdata)
weightsIn <- information.gain(health.ins~is.employed, custdata)
print(weights)

model1 <- lm(health.ins ~ marital.stat + age, data=custdata)
model <- glm(health.ins ~ income,family = binomial, data=custdata)
summary(model)
plot(health.ins ~ marital.stat+age, data=custdata)
lines(custdata$marital.stat, model1$fitted, type="l", col="red")

custdata$is.employed[is.na(custdata$is.employed)] <- 'Other'
custincome <- table(custdata[,c("is.employed", "health.ins")])
custincome

entfalse <- -((.369863 * log2(.37)) + (.630137 * log2(.630137)))
entfalse
enttrue <- -((.1402337 * log2(.1402337)) + (.8597663 * log2(.8597663)))
enttrue
entOther <- -((.14633415 * log2(.14633415)) + (.853685 * log2(.853685)))
entOther

280/328
27/(27+46)
46/(27+46)
84/(84+515)
328/1000
599/1000
73/1000

summary(custdata$is.employed)
73/672

igemp <- 0.6319119 - ((0.891369 * enttrue) + (0.108631 * entfalse) + (0.328 * entOther))#old
igemp <- 0.6319119 - ((0.599 * enttrue) + (0.073 * entfalse) + (0.328 * entOther))
igemp



custfreq <- table(custdata$health.ins)/nrow(custdata)
nrow(custdata)
table(custdata$health.ins)
length(custdata)
custfreq
pent <- entropy.empirical(custfreq, unit="log2")

custvec <- as.data.frame(custfreq)[,2]
-sum(custvec * log2(custvec))

# gtting probability of health.ins within marital.stat
custtab <- table(custdata[,c("marital.stat", "health.ins")]) # to get a frequency distribution of marital.stat by health.ins
#custtab[,"Htotal"] <- apply(custtab, 1, sum)
custtab
custtabvec <- as.data.frame(custtab)

custtabtot <- cbind(custtab, apply(custtab, 1, sum))
sum(custtab[1, ])
custtab <- names(c("FALSE", "TRUE", "TOT"))
custtabtot
custMfreq <- table(custdata$marital.stat)/nrow(custdata)
custMfreq
custMvec <- as.data.frame(custMfreq)[,2]
entropy.empirical(custvec, unit="log2")

custdata


?
FSelector
help("mclust")
?entropy
