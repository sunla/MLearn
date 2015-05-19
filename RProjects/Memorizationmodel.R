d <- read.table('orange_small_train.data.gz',    # Note: 1 
                  header=T,
                  sep='\t',
                  na.strings=c('NA','')) 	# Note: 2 
churn <- read.table('orange_small_train_churn.labels.txt',
                    header=F,sep='\t') 	# Note: 3 
d$churn <- churn$V1 	# Note: 4 
appetency <- read.table('orange_small_train_appetency.labels.txt',
                        header=F,sep='\t')
d$appetency <- appetency$V1 	# Note: 5 
upselling <- read.table('orange_small_train_upselling.labels.txt',
                        header=F,sep='\t')
d$upselling <- upselling$V1 	# Note: 6 
set.seed(729375) 	# Note: 7 
d$rgroup <- runif(dim(d)[[1]])
d$rgroup
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9) 	# Note: 8 
outcomes=c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll),
                c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in%
                  c('factor','character')] 	# Note: 9 
numericVars <- vars[sapply(dTrainAll[,vars],class) %in%
                      c('numeric','integer')] 	# Note: 10 
rm(list=c('d','churn','appetency','upselling')) 	# Note: 11 
outcome <- 'churn' 	# Note: 12 
pos <- '1' 	# Note: 13 
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0 	# Note: 14 
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)
dTrain$Var218

table218 <- table(
  Var218=dTrain[,'Var218'],   # Note: 1 
  churn=dTrain[,outcome], 	# Note: 2 
  useNA='ifany') 	# Note: 3 
print(table218)

mkPredC <- function(outCol,varCol,appCol) {   # Note: 1 
  pPos <- sum(outCol==pos)/length(outCol) 	# Note: 2 
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos] 	# Note: 3 
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3) 	# Note: 4 
  pred <- pPosWv[appCol] 	# Note: 5 
  pred[is.na(appCol)] <- pPosWna 	# Note: 6 
  pred[is.na(pred)] <- pPos 	# Note: 7 
  pred 	# Note: 8 
}


library('ROCR')

calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
}

# Note 1: 
#   Read the file of independent variables. All 
#   data from 
#   https://github.com/WinVector/zmPDSwR/tree/master/KDD2009. 

# Note 2: 
#   Treat both NA and the empty string as missing 
#   data. 

# Note 3: 
#   Read churn dependent variable. 

# Note 4: 
#   Add churn as a new column. 

# Note 5: 
#   Add appetency as a new column. 

# Note 6: 
#   Add upselling as a new column. 

# Note 7: 
#   By setting the seed to the pseudo-random 
#   number generator, we make our work reproducible: 
#   someone redoing it will see the exact same 
#   results. 

# Note 8: 
#   Split data into train and test subsets. 

# Note 9: 
#   Identify which features are categorical 
#   variables. 

# Note 10: 
#   Identify which features are numeric 
#   variables. 

# Note 11: 
#   Remove unneeded objects from workspace. 

# Note 12: 
#   Choose which outcome to model (churn). 

# Note 13: 
#   Choose which outcome is considered 
#   positive. 

# Note 14: 
#   Further split training data into training and 
#   calibration. 

