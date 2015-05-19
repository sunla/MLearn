spamd <- read.table('~/Downloads/spamD.tsv',
          sep='\t',header = T,)
spamTrain <- subset(spamd,spamd$rgroup>=10)
spamTest <- subset(spamd,spamd$rgroup<10)
spamVars <- setdiff(colnames(spamd), list('rgroup','spam'))
colnames(spamd)
spamFormula <- as.formula(paste('spam=="spam"',paste(spamVars,collapse='+'),sep='~'))
spamModel <- glm(spamFormula,family=binomial(link='logit'),data=spamTrain)
spamTrain$pred <- predict(spamModel,newdata=spamTrain,type='response')
spamTest$pred <- predict(spamModel,newdata=spamTest,type='response')
print(with(spamTest,table(y=spam,glmPred=pred>0.5)))
spamTrain$pred
