setwd("~/Documents/R/Otto/data")
library(kernlab) # ksvm
library(ggplot2)
library(e1071) # SVM and Naive Bayes
library(caret) # Confusion Matrix
library(dplyr)

data('spirals')

## Read data ####
prod <- read.table("train.csv", header = TRUE, sep = ",")
test <- read.table("test.csv", header = TRUE, sep = ",")

## Formula ####
formula <- target ~ feat_1 + feat_2 + feat_3 + feat_4 + feat_5 + feat_6 + feat_7 + feat_8 + feat_9 + feat_10 + 
  feat_11 + feat_12 + feat_13 + feat_14 + feat_15 + feat_16 + feat_17 + feat_18 + feat_19 + feat_20 + 
  feat_21 + feat_22 + feat_23 + feat_24 + feat_25 + feat_26 + feat_27 + feat_28 + feat_29 + feat_30 + 
  feat_31 + feat_32 + feat_33 + feat_34 + feat_35 + feat_36 + feat_37 + feat_38 + feat_39 + feat_40 + 
  feat_41 + feat_42 + feat_43 + feat_44 + feat_45 + feat_46 + feat_47 + feat_48 + feat_49 + feat_50 + 
  feat_51 + feat_52 + feat_53 + feat_54 + feat_55 + feat_56 + feat_57 + feat_58 + feat_59 + feat_60 + 
  feat_61 + feat_62 + feat_63 + feat_64 + feat_65 + feat_66 + feat_67 + feat_68 + feat_69 + feat_70 + 
  feat_71 + feat_72 + feat_73 + feat_74 + feat_75 + feat_76 + feat_77 + feat_78 + feat_79 + feat_80 + 
  feat_81 + feat_82 + feat_83 + feat_84 + feat_85 + feat_86 + feat_87 + feat_88 + feat_89 + feat_90 + 
  feat_91 + feat_92 + feat_93

## Feature Selection ####
correlationmatrix <- cor(prod[,2:94])
head(correlationmatrix,2)
correlationmatrix[1:3,]
highlycorrelated <- findCorrelation(correlationmatrix, cutoff = 0.70)

highlycorrelated # 3 15  9 39 30 with 0.70 cutoff

formula <- target ~ feat_1 + feat_2 + feat_3 + feat_4 + feat_5 + feat_6 + feat_7 + feat_8 + feat_9 + feat_10 + 
  feat_11 + feat_12 + feat_13 + feat_14 + feat_15 + feat_16 + feat_17 + feat_18 + feat_19 + feat_20 + 
  feat_21 + feat_22 + feat_23 + feat_24 + feat_25 + feat_26 + feat_27 + feat_28 + feat_29 + feat_30 + 
  feat_31 + feat_32 + feat_33 + feat_34 + feat_35 + feat_36 + feat_37 + feat_38 + feat_39 + feat_40 + 
  feat_41 + feat_42 + feat_43 + feat_44 + feat_45 + feat_46 + feat_47 + feat_48 + feat_49 + feat_50 + 
  feat_51 + feat_52 + feat_53 + feat_54 + feat_55 + feat_56 + feat_57 + feat_58 + feat_59 + feat_60 + 
  feat_61 + feat_62 + feat_63 + feat_64 + feat_65 + feat_66 + feat_67 + feat_68 + feat_69 + feat_70 + 
  feat_71 + feat_72 + feat_73 + feat_74 + feat_75 + feat_76 + feat_77 + feat_78 + feat_79 + feat_80 + 
  feat_81 + feat_82 + feat_83 + feat_84 + feat_85 + feat_86 + feat_87 + feat_88 + feat_89 + feat_90 + 
  feat_91 + feat_92 + feat_93

formulaTrain <- target ~ feat_1 + feat_2 + feat_4 + feat_5 + feat_6 + feat_7 + feat_8 + feat_10 + 
  feat_11 + feat_12 + feat_13 + feat_14 + feat_16 + feat_17 + feat_18 + feat_19 + feat_20 + 
  feat_21 + feat_22 + feat_23 + feat_24 + feat_25 + feat_26 + feat_27 + feat_28 + feat_29 + 
  feat_31 + feat_32 + feat_33 + feat_34 + feat_35 + feat_36 + feat_37 + feat_38 + feat_40 + 
  feat_41 + feat_42 + feat_43 + feat_44 + feat_45 + feat_46 + feat_47 + feat_48 + feat_49 + feat_50 + 
  feat_51 + feat_52 + feat_53 + feat_54 + feat_55 + feat_56 + feat_57 + feat_58 + feat_59 + feat_60 + 
  feat_61 + feat_62 + feat_63 + feat_64 + feat_65 + feat_66 + feat_67 + feat_68 + feat_69 + feat_70 + 
  feat_71 + feat_72 + feat_73 + feat_74 + feat_75 + feat_76 + feat_77 + feat_78 + feat_79 + feat_80 + 
  feat_81 + feat_82 + feat_83 + feat_84 + feat_85 + feat_86 + feat_87 + feat_88 + feat_89 + feat_90 + 
  feat_91 + feat_92 + feat_93

## Train against full training dataset for SVM model ####
prod$id <- NULL
model <- svm(target ~ ., prod, probability = TRUE)
prod$pred <- predict(model, prod, probability = TRUE)
confusionMatrix(prod$target,prod$pred)

## Predict on test fulltest with SVM model with default radial kernel ####
test$pred <- predict(model, test, probability = TRUE)
test$C <- round(attr(test$pred, "probabilities"),2)
write.csv(test[,c("id","C")],file = "OttoSubmission.csv", row.names = FALSE)
head(test)
class(test$C)
head(test$C)


## Validate results ####

prod$id <- NULL
test$id <- NULL
validate <- anti_join(test[,2:94], prod[,2:94])


#validate <- compare (prod[,-1],test, allowAll = TRUE)
#validate$tCpartial

## Train against 20% of dataset ####
set.seed(2335246L)
prod$group <- sample.int(100, size = dim(prod)[[1]], replace = T)
prodTrain <- subset(prod,group <20 )
head(prodTrain)
prodTrain$id <- NULL
prodTrain$group <- NULL
summary(prodTrain)
y <- "target"
x <- c("feat_11", "feat_60", "feat_27", "feat_34","feat_3",
       "feat_46", "feat_90", "feat_61", "feat_80", "feat_14", "feat_15",
       "feat_42", "feat_30", "feat_69", "feat_36", "feat_57", "feat_25", "feat_40")
fmla <- paste(y,paste(x,collapse = " + "), sep = " ~ ")
fmla




##feat_65  + feat_78    + feat_1   + feat_16   + feat_87   + feat_73   + feat_39    + feat_4 + feat_63   + feat_27   + feat_64   + feat_38   + feat_88   + feat_89   + feat_36  + feat_91 + feat_26   + feat_59   + feat_46   + feat_56    + feat_3   + feat_45  + feat_13  + feat_86  + feat_47   + feat_19    + feat_2   + feat_29   + feat_57   + feat_60   + feat_74   + feat_75 
##+ feat_72   + feat_35   + feat_93   + feat_68   + feat_33   + feat_90   + feat_42   + feat_69 
##+ feat_41   + feat_82    + feat_9   + feat_34   + feat_50   + feat_25   + feat_14   + feat_40 
##+ feat_15   + feat_11   + feat_31   + feat_58   + feat_43 

prodTrain$pred <- NULL
summary(prodTrain)
svmprod <- svm(target ~ . , prodTrain, probability = TRUE, kernel = "radial") 
ksvmprod <- ksvm(target ~ ., prodTrain, prob.model=T)
str(prod)
summary(svmprod)
prodTrain$pred <- predict(svmprod, prodTrain, probability = TRUE)
confusionMatrix(prodTrain$target,prodTrain$pred)
head(attr(prodTrain$pred, "probabilities"))
summary(prodTrain)
head(prodTrain$id,prodTrain$feat_1)

set.seed(2335246L)
test$group <- sample.int(100, size = dim(test)[[1]], replace = T)
prodtest <- subset(test,group >90 )
head(prodtest)
summary(prodtest)
prodtest$group <- NULL

