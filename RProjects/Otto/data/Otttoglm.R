setwd("~/Documents/R/Otto/data")
### Requires #####
require("e1071");     # used for Naive Bayes 
require("ROCR");      # used for perforance function
require("caret");     # used for confusion matrix & train functions
require("data.table")
require("dplyr")      # used for groupby function
library(ggplot2)

## Read data ####
prod <- read.table("train.csv", header = TRUE, sep = ",")
test <- read.table("test.csv", header = TRUE, sep = ",")

#feat_11 feat_60 feat_27 feat_34  feat_3 feat_46 feat_90 feat_61 feat_80 feat_14 feat_15 feat_42 
# 14        11       7       6       6       6       6       6       4       4       4       2   
#feat_30 feat_69 feat_36 feat_57 feat_25 feat_40 feat_72 feat_58 feat_2 feat_86 feat_88 feat_26 feat_75 feat_19 feat_20 
# 2       2       2       2       2       2       1       1       1       1       1       1       1       1       1 

y <- "target"
x <- c("feat_11", "feat_60", "feat_27", "feat_34","feat_3",
       "feat_46", "feat_90", "feat_61", "feat_80", "feat_14", "feat_15",
       "feat_42", "feat_30", "feat_69", "feat_36", "feat_57", "feat_25", "feat_40")
fmla <- paste(y,paste(x,collapse = "+"), sep = "~")
fmla

prod$id <- NULL
prod$group <- NULL
model <- glm (target ~ feat_65  + feat_78    + feat_1   + feat_16   + feat_87   + feat_73   + feat_39    + feat_4 + feat_63   + feat_27   + feat_64   + feat_38   + feat_88   + feat_89   + feat_36  + feat_91 + feat_26   + feat_59   + feat_46   + feat_56    + feat_3   + feat_45  + feat_13  + feat_86  + feat_47   + feat_19    + feat_2   + feat_29   + feat_57   + feat_60   + feat_74   + feat_75 
              + feat_72   + feat_35   + feat_93   + feat_68   + feat_33   + feat_90   + feat_42   + feat_69 
              + feat_41   + feat_82    + feat_9   + feat_34   + feat_50   + feat_25   + feat_14   + feat_40 
              + feat_15   + feat_11   + feat_31   + feat_58   + feat_43 , data = prod , family = binomial(link = "logit"))
summary(model)
sort(model$coefficients)

prod$pred <- predict(model, prod, type = "response")
test$pred <- predict(model, test, type = "response")
head(prod,2)
ggplot(prod, aes(x=pred, color = target, linetype = target)) + geom_density()
plot(model)
head(prod[,c("target", "pred")],10)
