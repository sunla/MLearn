setwd("~/Documents/R/Otto/data")
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

## Read data ####
prod <- read.table("train.csv", header = TRUE, sep = ",")
test <- read.table("test.csv", header = TRUE, sep = ",")

#feat_11 feat_60 feat_27 feat_34  feat_3 feat_46 feat_90 feat_61 feat_80 feat_14 feat_15 feat_42 
# 14        11       7       6       6       6       6       6       4       4       4       2   
#feat_30 feat_69 feat_36 feat_57 feat_25 feat_40 feat_72 feat_58 feat_2 feat_86 feat_88 feat_26 feat_75 feat_19 feat_20 
# 2       2       2       2       2       2       1       1       1       1       1       1       1       1       1 

head(prod)

y <- "target"
x <- c("feat_11", "feat_60", "feat_27", "feat_34","feat_3",
       "feat_46", "feat_90", "feat_61", "feat_80", "feat_14", "feat_15",
       "feat_42", "feat_30", "feat_69", "feat_36", "feat_57", "feat_25", "feat_40")
fmla <- paste(y,paste(x,collapse = "+"), sep = "~")
fmla

testmodel <- multinom(fmla, data = prod)
summary(testmodel)
