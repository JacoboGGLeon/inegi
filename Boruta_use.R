install.packages("Boruta", dependencies = TRUE)
install.packages("ranger", dependencies = TRUE)

library(ranger)
library(Boruta)

#Sys.setenv('R_MAX_VSIZE'=32000000000)

traindata <- read.csv("../jacoboleon/Desktop/houses_sales_kaggle/train_impute.csv", header = T, stringsAsFactors = F)

#Let’s have a look at the data.
str(traindata)
names(traindata) <- gsub("_", "", names(traindata))

#gsub() function is used to replace an expression with other one. In this case, I’ve replaced the underscore(_) with blank(“”).
summary(traindata)

#Now we’ll replace blank cells with NA. This will help me treat all NA’s at once.
traindata[traindata == ""] <- NA

#I’m following the simplest method of missing value treatment i.e. list wise deletion.
traindata <- traindata[complete.cases(traindata),]

#Let’s convert the categorical variables into factor data type.
convert <- c(2:6, 11:13)
traindata[,convert] <- data.frame(apply(traindata[convert], 2, as.factor))

set.seed(123)
boruta_train <- Boruta(x = traindata~.-LoanID, data = traindata, doTrace = 2)
print(boruta_train)

plot(boruta_train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_train$ImpHistory),function(i)
boruta_train$ImpHistory[is.finite(boruta_train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
at = 1:ncol(boruta_train$ImpHistory), cex.axis = 0.7)

final_boruta <- TentativeRoughFix(boruta_train)
print(final_boruta)

getSelectedAttributes(final_boruta, withTentative = F)

boruta_df <- attStats(final_boruta)
class(boruta_df)

print(boruta_df)