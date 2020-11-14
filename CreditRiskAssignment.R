#MADI KASSYMBEKOV HEC ID 11284135
require(tidyverse)

#path to train and real data sets 
TRAINDIR="~/Documents/HEC/Statistical Learning/Assignment1/CreditGameData/CreditGame_TRAIN.csv"
REALDIR="~/Documents/HEC/Statistical Learning/Assignment1/CreditGameData/CreditGame_Applications.csv"
#upload data to R
traindata = read.csv(TRAINDIR)#, row.names = 1, header=TRUE)
#structure of data set
str(traindata)

# Creating a hold-out validation set
set.seed(60603)
keep=sample(1:nrow(traindata))
trainSet=traindata[keep[300001:nrow(traindata)],]
validSet=traindata[keep[1:300000],]

# There are missing values for this set.
apply(is.na(trainSet),2,sum)
apply(is.na(validSet),2,sum)

imputation_mean=sapply(trainSet,mean,na.rm=TRUE)

trainSet = trainSet %>%
  mutate(AGE_D=is.na(AGE_D), TYP_RES=as.factor(TYP_RES),
         TYP_FIN=as.factor(TYP_FIN), ST_EMPL=as.factor(ST_EMPL)
         ,target_0=as.factor(target_0)
  )
validSet = validSet %>%
  mutate(AGE_D=is.na(AGE_D),  TYP_RES=as.factor(TYP_RES),
         TYP_FIN=as.factor(TYP_FIN), ST_EMPL=as.factor(ST_EMPL),
         target_0=as.factor(target_0))
#get rid of factor with 1 level
trainSet$TYP_FIN=NULL
validSet$TYP_FIN=NULL
for(i in c(6)){
  trainSet[[i]][is.na(trainSet[[i]])]=imputation_mean[i]
  validSet[[i]][is.na(validSet[[i]])]=imputation_mean[i]
}

for(i in c(10)){
  trainSet[[i]][trainSet[i]==""]="T"
  validSet[[i]][validSet[i]==""]="T"
}

str(trainSet)

a1=glm(formula=target_0~. -ID_TRAIN -NB_INTR_12M-REV_NET-MNT_DEMANDE-AGE_D,
       data=trainSet,family="binomial")
summary(a1)

library(boot)

#10-Fold Cross Validation
cv_glm <- cv.glm(validSet, glmfit = a1, K = 10)
1 - cv_glm$delta[1] 

p1=predict(a1,newdata=validSet,type="response")
a=cbind(ID_TRAIN=validSet$ID_TRAIN,Prob_Pay=p1)


source("~/Documents/HEC/Statistical Learning/Week5-PredBinaries/PredictingBinaries.R")

confusion(validSet$target_0,as.numeric(p1>.5))
confusion(validSet$target_0,as.numeric(p1>.95))


roc(validSet$target_0,p1,col="blue")$AUC
roc(validSet$target_0,p2,lines=TRUE,col="red")$AUC

library(ROCR)

# Plotting the ROC curve
pred=prediction(p1,validSet$target_0)
perf=performance(pred,"tpr","fpr")
plot(perf)

# For reference, plotting the same curve with the custom function
roc(validSet$target_0,p1,lines=TRUE,col="red")$AUC

# perf is a S4 object, not a list. 
# We can look into its content with commands such as:
slotNames(perf)
perf@x.name

# Computing the AUC
performance(pred,"auc")

costsol=performance(pred,measure="cost", cost.fp=10,cost.fn=1)

plot(costsol)

# Optimal cutoff -- x scale on threshold rather than depth
slotNames(costsol)
costsol@x.values[[1]][which.min(costsol@y.values[[1]])]

#Test Data Set for competition
testData = read.csv(REALDIR)#, row.names = 1, header=TRUE)
library(plyr)
testData = rename(testData, c("ID_TEST"="ID_TRAIN"))
str(testData)

apply(is.na(testData),2,sum)

imputation_mean=sapply(testData,mean,na.rm=TRUE)

testData = testData %>%
  mutate(AGE_D=is.na(AGE_D), TYP_RES=as.factor(TYP_RES),
         TYP_FIN=as.factor(TYP_FIN), ST_EMPL=as.factor(ST_EMPL)
  )
for(i in c(6)){
  testData[[i]][is.na(testData[[i]])]=imputation_mean[i]
}
for(i in c(10)){
  testData[[i]][testData[i]==""]="T"
}

apply(is.na(testData),2,sum)

testresults=predict(a1,newdata=testData,type="response")

a=cbind(ID=testData$ID_TRAIN,Prob_Pay=testresults,DEFAULT=ifelse(testresults > 0.060275, 1 , 0))

#0.061 - $781,584
#0.060275 - $782,022
#0.06 - $760, $781,670



x = as.data.frame(a)

count(x, "DEFAULT")

final = cbind(ID=x$ID[x$DEFAULT==0])

final = na.omit(final)

head(final)
write.csv(final,"~/Documents/HEC/Statistical Learning/Assignment1/CreditGameData/Default.csv", row.names = FALSE)
