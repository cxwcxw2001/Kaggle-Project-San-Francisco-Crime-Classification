library(data.table)
library(gbm)
library(lubridate)
setwd("kaggle/SFCrimeClassification")
source("ContToDisc2.R")

raw=read.csv('train.csv',header=TRUE)
training.dt=data.table(raw)

# Remove useless columns
training.dt[,Descript:=NULL]
training.dt[,Resolution:=NULL]

# Extract hour,month and year out of date
training.dt[,Dates:=ymd_hms(Dates)]
training.dt[,Hours:=hour(Dates)]
training.dt[,Months:=month(Dates)]
months_vector=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
for (mth in 1:12) {
  training.dt[Months==mth,Months_text:=months_vector[mth]]
}
training.dt[,Years:=year(Dates)]

# Create a unique identifier
training.dt[,CaseID:=1:nrow(training.dt)]
setkeyv(training.dt,"CaseID")

# Convert all continuous features (dates) into discrete features
casted.caseId.years=ContToDisc(training.dt,"Years")
casted.caseId.months=ContToDisc(training.dt,"Months_text")
casted.caseId.hours=ContToDisc(training.dt,"Hours")
casted.caseId.dayOfWeek=ContToDisc(training.dt,"DayOfWeek")
casted.caseId.PdDistrict=ContToDisc(training.dt,"PdDistrict")

# Combine them all together along with the outcome variable
casted.features=Reduce(merge,list(casted.caseId.months,casted.caseId.hours,
                      casted.caseId.dayOfWeek,casted.caseId.PdDistrict),
                      init=casted.caseId.years)
casted.training.dt=casted.features[training.dt[,list(CaseID,Category)]]
casted.training.dt[,CaseID:=NULL]

# Fit a generalized boosted multinominal regression model
fit.gbm=gbm(Category~.,distribution="multinomial",
            data=casted.training.dt,
            n.trees=120,
            cv.folds=3) 


# Check performance 
best.iter.gbm=gbm.perf(fit.gbm,method="OOB")

summary(fit.gbm) #

# Perform the same manipulation on the test data set
test.raw=read.csv('test.csv',header=TRUE)
testing.dt=data.table(test.raw)

# Extract hour,month and year out of date
testing.dt[,Dates:=ymd_hms(Dates)]
testing.dt[,Hours:=hour(Dates)]
testing.dt[,Months:=month(Dates)]
for (mth in 1:12) {
  testing.dt[Months==mth,Months_text:=months_vector[mth]]
}
testing.dt[,Years:=year(Dates)]

# Create a unique identifier
testing.dt[,CaseID:=1:nrow(testing.dt)]
setkeyv(testing.dt,"CaseID")

# Convert all continuous features (dates) into discrete features
casted.caseId.years=ContToDisc(testing.dt,"Years")
casted.caseId.months=ContToDisc(testing.dt,"Months_text")
casted.caseId.hours=ContToDisc(testing.dt,"Hours")
casted.caseId.dayOfWeek=ContToDisc(testing.dt,"DayOfWeek")
casted.caseId.PdDistrict=ContToDisc(testing.dt,"PdDistrict")

# Combine them all together along with the outcome variable
casted.features=Reduce(merge,list(casted.caseId.months,casted.caseId.hours,
                                  casted.caseId.dayOfWeek,casted.caseId.PdDistrict),
                       init=casted.caseId.years)
casted.features[,CaseID:=NULL]

# Make prediction
pred=predict.gbm(fit.gbm,newdata = casted.features,n.trees = 220,type = "response")

# Check predicted value
pred[1,1:39,1]
pred[2,1:39,1]
pred[3,1:39,1] 

# Export the predicted values to excel
pred.df=data.frame(matrix(data=pred,nrow=nrow(testing.dt),ncol=39),
                   row.names=0:(nrow(testing.dt)-1))
names(pred.df)=sort(unique(training.dt$Category))
write.csv(pred.df,file="pred_multinomialGBM2.csv")

