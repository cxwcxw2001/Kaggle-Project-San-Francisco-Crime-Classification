library(e1071)
library(data.table)
library(lubridate)
setwd("Kaggle/SFCrimeClassification")
source("MyOwnConverter.R")

training.raw=read.csv('train.csv',header=TRUE)
training.dt=data.table(training.raw)

# Remove useless columns
training.dt[,Descript:=NULL]
training.dt[,Resolution:=NULL]

# Extract hour,month and year out of date
training.dt[,Dates:=ymd_hms(Dates)]
training.dt[,Hours:=as.numeric(hour(Dates))]
training.dt[,Months:=month(Dates)]
training.dt[,Years:=year(Dates)]

# Remove all numbers at the beginning of the address
training.dt[,Address:=gsub(pattern="[0-9]+ Block",replacement="Block",x=training.dt$Address)]

# Fit a naive bayes model
fit_nb=naiveBayes(Category~DayOfWeek+Address+Hours+Months+Years,data=training.dt)

# Make predictions on test set provided by Kaggle
testing.raw=read.csv("test.csv",header=TRUE)
testing.dt=data.table(testing.raw)
testing.dt[,Dates:=ymd_hms(Dates)]
testing.dt[,Year:=year(Dates)]
testing.dt[,Month:=month(Dates)]
testing.dt[,Hour:=as.numeric(hour(Dates))]
testing.dt[,Address:=gsub(pattern="[0-9]+ Block",replacement="Block",x=testing.dt$Address)]
pred=predict(fit_nb,newdata=testing.dt,type="raw")

# Export the predicted values to excel
pred.df=data.frame(data=pred,row.names=0:(nrow(testing.dt)-1))
names(pred.df)=sort(unique(training.dt$Category))
write.csv(pred.df,file="pred_naiveBayes.csv")
