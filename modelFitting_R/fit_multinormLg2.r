library(data.table)
library(nnet)
library(lubridate)
setwd("kaggle/SFCrimeClassification")
source("MyOwnConverter.R")

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

# Convert all continuous features (dates) into discrete features
feature.name.list=c("Hours","Months_text","Years")
casted.features.tbl=MyOwnConverter(training.dt,feature.name.list)
casted.training.dt=casted.features.tbl[training.dt[,list(CaseID,Category,
                                              DayOfWeek,PdDistrict)]]
casted.training.dt[,CaseID:=NULL]

# Fit the multinomial logistic regression model
fit.multiLg=multinom(Category ~ ., data=casted.training.dt,maxit=200,MaxNWts=2574) 

# Preprocess testing data



