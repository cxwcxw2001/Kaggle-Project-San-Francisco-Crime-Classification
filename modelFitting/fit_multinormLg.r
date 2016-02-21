library(data.table)
library(nnet)
library(lubridate)
setwd("E:/kaggle/SFCrimeClassification")

raw=read.csv('train.csv',header=TRUE)
raw.dt=data.table(raw)

# Remove useless columns
raw.dt[,Descript:=NULL]
raw.dt[,Resolution:=NULL]

# Extract year,month and hour out of date
raw.dt[,Dates:=ymd_hms(Dates)]
raw.dt[,Year:=year(Dates)]
raw.dt[,Month:=month(Dates)]
raw.dt[,Hour:=hour(Dates)]

# Choose the level of the outcome that we wish to use as our baseline
raw.dt[,Category:=relevel(raw.dt$Category,ref='OTHER OFFENSES')]

# Preprocessing
train=copy(raw.dt)
train[,Year:=as.factor(Year)]
train[,Month:=as.factor(Month)]
train[,Hour:=as.factor(Hour)]

# Fit a multinominal logistic regression model
fit=multinom(Category ~ Year+Month+DayOfWeek+Hour+X+Y, data=train,
             maxit=200,MaxNWts=2500) 

# Make predictions on test set provided by Kaggle
test.kaggle.raw=read.csv("test.csv",header=TRUE)
test.kaggle.raw=data.table(test.kaggle.raw)
test.kaggle.raw[,Dates:=ymd_hms(Dates)]
test.kaggle.raw[,Year:=as.factor(year(Dates))]
test.kaggle.raw[,Month:=as.factor(month(Dates))]
test.kaggle.raw[,Hour:=as.factor(hour(Dates))]
pred.toSubmit=predict(fit,newdata=test.kaggle.raw,type="probs")
 
