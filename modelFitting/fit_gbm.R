library(data.table)
library(gbm)
library(lubridate)
setwd("kaggle/SFCrimeClassification")

raw=read.csv('train.csv',header=TRUE)
raw.dt=data.table(raw)
training.raw=copy(raw.dt)

# Remove useless columns
training.raw[,Descript:=NULL]
training.raw[,Resolution:=NULL]

# Extract hour,month and year out of date
training.raw[,Dates:=ymd_hms(Dates)]
training.raw[,Hours:=hour(Dates)]
training.raw[,Months:=month(Dates)]
training.raw[,Years:=year(Dates)]

# Fit a generalized boosted multinominal regression model
fit.gbm=gbm(Category~Years+Months+DayOfWeek+Hours+X+Y,
            distribution="multinomial",
            data=training.raw,
            n.trees=150)

summary(fit.gbm) # Only PdDistrict,Years and hours are informative predictors

# Check performance 
best.iter.gbm=gbm.perf(fit.gbm,method="OOB")

# Keep the variables with strong influence and refit the model with cross validation
fit.gbm2=gbm(Category~Years+X+Y+Hours,
             distribution = "multinomial",
             data=training.raw,
             n.trees = 150,
             cv.folds = 2)



# Input testing set
testing=read.csv('test.csv',header=TRUE)
testing=data.table(testing)
testing[,Dates:=ymd_hms(Dates)]
testing[,Hours:=(hour(Dates))]
testing[,Months:=(month(Dates))]
testing[,Years:=(year(Dates))]

# Make prediction
pred=predict(fit.gbm,newdata=testing,n.trees=120,type="response")

# Check predicted value
pred[1,1:39,1]
pred[2,1:39,1]
pred[3,1:39,1]

# Export the predicted values to excel
pred.df=data.frame(matrix(data=pred,nrow=nrow(testing),ncol=39),row.names=0:(nrow(testing)-1))
names(pred.df)=sort(unique(training.raw$Category))
write.csv(pred.df,file="pred_multinomialGBM.csv")

