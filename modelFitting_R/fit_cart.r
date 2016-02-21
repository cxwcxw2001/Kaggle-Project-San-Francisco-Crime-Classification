library(data.table)
library(rpart)
library(lubridate)
setwd("E:/kaggle/SFCrimeClassification")

raw=read.csv('train.csv',header=TRUE)
raw.dt=data.table(raw)

# Remove useless columns
raw.dt[,Descript:=NULL]
raw.dt[,Resolution:=NULL]

# Extract hour out of date
raw.dt[,Dates:=ymd_hms(Dates)]
raw.dt[,Hours:=hour(Dates)]

# Some categories are pretty rare.Check the frequency
raw.category.summary=raw.dt[,.(freq=.N),by=Category]
raw.category.summary[,prop:=freq/sum(freq)]
setorderv(raw.category.summary,'prop',order=-1) 

# Split into training and testing set
inTrain=sample(1:nrow(raw.dt),size = 0.6*nrow(raw.dt))
training=raw.dt[inTrain]
testing=raw.dt[-inTrain]

# Fit a single CART tree
train.control=rpart.control(cp=0.1,minsplit=0.01*nrow(training))
cart.fit=rpart(Category~DayOfWeek+Hours+X+Y,data=training,method='class',
               control = train.control)
summary(cart.fit)

# Check the model performance
test.pred=predict(cart.fit,newdata = testing,type='class')
if.accurate=test.pred==testing$Category
sum(if.accurate)/nrow(testing)
