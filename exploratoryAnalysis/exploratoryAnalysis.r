library(data.table)
library(lubridate)
library(ggplot2)
library(rpart)

setwd("kaggle/SFCrimeClassification")

train.raw=read.csv('train.csv',header=TRUE)
train.raw.dt=data.table(train.raw)

# Remove useless columns
train.raw.dt[,Descript:=NULL]
train.raw.dt[,Resolution:=NULL] 

# Convert Dates column from factor to date
train.raw.dt[,Dates:=ymd_hms(Dates)]

# Histogram of the outcome
ggplot(train.raw.dt,aes(x=Category))+
  geom_histogram(fill='white',colour='blue')+
  theme(axis.text.x=element_text(angle=60))

## Category VS time
theft.time=train.raw.dt[Category=='LARCENY/THEFT',
                        .(Dates,Category)]
theft.time.summary=theft.time[,.(frequency=.N),
                              by=Dates]
ggplot(theft.time.summary,aes(x=Dates,y=frequency))+
         geom_line() # frequency remains stable over 10 years,but kind of obscure

# Category VS year
theft.year=theft.time[,.(Year=year(Dates),Day=yday(Dates),Dates,Category)]
theft.year.summary=theft.year[,.(Occurence=.N),by=Year]
ggplot(theft.year.summary,aes(x=Year,y=Occurence))+
  geom_line()  
# Finding: Stable from 2002 to 2010, goes up to peak at 2013, then there is a sharp decrease

# Category vs month
theft.month=theft.time.summary[,.(mean_frequency=mean(frequency)),
                               by=month(Dates)]
ggplot(theft.month,aes(x=month,y=mean_frequency))+
  geom_line()
# Finding: An increasing trend from Jan to Sep, then the trend reverses


# Category vs DayOfWeek
crime.dayOfWeek=train.raw.dt[Category=='LARCENY/THEFT',list(Category,DayOfWeek)]
crime.dayOfWeek.summary=crime.dayOfWeek[,.(Occurence=.N),by=DayOfWeek]
crime.dayOfWeek.summary
# Finding: Crime occurs most frequently on Friday and Saturday 

# Extract year,hour out of date
train.raw.dt[,Hour:=hour(Dates)]

# Category vs hour
crime.hour=train.raw.dt[Category=='LARCENY/THEFT',list(Category,Hour)]
crime.hour.summary=crime.hour[,.(Occurence=.N),by=Hour]
ggplot(crime.hour.summary,aes(x=Hour,y=Occurence))+
       geom_line()
# Finding: decreasing trend from 0-5, increasing trend from 5 to 18, and decresing trend from 18 to 24

ctg.hour.tbl=table(train.raw.dt[,Hour],train.raw.dt[,Category])
chi.test=chisq.test(ctg.hour.tbl,simulate.p.value=TRUE)
chi.test$p.value

## Category by X (longtitude)
ctg.x=train.raw.dt[,.(mean_X=mean(X)),by=Category]
setorderv(ctg.x,'mean_X',order=-1)
ctg.x
# Finding: X does not change significantly across categories

# Statistical test
anova.test.X=aov(X~Category,data=train.raw.dt)
summary(anova.test.X)
# Finding: null hypothesis rejected

# Category by Y (latitude)
ctg.y=train.raw.dt[,.(mean_Y=mean(Y)),by=Category]
setorderv(ctg.y,'mean_Y',order=-1)
ctg.y
# Finding: Y does not change significantly across categories either

# Statistical test
anova.test.Y=aov(Y~Category,data=train.raw.dt)
summary(anova.test.Y)
# Finding: null hypothesis rejected

# Category VS PdDistrict
crime.pd=train.raw.dt[Category=="LARCENY/THEFT",.(Category,PdDistrict)]
crime.pd.count=crime.pd[,.(counts=.N),by=PdDistrict]
crime.pd.count
# Finding: Obviously counts is important factor

# Conclusion: Year, month, day of week, hour, pdDistrict are all significant features



 