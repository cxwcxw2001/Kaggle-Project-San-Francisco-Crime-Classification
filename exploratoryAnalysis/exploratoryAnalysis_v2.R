library(data.table)
library(lubridate)
library(ggplot2)
library(rpart)

setwd("kaggle/SFCrimeClassification")

train.raw=read.csv('train.csv',header=TRUE)
train.raw.dt=data.table(train.raw)

# Remove useless columns
train.dt=copy(train.raw.dt)
train.dt[,Descript:=NULL]
train.dt[,Resolution:=NULL] 

# Convert Dates column from factor to date
train.dt[,Dates:=ymd_hms(Dates)]

# Extract year, month and hour out of date
train.dt[,Year:=year(Dates)]
train.dt[,Month:=month(Dates)]
train.dt[,Hour:=hour(Dates)]

# Histogram of the outcome
ggplot(train.raw.dt,aes(x=Category))+
  geom_histogram(fill='white',colour='blue')+
  theme(axis.text.x=element_text(angle=60))

# Frequency of the outcome
category.freq=train.dt[,list(Freq_prop=.N/nrow(train.dt)),by=Category]
setorderv(category.freq,"Freq_prop",-1)
category.freq # the top three categories are LARCENY/THEFT,OTHER OFFSENSE,NON-CRIMINAL,
              # ASSAULT,and DRUG/NARCOTIC

## Category VS time
theft.time=train.dt[Category=='LARCENY/THEFT',list(Dates,Category)]
theft.time.summary=theft.time[,.(frequency=.N),by=Dates]
ggplot(theft.time.summary,aes(x=Dates,y=frequency))+
         geom_line() # frequency remains stable over 10 years,but kind of obscure

# Category VS year
theft.year=theft.time[,.(Year=year(Dates),Day=yday(Dates),Category)]
theft.year.summary=theft.year[,.(Occurence=.N),by=Year]
ggplot(theft.year.summary,aes(x=Year,y=Occurence))+
  geom_line()  
# Finding: Stable from 2003 to 2010, goes up to peak at 2013, then there is a sharp decrease
# Thought: 2003-2011 as year_range1, 2012 as year_range2,2013 as year_range3,2014 as year_range4,
# 2015 as year_range5
assault.year=train.dt[Category=="ASSAULT",.(Occurence=.N),by=Year]
ggplot(assault.year,aes(x=Year,y=Occurence))+geom_line()
# Finding: Stable from 2003-2014, and drops sharply in 2015
# Thought: the previous range-cutting method may not work
drug.year=train.dt[Category=="DRUG/NARCOTIC",.(Occurence=.N),by=Year]
ggplot(drug.year,aes(x=Year,y=Occurence))+geom_line()
# Finding: 2002-2006 as year_range1, 2007-2009 as year_range2, the rest years should be taken
# as an individual range respectively
# Thought: Each year should be taken as one year_range

# Category vs month
theft.month=train.dt[Category=="LARCENY/THEFT",.(Occurence=.N),by=Month]
ggplot(theft.month,aes(x=Month,y=Occurence))+geom_line()
# Finding: the occurence frequency is pretty volatile across 12 months, except that
# it is stable from June to September
assault.month=train.dt[Category=="ASSAULT",.(Occurence=.N),by=Month]
ggplot(assault.month,aes(x=Month,y=Occurence))+geom_line()
# Finding: significant change across 12 months
drug.month=train.dt[Category=="DRUG/NARCOTIC",.(Occurence=.N),by=Month]
ggplot(drug.month,aes(x=Month,y=Occurence))+geom_line()
# Finding: Stable from Jan to Apr, goes down sharply from Apr to June, 
# increases to Oct, plumets until Dec
# Thought: No month can be bucketed together when perform discretization


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



 