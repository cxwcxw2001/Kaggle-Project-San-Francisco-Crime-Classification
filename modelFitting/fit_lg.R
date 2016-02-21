#================================Function Definition============================
addrCountFeaturizer=function(dt){
  # Perform count featurization on Address
  addresses=as.character(unique(dt$Address))
  crime_list=as.character(unique(dt$Category))
  addrCount_dt=data.table(Address=addresses)
  
  for (crime in crime_list){
    addrCount_dt[,paste0(crime," PROBABILITY"):=0]
  }
  
  for (addr in addresses){
    sub_dt=dt[Address==addr,]
    sub_dt_summary=sub_dt[,list(Crime_Frequency=.N),by=Category]
    sub_dt_summary[,Crime_Probability:=Crime_Frequency/nrow(sub_dt)]
    for (crime in crime_list){
      if (crime %in% sub_dt$Category){
        addrCount_dt[Address==addr,paste0(crime," PROBABILITY"):=sub_dt_summary
                    [Category==crime,.(Crime_Probability)]]
      } 
    }
  }
  
  return (addrCount_dt)
}

preProcessor=function(dt,addrCount_dt){
  # Remove useless columns
  if ("Descript" %in% names(dt)){
    dt[,Descript:=NULL]
  }
  if ("Resolution" %in% names(dt)){
    dt[,Resolution:=NULL]
  }
  dt[,c("X","Y"):=NULL]
  
  
  # Extract hour,month and year out of date
  dt[,Dates:=ymd_hms(Dates)]
  dt[,Hours:=as.numeric(hour(Dates))]
  dt[,Months:=month(Dates)]
  dt[,Years:=year(Dates)]
  dt[,Dates:=NULL]
  dt[,PdDistrict:=NULL]
  
  # Feature standardization
  dt[,Hours:=scale(Hours)]
  dt[,Months:=scale(Months)]
  dt[,Years:=scale(Years)]

  
  new_dt=merge(dt,addrCount_dt,by="Address",all.x=TRUE)
  new_dt[,Address:=NULL]
  return (new_dt)  
}

#===================================================================
library(data.table)
library(lubridate)
setwd("kaggle/SFCrimeClassification")

training_raw=read.csv('train.csv',header=TRUE)
training_raw=data.table(training_raw)
addrCountFeature=addrCountFeaturizer(training_raw)
training_dt=preProcessor(training_raw,addrCountFeature)

testing_raw=read.csv("test.csv",header=TRUE)
testing_raw=data.table(testing_raw)
testing_dt=preProcessor(testing_raw,addrCountFeature)

# Fit a logistic regressino model to each crime category seperately, and then
# comebine the results
crime_list=as.character(unique(training_dt$Category))
submission=data.table(Id=testing_raw$Id)

for (crime in crime_list){
   training_dt_binary=copy(training_dt)
   training_dt_binary[,BinaryCategory:=0]
   training_dt_binary[Category==crime,BinaryCategory:=1]
   training_dt_binary[,Category:=NULL]
   fit.lg=glm(BinaryCategory~.,data=training_dt_binary,family=binomial)
   pred=predict(fit.lg,newdata=testing_dt,type="response")
   submission[,paste0(crime):=pred]
}

submission[is.na(submission)]=0
submission=data.frame(submission)
names(submission)=c("Id",crime_list)
write.csv(submission,file="pred_lg.csv",row.names=FALSE)