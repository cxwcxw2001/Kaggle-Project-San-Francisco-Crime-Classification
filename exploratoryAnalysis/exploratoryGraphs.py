import pandas as pd
import matplotlib.pyplot as plt

train=pd.read_csv('E:/kaggle/SFCrimeClassification/train.csv',parse_dates=['Dates'])

train['Year']=train['Dates'].map(lambda x: x.year)
train['Week']=train['Dates'].map(lambda x: x.week)
train['Hour']=train['Dates'].map(lambda x: x.hour)

print(train.head())

train['PdDistrict'].value_counts().plot(kind='bar')

train['event']=1
weekly_events = train[['Week','Year','event']].groupby(['Year','Week'])
    .count().reset_index()
weekly_events_years = weekly_events.pivot(index='Week', columns='Year', 
                                          values='event').fillna(method='ffill')
print (weekly_events_years.head())
ax = weekly_events_years.interpolate().plot(title=
    'number of cases every 2 weeks', figsize=(10,6))

hourly_events=train[['Hour','event']].groupby('Hour').count().reset_index()
print (hourly_events)

hourly_district_events=train[['PdDistrict','Hour','event']].groupby(
    ['PdDistrict','Hour']).count().reset_index()
print (hourly_district_events.head())
