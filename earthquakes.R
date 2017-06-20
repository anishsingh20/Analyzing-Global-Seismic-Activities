#Seismic Data Analysis

#loading the dataset

earthq<-read.csv('database.csv')
earthq

#Summary Statistics
summary(earthq)

#removing the NA columns-suing apply function
earthq<-earthq[,!apply(is.na(earthq), 2, any)]

#making a new column Year with Year values
earthq$Year <- format(as.Date(earthq$Date, format="%d/%m/%Y"),"%Y")

#Removing NA values from Year column


require(dplyr)
require(ggplot2)
as.Date(earthq$Date)

#Distribution of Magnitude of Earthquakes
ggplot(aes(x = Magnitude ),data = earthq) + 
  geom_histogram(color="black",fill="yellow",bins=35) + 
  labs(title="Distribution of Magintude of Seismic Activities") 
 
by(earthq$Magnitude,earthq$Type,summary)
#Highest Median Magnitude of Rock Burst

#most types of seismic activities were Earthquakes then Explosions 
table(earthq$Type)







