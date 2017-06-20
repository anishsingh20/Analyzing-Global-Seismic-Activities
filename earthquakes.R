#Seismic Data Analysis

#loading the dataset
require(dplyr)
require(ggplot2)
library(maps)
library(fpp)
library(astsa)
library(lubridate)

earthq<-read.csv('database.csv')
earthq

#Summary Statistics
summary(earthq)

#removing the NA columns-suing apply function
earthq<-earthq[,!apply(is.na(earthq), 2, any)]

#making a new column Year with Year values
earthq$Year <- format(as.Date(earthq$Date, format="%d/%m/%Y"),"%Y")




#Distribution of Magnitude of Earthquakes
ggplot(aes(x = Magnitude ),data = earthq) + 
  geom_histogram(color="black",fill="yellow",bins=35) + 
  labs(title="Distribution of Magintude of Seismic Activities") 

#istribution of Magintudes for Earthquakes
ggplot(data=filter(earthq,Type=="Earthquake"),aes(Magnitude))+
  geom_area(aes(y = ..count..,fill="blue"), stat = "bin")+
  labs(title="Earthquakes",caption="Anish") + 
  guides(fill=FALSE)
 
by(earthq$Magnitude,earthq$Type,summary)
#Highest Median Magnitude of Rock Burst

#most types of seismic activities were Earthquakes then Explosions 
table(earthq$Type)


#Let's check which years had most earthquakes 

yeardf<-earthq %>%
    filter(Type=="Earthquake",complete.cases(Year)==T) %>%
  #complete.cases==T to excllude NA values
    group_by(Year) %>%
    summarise(Count = n() , Mean_Magnitude = mean(Magnitude) )

#Converting Year column to numeric Type for plotting
yeardf$Year<-as.numeric(yeardf$Year)


ggplot(yeardf , aes(Year , Count)) + 
   geom_line(color="purple") + 
    geom_point(color="purple") + 
    scale_x_continuous(limits = c(1965,2016),breaks= seq(1965,2016,10))  + 
  labs(title="Time Series Plot of Year vs Number of Earthquakes",
       x = "Year " , y ="Number of Earthquakes")
  
#Highest number of Earthquakes in Year 2011  
  


#Time Series of Mean Magnitudes vs Year

ggplot(aes(x = Year , y = Mean_Magnitude),data = yeardf) + 
  geom_line(color="blue")  + geom_point(color="blue") + 
  scale_x_continuous(limits = c(1965,2016),breaks = seq(1965,2016,10))  + 
  labs(title="Time Series Plot of Year vs Mean of Magnitudes for that Year",
       x = "Year " , y ="Mean Magnitudes")
#Mean Magnitues of Earthquakes higher for years 1965 to 1970 after then dropped
  



#Distribution of Magnitudes Type
table(Magnitude_Type = na.omit(earthq$Magnitude.Type))

ggplot(aes(Magnitude.Type),data = earthq) + 
  geom_bar(aes(fill=Magnitude.Type,color=Magnitude.Type,alpha = 0.7)) + 
  labs(title="Distribution of Magnitude Types", x = "Type of magnitude" , y="Count")
#Highest for type MW

#Density Plot of Magnitude colored by its type- Smoothed version of Histograms
ggplot(aes(Magnitude),data = earthq) + 
  geom_density(aes(fill=Magnitude.Type,color=Magnitude.Type,alpha=0.001)) + 
  labs(title="Density Plot of Magnitude",y="Density")


#World Map of Earthquakes 

map <- ggplot(earthq) + borders("world", colour="#8C7450", fill="#F0CE23")  

print(map + geom_point(aes(x=earthq$Longitude, y=earthq$Latitude,color=Magnitude,size=Magnitude),shape=18) +
          scale_color_gradient(low="#715EEA", high="#FE012B") +
          theme(legend.position = "top")+
          ggtitle("World Map of Earthquakes by Magnitude")+labs(caption="--Made by Anish",x = "Longitude",
                                                   y="Latitude"))
