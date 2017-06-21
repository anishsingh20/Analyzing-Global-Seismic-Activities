#Seismic Data Analysis


#loading the required Packages
require(dplyr)
require(ggplot2)
library(maps)
library(fpp)
library(astsa)
library(lubridate)

#loading the dataset
earthq<-read.csv('database.csv',stringsAsFactors = FALSE)
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
  geom_area(aes(y = ..count..),fill="purple", stat = "bin",alpha=0.7)+
  labs(title="Earthquakes",caption="Anish") + 
  guides(fill=FALSE)
 
by(earthq$Magnitude,earthq$Type,summary)
#Highest Median Magnitude of Rock Burst

ggplot(aes(x = Type, y = Magnitude),data = earthq) + 
  geom_boxplot() + 
  coord_trans(y = "log10") + 
  labs(title="Boxplot of Type vs Magnitude",x = "Type of Seisic Activity", y = "Magnitude")


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
       x = "Year " , y ="Mean Magnitudes")+ 
  scale_y_continuous(breaks=seq(5.4,7,0.05))
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

print(map + geom_point(alpha=0.7,aes(x=earthq$Longitude, y=earthq$Latitude,color=Magnitude,size=Magnitude),shape=18) +
          scale_color_gradient(low="#715EEA", high="#FE012B") +
          theme(legend.position = "top")+
          ggtitle("World Map of Earthquakes by Magnitude")+labs(caption="--Made by Anish",x = "Longitude",
          y="Latitude"))



#Analysing All Seismic Activities having Magnitude >= 7 


#making a world map of only Seismic activities above Magnitude 7
HighMagnitude<-earthq %>%
                filter(Magnitude >=7,complete.cases(Year)==T,Type=="Earthquake")

#Histogram of High Magnitude Seismic Activities colored by Type
ggplot(aes(Magnitude),data = HighMagnitude) + 
  geom_histogram(color="black",
    alpha=0.7,aes(fill=Magnitude.Type),bins=30) + 
  labs(title="Histogram of High Magnitudes Earthquakes and its Types")

#Density Plot
ggplot(aes(Magnitude),data = HighMagnitude) + 
  geom_density(aes(color=Magnitude.Type,fill = Magnitude.Type ,alpha=0.7)) +
  labs(title="Density Plot of High Magnitude Earthquakes colored by Type")
                            
#High Magnitude Siesmic Activities are all of type Earthquake
table(HighMagnitude$Type)

#Choosing Median becuase it is not skewed and remains unaffected by Noise
Highdf<- HighMagnitude %>% 
        group_by(Year) %>%
        summarise(Count  = n() , Median_Magnitude=median(Magnitude))

#Converting Year Variable to numeric value
Highdf$Year<-as.numeric(Highdf$Year)

#Time Series of Year vs Count

ggplot(aes(Year , Count),data  = Highdf) +
  geom_line(color="#660066") +
   geom_point(color="#660066",shape=19,size=3) + 
    labs(title="Time Series of number of High Magnitude Earthquakes",
         x="Year",y ="Number of Earthquakes") + 
      scale_x_continuous(breaks=seq(1965,2016,5))
    

#Time series of Median Magnitude

ggplot(aes(Year , Median_Magnitude),data  = Highdf) +
  geom_line(color="#ff0066") +
  geom_point(color="#ff0066",shape=19,size=3) + 
  labs(title="Time Series of number of High Median Magnitudes of Earthquakes ",
       x="Year",y ="Median Magnitudes") + 
  scale_x_continuous(breaks=seq(1965,2016,5))
#There are lots of fluctuations in the plot over time but highest Median Magnitude
#was recorded for year 1986


#Plotting of High Magnitude Earthquake Sites on the World Map

map2 <- ggplot(HighMagnitude) + borders("world", colour="black", fill="#1ab2ff")  

print(map2 + geom_point(aes(x=Longitude,y=Latitude,color=Magnitude,size=Magnitude),shape=18) +
        scale_color_gradient(low="#00ff00", high="#FE012B") +
        theme(legend.position = "top")+
        ggtitle("Earthquakes Sites With Magnitude >= 7 ")+labs(caption="--Made by Anish",x = "Longitude",
                                                              y="Latitude"))

#Regions Having lots of High Magnitude Earthquakes are Africa and Japan and some portions
#of Asia below India






#Creating Animated Plots
#Convert the dates into character in order to split the coloumn into "dd" "mm" "yy"" columns

#considering Earthquakes above Magnitude 6.5
Newdf<-earthq %>%
  filter(Magnitude >=6.5)


Newdf$Date<-as.character(Newdf$Date)



## Splitting the date into 3 new columns   
Newdf$Year <- format(as.Date(Newdf$Date, format="%d/%m/%Y"),"%Y")
Newdf$Month <- format(as.Date(Newdf$Date, format="%d/%m/%Y"),"%m")
Newdf$Day <- format(as.Date(Newdf$Date, format="%d/%m/%Y"),"%d")


#Change the Year column to numeric
Newdf$Year=as.numeric(Newdf$Year)


#Getting the world map for plot and load the necessary package


library(ggmap)

#Loading the World map as a data frame to use in ggplot
world<-map_data("world")

#Removing Antarctica region from the world map
world <- world[world$region != "Antarctica",]

map<-ggplot()+geom_map(data=world,map=world,aes(x=long,y=lat,map_id=region),
                       color='#333300',fill='#663300')


#Plotting points on world Map


