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


require(dplyr)
require(ggplot2)

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
    summarise(Count = n() , Mean_Magntidue = mean(Magnitude) )



ggplot(yeardf , aes(as.numeric(Year) , Count)) + 
   geom_line(color="red") + 
    geom_point(color="red") + 
    scale_x_continuous(limits = c(1965,2016),breaks = seq(1965,2016,10))
  
#Highest number of Earthquakes in Year 2011  
  






