#Applying Deep learning to predict the Magnitude of Earthquakes
require(keras)
require(dplyr)
require(lubridate)
#Data Preprocessing
str(earthq)
earthq$Date<-as.Date(earthq$Date)

#Generating new Dataframes with only appropiate Variables
earth_new<-earthq %>% select(Date,Time,Latitude,Longitude,Type,Depth,Magnitude)

#Converting data frame to a Matrix
earth_new<-as.matrix(earth_new)

#Converting Type to Numeric data
earth_new$Type <- to_categorical(earth_new$Type)

#converting Date and Time to Numeric Type
earth_new$Date<-as.numeric(ymd_hms(earth_new$Date))


#Normalizing the Data
earth_new<-normalize(earth_new[,1:6])


#Saperating Test and Training Data
#generating Sample data
rand<-sample(2,nrow(earth_new),replace = T,prob=c(0.67,0.33))

#Splitting the data
earth.train<-earthq[rand==1,1:6]