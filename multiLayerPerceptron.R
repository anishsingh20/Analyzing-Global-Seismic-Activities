#Applying Deep learning to predict the Magnitude of Earthquakes
require(keras)
require(dplyr)
require(lubridate)
#Data Preprocessing
str(earthq)


#Generating new Dataframes with only appropiate Variables
earth_new<-earthq %>% select(Date,Time,Latitude,Longitude,Type,Depth,Magnitude)

#Converting Type to Numeric data
earth_new$Type<-as.factor(earth_new$Type)
#to_categorical converts it to  OHE- one hot encoding
earth_new$Type <- as.numeric(earth_new$Type)

#converting Date and Time to Numeric Type 
#factors are just a data type in R which are simply categorical values
earth_new$Time<-as.factor(earth_new$Time)
earth_new$Time<-as.numeric(earth_new$Time)
earth_new$Date<-as.factor(earth_new$Date)
earth_new$Date<-as.numeric(earth_new$Date)

#Converting data frame to a Matrix
earth_new<-as.matrix(earth_new)

#Getting the Dimentions of the Matrix
cat("The Dimentions of the matrix is", dim(earth_new))


#Normalizing the Data i.e the inputs
earth_new[,1:6]<-normalize(earth_new[,1:6])
summary(earth_new)


#Saperating Test and Training Data
#generating Sample data
rand<-sample(2,nrow(earth_new),replace = T,prob=c(0.67,0.33))

#Splitting the data
earth.train<-earthq[rand==1,1:6]