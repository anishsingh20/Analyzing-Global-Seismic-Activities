#Applying Deep learning to predict the Magnitude of Earthquakes
require(keras)
require(dplyr)
require(lubridate)
#Data Preprocessing
str(earthq)


#Generating new Dataframes with only appropiate Variables
earth_new<-earthq %>% select(Date,Time,Latitude,Longitude,Depth,Magnitude,Type)


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

#Setting the seed for reproducable results
set.seed(1212)

#Training Data
earth.train<-earth_new[rand==1,1:6]
earth.trainY<-earth_new[rand==1,7]


#Test Data
earth.test<-earth_new[rand==2,1:6]
earth.testY<-earth_new[rand==2,7]


#Model Defination

model<-keras_model_sequential()


#Defining the Architecture of the MLP Model

model %>% layer_dense(units=12,activation="relu",input_shape=c(6)) %>%
  layer_dense(units=1) 
#No activation for output layer because we only want the numeric values

get_config(model)
get_layer(model,index = 1)


#compiling the Model
model %>% compile(loss="mean_squared_error",optimizer="adam",metrics="accuracy")
#Loss function is mean squared error bacause of Regression Problem as we want to predict
#numeric values


#Fitting the Model ---------------------

history<- model %>% fit(earth.train,earth.trainY,epochs=50,batch_size=10,
                        validation_split=0.2,verbose=1,
                        callbacks= callback_tensorboard(log_dir = "logs/run_a",write_graph=T,
                        histogram_freq=1))


#Fine tuning the MLP Model for producing better Results and accuracy


#second run with different Parameters
history1<- model %>% fit(earth.train,earth.trainY,epochs=200,batch_size=50,
                         validation_split=0.2,verbose=1,
                         callbacks= callback_tensorboard(log_dir = "logs/run_a",write_graph=T,
                                                         histogram_freq=1))


#Saving the First MLP model
save_model_hdf5(model,"MLP1.h5") 



#--------------------------------------------------------------------------------


#Another Deeper Model----------------

#Trying to do classification to Predict the type of Siesmic Activity

#Saperating Training and Test Data

#converting Date and Time to Numeric Type 
#factors are just a data type in R which are simply categorical values
earth_new$Time<-as.factor(earth_new$Time)
earth_new$Time<-as.numeric(earth_new$Time)
earth_new$Date<-as.factor(earth_new$Date)
earth_new$Date<-as.numeric(earth_new$Date)

earth.train<-earth_new[rand==1,1:6]
earth.trainY<-earth_new[rand==1,7]

#Test Data
earth.test<-earth_new[rand==2,1:6]
earth.testY<-earth_new[rand==2,7]

#Converting Labels to One Hot Encoding
earth.trainY<-to_categorical(earth.trainY)
earth.testY <-to_categorical(earth.testY)

#Defining the MLP Model

model<-keras_model_sequential()


#making a deeper Model with more layers
model %>% layer_dense(units = 32 , activation = "relu" , input_shape=c(6)) %>%
          layer_dense(units=10 ,activation="relu") %>%
          #Output Layer
          layer_dense(units= )


summary(model)

#Compiling the Model

model %>% compile(loss = "mse",optimizer="adam",
                   metrics='accuracy')



history<-model%>%fit(earth.train,earth.trainY,epochs=300,batch_size=5,
                         validation_split=0.2,verbose=1,
                         callbacks= 
                         callback_tensorboard(log_dir = "logs/run_a",write_graph=T)
                                                         )







