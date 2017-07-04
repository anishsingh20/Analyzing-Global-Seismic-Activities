#Applying Deep learning to predict the Magnitude of Earthquakes
require(keras)
require(dplyr)
require(lubridate)
#Data Preprocessing
str(earthq)


#Generating new Dataframes with only appropiate Variables
earth_new<-earthq %>% select(Date,Time,Type,Latitude,Longitude,Depth,Magnitude)


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

earth_new$Type<-as.numeric(as.factor(earth_new$Type))

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
earth.train<-as.matrix(earth_new[rand==1,1:6])
earth.trainY<-as.matrix(earth_new[rand==1,7])


#Test Data
earth.test<-as.matrix(earth_new[rand==2,1:6])
earth.testY<-as.matrix(earth_new[rand==2,7])


#Model Defination

model1<-keras_model_sequential()


#Defining the Architecture of the MLP Model

model1 %>% layer_dense(units=5,activation="relu",input_shape=c(6)) %>%
  layer_dense(units=1) 
#No activation for output layer because we only want the numeric values
summary(model1)
get_config(model1)
get_layer(model1,index = 1)


optrms<-optimizer_rmsprop(lr=0.01) 
#compiling the Model
model1 %>% compile(loss="mean_squared_error",optimizer=optrms,metrics="accuracy")
#Loss function is mean squared error bacause of Regression Problem as we want to predict
#numeric values


#Fitting the Model ---------------------

history<- model1 %>% fit(earth.train,earth.trainY,epochs=50,batch_size=128,
                        validation_split=0.2,verbose=2,
                        callbacks= callback_tensorboard(log_dir = "logs/run_a",write_graph=T,
                        histogram_freq=1))




#evaluating the Model

score<-model1 %>% evaluate(earth.test,earth.testY,batch_size=120)
score


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

#Converting Training and Test data to Matrix
earth.train<-as.matrix(earth_new[rand==1,1:6])
earth.trainY<-as.numeric(as.factor(earth_new[rand==1,7]))

#Test Data
earth.test<-as.matrix(earth_new[rand==2,1:6])
earth.testY<-as.numeric(as.factor(earth_new[rand==2,7]))



#Converting the Labels to numeric values
earth.testY<-as.numeric(earth.testY)
earth.trainY<-as.numeric(earth.trainY)



#Converting Labels to One Hot Encoding
earth.trainY<-to_categorical(earth.trainY)
earth.testY <-to_categorical(earth.testY)

#Defining the MLP Model

model<-keras_model_sequential()


#making a deeper Model with more layers
model %>% layer_dense(units = 8 , activation = "relu" , input_shape=c(6)) %>%
          #Output Layer
          layer_dense(units=4,activation = "softmax" )


summary(model)

#Compiling the Model

opt<-optimizer_rmsprop(lr=0.01)
  
model %>% compile(loss = 'categorical_crossentropy'
                  , optimizer=opt,
                    metrics='accuracy')



history<-model%>%fit(earth.train,earth.trainY,epochs=20,batch_size=10,
                         validation_split=0.2,verbose=1,
                         callbacks= 
                         callback_tensorboard(log_dir = "logs/run_c",write_graph=T)
                                                         )

#Visualizing the Model's Metrics and Architecture
tensorboard()


#Plotting Metrics

plot(history$metrics$acc,col="blue",type="l",xlab="Epochs",ylab="Accuracy",main="Epochs vs Accuracy")
lines(history$metrics$val_acc,col="red",type="l")


plot(history$metrics$loss,col="red",type="l",xlab="epochs",ylab="Loss",main="Epochs vs Loss") 
lines(history$metrics$val_loss,col="blue",type="l")



#Evaluating the Model on Test Data

score<- model %>% evaluate(earth.test,earth.testLabels,batch_size=10)

score





