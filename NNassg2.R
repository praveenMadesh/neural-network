library(readr)
concrete <- read.csv(file.choose())##data set concrete
View(concrete)
str(concrete)
attach(concrete)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}### to normalized data 
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$strength)

summary(concrete$strength)

###data partition into train and test 
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

library(neuralnet)
library(nnet)
# Building model
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
                            data = concrete_train,
                            hidden = 2)##By changing hidden value 
#we can see the 2 nodes in between x and y that is independed variables and depended variable

str(concrete_model)
plot(concrete_model)

model_results <- compute(concrete_model,concrete_test[1:8])# compute function to generate ouput for the model prepared
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)

