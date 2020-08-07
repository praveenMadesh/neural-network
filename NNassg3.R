library(readr)
data <- read.csv(choose.files())
View(data)
data <- data[,-(1:2)]
#can see how size category divided
summary(data$size_category)

#can see how size category divided by graph
plot(data$size_category)

##data pratition into train and test 
data_train <- data[1:385,]
data_test <- data[386:517,]

##Building the model
fire_model <- neuralnet(size_category~.,
                        data = data_train,
                        hidden = 1)
str(fire_model)
plot(fire_model)
model_result <- compute(fire_model,data_test[1:29])
predicted_forestfire <- fire_model$net.result
predicted_forestfire##predicted values 

