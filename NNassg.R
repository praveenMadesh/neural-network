library(readr)
data <- read.csv(file.choose()) #data set used 50_startups
View(data)
str(data)
data <- data[,-4] ### all column shoule be in numerical 
View(data)
plot(data$Profit)###direction of data is -ve and linear 
hist(data$Profit)##should normalize the data
norm <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

norm_data <- as.data.frame(lapply(data, FUN = norm))
summary(norm_data$Profit) 
hist(norm_data$Profit) ###now data is normalized

###data partition
training <- norm_data[1:38,]
testing <- norm_data[39:50,]

##building neural network model
library(neuralnet)
library(nnet)
profit_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+Profit,
                           data = training,
                           hidden = 1)##by changing hidden value we can see the hodden nodes
#and also it create more steps in neural networks
plot(profit_model) ##by this we can see only one hidden node by useing hidden value is 1

#profit_model1 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+Profit,
#                          data = training,
#                          hidden = 5)##by changing hidden value 5 we can see more the hidden nodes
#plot(profit_model1)###the hidden nodes are 5 ,can change that values

####prediction
output <- compute(profit_model,testing[])##computing the neural network model to the testing data
pred_profit <- output$net.result
pred_profit ###predicted values
cor(pred_profit,testing$Profit)##0.9615
plot(pred_profit,testing$Profit)
