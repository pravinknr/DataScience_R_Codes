#Neural Networks Algorithm


install.packages("neuralnet")
concrete <- read.csv("C:\\Users\\pravi\\Desktop\\R practice\\Supervised Machine Learning Techniques in R\\Neural Networks\\concrete.csv")

#Derive the Normalization Function
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

#Apply Normalization to entire data frame

concrete_norm <- as.data.frame(lapply(concrete, normalize))

#Creating Training and Test Data
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

##Training a model on the data
#Train the Neural Network model

library(neuralnet)

#Simple ANN (Artificial Neural Network) with a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)

#Visualize the Network Topology
windows()
plot(concrete_model)

##Evaluating Model Performance
#Obtain Model Results

model_results <- compute(concrete_model, concrete_test[1:8])

#Obtain Predicted Strength Values
predicted_strength <- model_results$net.result

#Examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

#Improving Model Performance
#A more complex Neural Network Topology with 5 Hidden Neurons 
concrete_model1 <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = c(5,2))

#plot the Network
windows()
plot(concrete_model1)

#Evaluate the results as we did before

model_results1 <- compute(concrete_model1, concrete_test[1:8])

predicted_strength1 <- model_results1$net.result

cor(predicted_strength1, concrete_test$strength)
