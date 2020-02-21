#Linear Regession Example1

#I have imported a data named WC_AT.csv
#This data contains sample values of the Waist size and the AT Area where AT stands for Adipose Tissue
#The At value was obtained by a CT Scan for all the people with different Waist Circumference.
#As CT scan is very Expensive and very less Hospitals have Access to this CT scan Equipment.
#So we have come up with a prediction model that will give the AT value when we have the waist Circumference value

data1 <- WC_AT

plot(data1) #Plotting the Dataset

#Here AT value is dependent on Waist value so we take Waist on the X-Axis and AT on the Y-Axis 
model1 <- lm(AT~Waist, data = data1) #Creating a model with the data 
summary(model1) #Summary of the model

#Visualization
dotplot(data1$Waist, main = "Waist size of the People")

dotplot(data1$AT, main = "Adipose Tissue of the people")

boxplot(data1$Waist, col = "cyan")

boxplot(data1$AT, col = "red")

pred <- predict(model1) #predict the values of the dataset 
pred

temp <- data.frame(data1,pred , "Errors" = data1$AT - pred) #This will make a new dataframe with a column error which will have the subtracted values of the AT value and the Predicted At value


#Lets predict the AT value for the Waist size 70 , 60

newdata1 <- data.frame(Waist = c(60,70)) #Here we have added waist values 60 and 70
pred2 <- predict(model1, newdata = newdata1) #Here we predict the AT area for the waist sizes 60 and 70  
pred2
