library(gmodels) #installs the library named gmodels that contains various computations functions

data<-nycflights13::flights #assigns the name data to the dataset

data #views the data

data$dep_time #dispays the data of the column dep_time

dep_delay<-data$dep_delay #creates a data frame named dep_delay that contains the values of the column dep_delay of the dataset

ar_delay<-data$arr_delay #creates a data frame named ar_delay that contains the values of the column arr_delay of the dataset

dep_delay1<-dep_delay[!is.na(dep_delay)] #removes the NA values from the dataset

ar_delay1<-ar_delay[!is.na(ar_delay)] #removes theNA values from the dataset

ci(dep_delay1) #ci command is used to find the confidence intervals of the particular dataset

ci(ar_delay1) #ci command is used to find the confidence intervals of the particular dataset
 