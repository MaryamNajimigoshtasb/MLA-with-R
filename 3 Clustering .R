###
setwd('E:/R/3 Clustering 1.5 Hr')
install.packages('amap')
library(amap)
##Read the data in the file
cust_data<-read.csv("Insurance_Dataset_Clustering_Analysis.csv")

View(cust_data)
colnames(cust_data)
### Select the requried columns for clustering
cust_data<-cust_data[,c(2,4,5,7,9,10,13)]
###Verify the data
colnames(cust_data)
head(cust_data)

fn_norm <- function(var){
  cust_data[,var] <- (cust_data[,var] - min(cust_data[,var]))/(max(cust_data[,var])-min(cust_data[,var])) 
  return(cust_data[,var])
}

cust_data$N_Age <- fn_norm("Age")
cust_data$N_Years.of.Driving.Experience <- fn_norm("Years.of.Driving.Experience")
cust_data$N_Number.of.Vehicles <- fn_norm("Number.of.Vehicles")
cust_data$N_Vehicle.Age <- fn_norm("Vehicle.Age")
colnames(cust_data)
selected_cols <- c(4,5,7:11)

set.seed(3)
test =  sample(1:nrow(cust_data),nrow(cust_data)/10)
train = -test
training_data = cust_data[train,]
testing_data = cust_data[test,]

# Estimating the number of clusters
install.packages('NbClust')
library(NbClust)
nb <- NbClust(testing_data[,selected_cols], distance = "euclidean", min.nc = 2,
              max.nc = 4, method = "complete", index ="all")

###Run the kmeans algorithm to generate the clusters
k1<-kmeans(training_data[,selected_cols], 3, iter.max = 200, nstart = 1)
###See the clustering results
###Fetch the group means for each variable
k1$centers
###Fetch size/n of obs for the groups
k1$size
###Fetch the cluster for each obs
training_data$Segment <- k1$cluster
aggregate(.~Segment,data=training_data,mean)





