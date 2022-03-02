library(ggplot2)
library(car)

setwd("E:/R/Logistic Regression")

#1. Data load
data <- read.csv("telecom_customer_churn.csv",header = TRUE, stringsAsFactors = FALSE)
View(data)
head(data)
#2. Data sanity check
dim(data)
str(data)
summary(data)

#2.5 realizing categ & cont var
all_columns <- colnames(data)
cont_columns <- c("tenure","MonthlyCharges","TotalCharges" )
categ_columns <- c("gender","SeniorCitizen", "Partner", "Dependents", "PhoneService" ,
                   "MultipleLines" , "InternetService" ,"OnlineSecurity","OnlineBackup",
                   "DeviceProtection", "TechSupport","StreamingTV" ,"StreamingMovies" ,
                   "Contract","PaperlessBilling","PaymentMethod","Churn" )

#3. Check the missing value (if any)

function(x) {
  sum(is.na(x))
}

apply(data,2, function(x) sum(is.na(x))) #you can also use apply

## removing the rows with missing values
data<- na.omit(data)## removes the missing values


#4. variable type conversion if required

#5. Manual data exploration
#category vars
table(data[,c("Churn")])


table(data$gender)
table(data$Partner)

#continous vars
plot(data$MonthlyCharges)
plot(data$TotalCharges)
hist(data$TotalCharges)
hist(log10(data$TotalCharges))


#Variable creation, dummy and others
data$D_Contract_monthly <- ifelse(data$Contract=="Month-to-month" ,1,0)
data$D_Contract_biannual <- ifelse(data$Contract=="Two year" ,1,0)
data$D_Dependents_No <- ifelse(data$Dependents=="No" ,1,0)
data$D_DeviceProtection_No <- ifelse(data$DeviceProtection=="No" ,1,0)
data$D_DeviceProtection_NoServ <- ifelse(data$DeviceProtection=="No internet service" ,1,0)
data$D_InternetService_FO <- ifelse(data$InternetService=="Fiber optic" ,1,0)
data$D_InternetService_No <- ifelse(data$InternetService=="No" ,1,0)
data$D_OnlineBackup_No <- ifelse(data$OnlineBackup=="No" ,1,0)
data$D_OnlineSecurity <- ifelse(data$OnlineSecurity=="No" ,1,0)
data$D_PaperlessBilling <- ifelse(data$PaperlessBilling=="Yes" ,1,0)
data$D_Partner <- ifelse(data$Partner=="No" ,1,0)
data$D_PaymentMethod <- ifelse(data$PaymentMethod=="Electronic check" ,1,0)
data$D_PaymentMethod_CCA <- ifelse(data$PaymentMethod=="Credit card (automatic)" ,1,0)
data$D_StreamingMovies_No <- ifelse(data$StreamingMovies=="No" ,1,0)
data$D_StreamingTV_No <- ifelse(data$StreamingTV=="No" ,1,0)
data$D_TechSupport_No <- ifelse(data$TechSupport=="No" ,1,0)
data$D_Gender_M <- ifelse(data$gender=="Male" ,1,0)
#SELECT THE REQUIRED VARS
colnames(data)
data2 <- data[,-c(1,3,4,6:17)]

#PCA
colnames(data2)
pcaData <- data2[, -c(5)]
ncol(pcaData)
fit <- princomp(pcaData, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type="lines") #scree plot
data2$Factor9 <-  (0.588*data2$D_OnlineSecurity)+(-0.616*D_PaymentMethod)

#STEP8 VIF
colnames(data2)
fit <- lm(Churn ~. -D_InternetService_No , data= data2)
sort(vif(fit))
fit <- lm(Churn ~. -D_InternetService_No -MonthlyCharges , data= data2)
sort(vif(fit))
fit <- lm(Churn ~. -D_InternetService_No -MonthlyCharges-TotalCharges , data= data2)
sort(vif(fit))



#Training and Test data
set.seed(113)
test <-   sample(1:nrow(data2),nrow(data2)*0.2)
train <-  -test
data.train  = data2[train,]
data.test = data2[test,]
nrow(data.train)
nrow(data.test)

#Logistic Regression on full data

model <- glm(Churn ~ . -D_InternetService_No -MonthlyCharges-TotalCharges, data = data.train, family = binomial())
summary(model)
vif.output <- lm(Churn~., data=data.train)
sort(vif(vif.output))
alias(vif.output)

model <- glm(Churn~
SeniorCitizen+
  tenure+
  TotalCharges+
  D_InternetService_FO+
  D_OnlineSecurity+
  D_PaperlessBilling+
  D_PaymentMethod+
  D_StreamingMovies_No+
  D_TechSupport_No, data=data.train, family=binomial())
summary(model)
## Remove the insignificant variable

# Model Statistics

#Logistic regression is estimated by maximizing the likelihood function. 
#Let L0 be the value of the likelihood function for a model with no predictors, 
#and let LM be the likelihood for the model being estimated. 
#McFadden's R2 is defined as R2 = 1 - LM / L0
#McFadden's pseudo R2. It is also called rho-squared. 
#Those unfamiliar with rho-squared should be forewarned that its values tend to be considerably 
#lower than those of the R2 index...
#For example, values of 0.2 to 0.4 for rho-squared represent EXCELLENT fit. 

# McFadden's R2
LM <- model$deviance
L0 <- model$null.deviance
R2.MCF<- 1- LM/L0
R2.MCF



#model validation - concordance
# p(1)>p(0)		concordant pair
# p(1)<p(0)		discordant pair
# p(1)=p(0)		tied pair
# concordance = concordant pairs / total number of pairs		

Concordance <-  function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0}
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate))
} 

Concordance(model)


# get prob scores
data.train$Score <- predict(model,data.train,type="response")
View(data.train[,c("Churn","Score")])

data.test$Score <- predict(model,data.test,type="response")

#15 Measuring Accuracy of Model Predictions
#There are many ways to measure how well a statistical model predicts a binary outcome.  
#Three very common measures are  accuracy, sensitivity, and specificity.
# first let's understand the 4 possible outcomes of a model prediction - 
# True Negative (TN) - Actual is 0 and model also predicted 0
# True Positive (TP) - Actual is 1 and model also predicted 1
# False Positive (FP) - Actual is 0 and model predicted 1
# False Negative (FN) - Actual is 1 and model predicted 0
# When we put the above 4 possibilities in a matrix it is also called confusion matrix
# Sensitivity - % of 1's that we managed to capture i.e. TP / (TP+FN)
# Specificity - % of 0's that we managed to capture i.e. TN / (TN+FP) 
# Accuracy - % of correct classification i.e. (TP+TN)/(TP+TN+FP+FN)
# Now higher the all the 3 the better it is.
# Now everything depends on the threshold we use in our prob score to define 1 or 0. 
# But there is always a trade-off. 
# If the threshold is low it will increase sensitivity but decrease specificity and vice-versa
# It's important to understand this as you're choosing that threshold and evaluating a model.
# We should select a threshold that gives the best accuracy
# Best threshold can be obtained by using coords() function


# ROC curve
# ROC Curves are used to see how well your classifier can separate 
# positive and negative examples and to identify the best threshold for separating them.
# ROC curve is obtained by plotting sensitivity against 1-Specificity for many possible thresholds.
# Sensitivity is also called True Positive Rate. 1-Specificity is FP/(TN+FP) also called False Positive Rate i.e. how many i am wrongly classifying as 1 out of total possible wrongly classifiying 1
# So you want higher Sensitivity (or True Positive Rate) & Lower False Positive Rate (or 1-Specificity)
# Area under the ROC curve determines how good the model is. Higher the area the better it is.
install.packages('pROC')
library(pROC)
rocCurve   <- roc(response = data.train$Churn, predictor = data.train$Score, levels = c(0,1))
# Plot ROC curve
plot(rocCurve)

# Best threshold
coords(rocCurve,"best")

# Confusion metric
predclass <-ifelse(data.train$Score>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Churn)
Confusion

# Accuracy. Higher the better.
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
AccuracyRate

# Area under the ROC curve. Higher the better.
auc(rocCurve)

save("model",file ="mymodel.rdata")
load("mumodel.rdata")



#model validation - lift chart
Pctl_tbl<-as.vector(quantile(data.train[,"Score"], probs=c(.10,.20, .30,.40,.50, .60,.70, .80,.90)))
Pctl_tbl<-data.frame(c("P10","P20","P30","P40","P50","P60","P70","P80","P90"),Pctl_tbl)
colnames(Pctl_tbl)<-c("quantiles","Score")
View(Pctl_tbl)
data.train$Segment <-ifelse(data.train[,"Score"] > Pctl_tbl[Pctl_tbl$quantiles=="P90",2] , 10, 
                            ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P80",2], 9, 
                                   ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P70",2], 8,
                                          ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P60",2],7,
                                                 ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P50",2],6,
                                                        ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P40",2],5,
                                                               ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P30",2],4,
                                                                      ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P20",2],3,
                                                                             ifelse(data.train[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P10",2],2, 1)))))))))


library(gmodels)
LiftChartData<-data.frame(CrossTable(data.train$Churn,data.train$Segment) )
LiftChartData <- LiftChartData[LiftChartData$prop.col.x==1,c("t.Freq","prop.col.y")]
write.csv(LiftChartData, "LiftChartData.csv", row.names=FALSE)

table(data.train$Churn)

#model validation - lift chart for test
Pctl_tbl<-as.vector(quantile(test_data[,"Score"], probs=c(.10,.20, .30,.40,.50, .60,.70, .80,.90)))
Pctl_tbl<-data.frame(c("P10","P20","P30","P40","P50","P60","P70","P80","P90"),Pctl_tbl)
colnames(Pctl_tbl)<-c("quantiles","Score")

test_data$Segment <-ifelse(test_data[,"Score"] > Pctl_tbl[Pctl_tbl$quantiles=="P90",2] , 10, 
                           ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P80",2], 9, 
                                  ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P70",2], 8,
                                         ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P60",2],7,
                                                ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P50",2],6,
                                                       ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P40",2],5,
                                                              ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P30",2],4,
                                                                     ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P20",2],3,ifelse(test_data[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P10",2],                                                  2, 1)))))))))

LiftChartData<-data.frame(CrossTable(test_data$Responder,test_data$Segment) )
LiftChartData <- LiftChartData[LiftChartData$prop.col.x==1,c("t.Freq","prop.col.y")]
write.csv(LiftChartData, "LiftChartData.csv", row.names=FALSE)