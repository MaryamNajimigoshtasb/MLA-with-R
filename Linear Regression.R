setwd("E:/data/R2 even/Linear Regression/Case Study Files")

install.packages("lubridate")

library(lubridate)



#1. Read data files for cust and their transactions in a file 

LinRegData <- read.csv(file = "kansas_city_house_sales.csv", stringsAsFactors = F)

ncol(LinRegData)

View(LinRegData)

head(LinRegData)

tail(LinRegData)

str(LinRegData)



#2. Change variable format

# regexpr("y","jaydeep")

pos <- regexpr('T',LinRegData$date)   #  regexpr('T','HKHKJTKJTLJK')

unique(pos)

LinRegData$date <- substr(LinRegData$date,1,unique(pos)-1)    # substr('jaydeep',4,7)

LinRegData$date <- as.Date(LinRegData$date, format='%Y%m%d')

sum(is.na(LinRegData$date))   #length(which(is.na(LinRegData$date)))

str(LinRegData)





#3. dependant variable exploration

plot(LinRegData$price)

options(scipen = 20)

plot(LinRegData$price)



# my interpretation is that there are ouliers

# Outliers are bad for models. They cause more errors in models.

# Take a look at the outliers, try to figure out why that is happening

# Once you realize, even if the oyliers are there for a good reason, you must still

# remove them to build a better SCOPE.

# So if your model is built on a smaller scope like - price of house is betwen 100K to 1Mn

# will it be able to predict price of house beyond this scope



# Lets explore the house with max price & compare it with an avg price house

View(LinRegData[ LinRegData$price ==  max(LinRegData$price) | LinRegData$price ==  660000, ])



hist(LinRegData$price) 

# we see a skewed histogram due to a very high range of proce AND because majority of the houses have low price

# but we need a normal histogram for our DV

# to get that we need to transform the usual price of house to log(price of house)

hist(log10(LinRegData$price))

# We will use log(price) as our DV and not the usual price

plot(log10(LinRegData$price))

# we will still need to remove the outliers



# How do we decide what is the cutoff for high price outliers and for low price outliers

# You can use standard benchmarks like - Mean + 3*Std Dev (for higher outlier cutoff)

# Mean - 3* Std Dev (for lower outlier cutoff)

# for our case lets find out these ouliers

mean(LinRegData$price) + (3*sd(LinRegData$price))  # high outlier cutoff is 1.6Mn

mean(LinRegData$price) - (3*sd(LinRegData$price))  # low outlier cutoff is -ve what that means there are no low outliers basis this technique

# Another standard way is you remove every obs more than 99.9%ile value

# and similarly less than 0.1%ile value

quantile(LinRegData$price, 0.999)  # high outlier cutoff is 3.5Mn

quantile(LinRegData$price, 0.001)  # low outlier cutoff is 95K

# there are other methods like tuckey fences



# Explore outliers to decide the cutoff

library(ggplot2)

x_data <- c(.90, .95, .96, .97, .98, .99, 1)

y_data <- round(quantile(LinRegData$price, x_data),0)

qplot(x = x_data, y = y_data,label = round(y_data/1000000,1), geom=c("text","point"), hjust=-0.25)



# Now we will explore all values beyond 2 Mn price

hist(LinRegData[LinRegData$price>2000000,'price'])

plot(LinRegData[LinRegData$price>2000000,'price'])

sum(LinRegData$price>3000000)

# so 3 mn price will be the higher outlier cutoff



# Now we will explore for lower value cutoff

x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)

y_data <- round(quantile(LinRegData$price,x_data ),1)

qplot(x = x_data, y = y_data,label = round(y_data/1000,1), geom=c("text","point"), hjust=-0.25)

hist(LinRegData[LinRegData$price<150000,'price'])

plot(LinRegData[LinRegData$price<150000,'price'])

sum(LinRegData$price<100000)



# finally remove the houses which are outliers

LinRegData <- LinRegData[LinRegData$price>100000 & LinRegData$price<3000000,]



# Create the of log of price, which will be new DV

LinRegData$price_log <- log10(LinRegData$price)



#4. independant variable exploration

# We will perform uni-variate analysis, which means we will pick up one variable at a time

# and explore that through either frequency distribution tables or histograms & plots

# Why are we doing Uni Variate?

# We are doing this to find if any variable needs - 

# a) outlier treatement

# b) missing value treatement

# c) Any transformation (like log) is needed

# d) any data type conversion

# e) if there is enough variation in the variable

# f) to combine certain values into a single value (banding/ grouping)



# We start with date

table(year(LinRegData$date))   #The year function is provided by lubridate(). Other similar functions are month() & day()

table(month(LinRegData$date))

table(paste0(year(LinRegData$date), "-", month(LinRegData$date)))

min(LinRegData$date)

max(LinRegData$date)

plot(table(month(LinRegData$date)))

max(LinRegData$date)

# we leave date as it is. no change required.



# bedrooms

table(LinRegData$bedrooms)   

View(LinRegData[LinRegData$bedrooms==33,])

# this looks like a case of typo. So we will update this to 3.

LinRegData[LinRegData$bedrooms==33,c(4)] <- 3   # id - 2402100895

View(LinRegData[LinRegData$id==2402100895,])



# bathrooms

plot(table(LinRegData$bathrooms))

unique(LinRegData$bathrooms)



# sqft living area

plot(LinRegData$sqft_living) 

hist(LinRegData$sqft_living) 

# We can see that there are ouliers

# here again we see a very high range and a skewed histogram

# so once again we will try to take a log transformation and see if we can get a normal distribution

# if we get a normal distribution then we can keep this new var in the model



hist(log10(LinRegData$sqft_living))

# so we will be using log of sqft_living



# now lets explore for high outliers

x_data <- c(.90, .95, .96, .97, .98, .99, 1)

y_data <- round(quantile(LinRegData$sqft_living, x_data),0)

qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)

# lets zoom in beyond 4800 sq ft

hist(LinRegData[LinRegData$sqft_living>4800,'sqft_living'])

# so higher outlier chosen as 8000 sq ft, what is the total count of such houses

sum(LinRegData$sqft_living>8000)



# now we check for lower outliers

x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)

y_data <- round(quantile(LinRegData$sqft_living, x_data),0)

qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)

# lets zoom in less than 700 sq ft

hist(LinRegData[LinRegData$sqft_living<700,'sqft_living'])

# so higher outlier chosen as 500 sq ft, what is the total count of such houses

sum(LinRegData$sqft_living<500)



# apply the outliers to filter the data

LinRegData <- LinRegData[LinRegData$sqft_living>500 & LinRegData$sqft_living<8000,]



# lets create the new column for log of sqft living

LinRegData$sqft_living_log <- log10(LinRegData$sqft_living)



# so lets now visualize the new histogram

hist(LinRegData$sqft_living_log)





# sqft lot

plot(LinRegData$sqft_lot) # there are outliers

hist(LinRegData$sqft_lot) # lets try log transformation

hist(log10(LinRegData$sqft_lot))



# exploring the higher outliers

x_data <- c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)

y_data <- round(quantile(LinRegData$sqft_lot, x_data),1)

qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)

# exploring houses with sqft lot more than 200K

hist(LinRegData[LinRegData$sqft_lot>200000,'sqft_lot'])

# so higher outlier chosen as 500K sq ft, what is the total count of such houses

sum(LinRegData$sqft_lot>500000)



# Exploring the lower outliers

x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)

y_data <- round(quantile(LinRegData$sqft_lot, x_data),1)

qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)

# so lower outlier chosen as 1K sq ft, what is the total count of such houses

sum(LinRegData$sqft_lot<1000)

# there are still 200 houses less than 1K sq ft, so we will explore further

hist(LinRegData[LinRegData$sqft_lot<1000,'sqft_lot'])

# lets look at the count of houses less than 700 sqft

sum(LinRegData$sqft_lot<700)

# so lower cut off is 700 sq ft



# so lets now filter out the relevant houses

LinRegData <- LinRegData[LinRegData$sqft_lot<500000 & LinRegData$sqft_lot>700,]



# make the log transformation of sqft lot

LinRegData$sqft_lot_log <- log10(LinRegData$sqft_lot)



# finally visualize this to check of everything is fine

hist(LinRegData$sqft_lot_log)



# floors

table(LinRegData$floors)

# now if we believe that beyond 2 floors there is no incremental price as, it increases cost of 

# maintainence & cleaning, so you can create a floor band. And you can try this new var in the model

# and you can decide if your hypothesis was right basis the model result. 

LinRegData$floors_band <-ifelse(LinRegData$floors>2,2,LinRegData$floors)

table(LinRegData$floors_band)



# Waterfront

table(LinRegData$waterfront)

# so here we develop a hypothesis that waterfront might not be a significant var as it is almost constant

# but you put the var in the model and let the model decide. 

# another approach can be - remove all the rows with a waterfront and build the model. 

# see if this is a better model. If it is then you can retain this strategy



# View

table(LinRegData$view)

# View is not numeric, although it has numbers in it. It is not continous and you can not take an avg

# there is no avg view. So you can not leave it like this. 

# now if you want to use a categorical var (like gender / marital status) in a model

# then you have to create DUMMY VARIABLES. We will learn this later

# Right now we can only represent it through a band with 0 & 1 value.

LinRegData$view_band <-ifelse(LinRegData$view>0,1,0)

# remember that in this case u can only use the view_band and not the original variable
# Condition
# Univariate anlaysis for condition - 
# condition is continous as you can take the avg of condition - so yo ucan make the histogram & plot
# condition has less number of distinct values so you can also look at the freq distribution and consider a table function
table(LinRegData$condition) 
# so over here, we can see that there are only 202 houses with condition less than 3, we are removing them to redefine the scope
sum(LinRegData$condition<3)
LinRegData <-LinRegData[LinRegData$condition>2,]

# Grade
# And grade will be considered similar as condition. Again we can use a histogram as it is continous & we can take an average
# at the same time as grade as less distinct values, we can use a table as well.
table(LinRegData$grade)
hist(LinRegData$grade)
# so here we combine the grades less thna 6 with 6 and more than 10 with 10
# this is my hunch, but create any variable that you feel might help and model decide if it is good or bad
LinRegData$grade_band <-ifelse(LinRegData$grade<6,6,ifelse(LinRegData$grade>10,10,LinRegData$grade))
table(LinRegData$grade_band)
# Sqft Above
plot(LinRegData$sqft_above)   
hist(LinRegData$sqft_above)   
# there are outliers
# it might help to take a log
hist(log10(LinRegData$sqft_above))
# we can see that log10 gives me normal distribution

# lets now cosider higher outliers
x_data <- c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
y_data <- round(quantile(LinRegData$sqft_above, x_data),1)
library(ggplot2)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)

# lets zoom beyond 4000 sqft
hist(LinRegData[LinRegData$sqft_above>4000,'sqft_above'])
# looks like 6500 is a good high outlier cutoff. lets count the number of houses there
sum(LinRegData$sqft_above>6500)

# Now lets look at the lower outliers
x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
y_data <- round(quantile(LinRegData$sqft_above, x_data),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
# lets zoom less than 700 sq ft
hist(LinRegData[LinRegData$sqft_above<700,'sqft_above'])
# so we consider 450 sqft as lower outlier cutoff. lets count teh number of houses
sum(LinRegData$sqft_above<450)

# so now lets redefine the scope by considering houses between 450 to 6500 sq ft
LinRegData <- LinRegData[LinRegData$sqft_above>450 & LinRegData$sqft_above<6500,]

# sqft basement
plot(LinRegData$sqft_basement)
# looks like there are outliers
hist(LinRegData$sqft_basement) 
# So here even though it looks like a case of log transformation, we can not do that
# as a lot of houses has 0 basement area and log10(0) id -Inf. So only thing that you can do is take care of outliers

# lets consider higher outliers
x_data <- c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)
y_data <- round(quantile(LinRegData$sqft_basement, x_data),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)

# lets zoom beyond 1600
hist(LinRegData[LinRegData$sqft_basement>1600,'sqft_basement'])
# looks like 3K sqft is a good cutoff. Lets count the cases
sum(LinRegData$sqft_basement>3000)

# Latitude
# It is continous. becasue anyways we are doing all sorts of maths on this. We are using it calculate distance
# we explore this like any other continous var - plot / hist
plot(LinRegData$lat)
# may be on the lower side there are a few ouliers
hist(LinRegData$lat)
# there is a slight skew. but log will not help, because log works only when the range is very high.
# log helps us reduce the range
hist(log10(LinRegData$lat))

# for lower outliers lets check for the houses less than 47.2
hist(LinRegData[LinRegData$lat<47.2, "lat"])
# lets consider 47.19 as the cutoff. remember that even a very small change in this number will mean
# change of several kilometers and it might indicate a house in a forest or outskirts etc.
sum(LinRegData$lat>=47.19)

# So improving the scope by removing these houses
LinRegData <- LinRegData[LinRegData$lat>=47.19,]

# Longitude
# same as latitude. lets look at plot & hist
plot(LinRegData$long)
# looks like there are definitely higher outliers and possibly lower as well
hist(LinRegData$long)
# we can not take a log on this for 2 reasons - 
# 1. the numbers are negative (you can not take log of negative numbers)
# 2. The range is not high. So just like lat, even if you take a log it will not help
LinRegData$long <- abs(LinRegData$long)
hist(LinRegData$long) # so now the outliers are on the lower side
hist(log10(LinRegData$long)) # it doesnt help, bcoz the range is small
# lets check for lower outliers
x_data <- c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
y_data <- round(quantile(LinRegData$long, x_data),1)
qplot(x = x_data, y = y_data,label = y_data, geom=c("text","point"), hjust=-0.25)
# lets zoom beyond 121.8
hist(LinRegData[LinRegData$long<121.8,'long'])
# 121.7 looks like a good cutoff. how many houses.
sum(LinRegData$long<121.7)

# refining the scope basis this cutoff
LinRegData <- LinRegData[LinRegData$long >= 121.7,]

#5. Create derived variables
LinRegData$Sales_year <- year(LinRegData$date)
LinRegData$Sales_month <- month(LinRegData$date)

LinRegData$per_living <- (LinRegData$sqft_living*100)/LinRegData$sqft_lot

LinRegData$per_above <- (LinRegData$sqft_above*100)/LinRegData$sqft_lot

LinRegData$age <- LinRegData$Sales_year - LinRegData$yr_built

LinRegData$age_renovated <- ifelse(LinRegData$yr_renovated>0,LinRegData$Sales_year-LinRegData$yr_renovated,LinRegData$age)

LinRegData$renovated_flag <- ifelse(LinRegData$yr_renovated>0,1,0)


#6. removing variables not required
colnames(LinRegData)
LinRegData_bkup <- LinRegData
LinRegData <- LinRegData[,-c(1:3,6,7,10, 13,15:17)]


#7. Univariate analysis - categorical variable or discrete variables 
# Those vaeriables which are continous but has less distinct values are known as discrete variables
# the function created below, shows you the frequency distribution - % of obs across each distinct value
# for e.g. for gender it will give you % of obs for males & females in a bar graph
# continous - can take value, you can take avg. like - sales, price
# categorical - you can not take avg, usually less distinct values, like gender, marital status, pincodes
# discrete - continous with less distinct values

uniAnalysisCateg<-function(var)
{
  Freq_tbl<-data.frame(table(LinRegData[,var]))  # when using on your own, ensure the dataset name is LinRegData
  Freq_tbl$var_name <-var
  colnames(Freq_tbl)<-c("values","freq","variable")
  return(Freq_tbl)
}
uniDataCateg <- data.frame(values = character(),freq = numeric(),variable = character())
# when using on your own - make changes in these lines to replace the existing variable names with your variable names
uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg("bedrooms"))
uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg("condition"))
uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg("grade"))
uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg("floors_band"))
uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg("view_band"))
# you add of delete these lines basis the number of variables you have

#library(ggplot2)
tot<-nrow(LinRegData)
uniDataCateg$perc<-round(100*uniDataCateg$freq/tot,0)
ggplot(uniDataCateg, aes(x = values, y = perc)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(uniDataCateg, "uniDataCateg_cp.csv", row.names=FALSE)
View(uniDataCateg)

# 7.1. Univariate Analysis - Continous var
# Here what this does is - it gives you the percentile distribution of continous variables
# this will help you realize if  outilers are there or you need transformations
uniAnalysisCont<-function(var)
{
  Pctl_tbl<-as.vector(quantile(LinRegData[,var], probs=c(.01, .10, .20, .50, .80, .90, .99, 1.0)))  # when using on your own, ensure the dataset name is LinRegData
  Pctl_tbl<-data.frame(c("P001","P010","P020","P050","P080","P090","P099","P100"),Pctl_tbl)
  Pctl_tbl<-data.frame(c(var,var,var,var),Pctl_tbl)
  colnames(Pctl_tbl)<-c("variable","quantiles","values")
  return(Pctl_tbl)
}
uniDataCont <- data.frame(variable = character(), quantiles = character(), values = numeric())
# when using on your own - make changes in these lines to replace the existing variable names with your variable names
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("sqft_above_log"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("sqft_basement"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("lat"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("long"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("price_log"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("sqft_living_log"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("sqft_lot_log"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("Sales_month"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("per_living"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("per_above"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("age"))
uniDataCont<-rbind(uniDataCont,uniAnalysisCont("age_renovated"))
# you add of delete these lines basis the number of variables you have
ggplot(uniDataCont, aes(x = quantiles, y = values)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(uniDataCont, "uniDataCont.csv", row.names=FALSE)
View(uniDataCont)


#8. Bivariate Analysis 
# it is used for reducing the variables, by finding out which variables are not important
# it means which var will not explain the variations in price
# To do this, we have to do this separately for continous & separately for categorical/discrete
# now you can see that at this stage there is no pure categorical variable,
# as for all the vars we can take avgs, so it either continous or discrete (continous with less distinct values)# lets look at the column names
colnames(LinRegData)
# discrete - those variables which are continous (can take avg) and less distinct values.
# count of unique values of any variable. if it is less and you can take an avg also than 
# it is discrete. e.g. - 
# bedroom, bathrooms, floor, condition, grade, floors_band, grand_band# Continous - are all the var whose avg makes sense
# [1] "bedrooms"        "bathrooms"       "floors"                "condition"      
# [6] "grade"           "sqft_basement"   "lat"             "long"            "price_log"      
# [11] "sqft_living_log" "sqft_lot_log"    "floors_band"       "grade_band"     
# [16] "sqft_above_log"  "Sales_year"      "Sales_month"     "per_living"      "per_above"      
# [21] "age"             "age_renovated"   

# Categorical - the ones whose avg has no meaning
# waterfront, view_band, renovated_flag, zip code
# out of these all the others except zipcode has very less distinct values
# zip code has high number of distinct values. 
# For every categorical variable you can not put them in the model without creating dummy variables
# For categorical var with high distinct values (like zip code) - you have to 
# first reduce the number of distinct values by making groups of zip codes
# and then you have to make dummy variables
# if your categorical variables has only 0,1 like waterfront, renovated_flag & view_band
# then you do not need to create dummy variables

