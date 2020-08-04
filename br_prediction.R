#Clean the environment
rm(list = ls())

#set current working directory
setwd("C:/R programs")

#Current working directory
getwd()

#reading CSV
day = read.csv(file = "day.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))

##################################Data pre-processing######################################

head(day)
#Getting the structure of the dataset
str(day)

#Getting the number of variables and obervation in the datasets
dim(day)

#removing instant & dteday
day$instant = NULL
day$dteday = NULL

dim(day)

#converting features to appropriate types
day$season = as.factor(day$season)
day$yr = as.factor(day$yr)
day$mnth = as.factor(day$mnth)
day$holiday = as.factor(day$holiday)
day$weekday = as.factor(as.character(day$weekday))
day$workingday = as.factor(as.character(day$workingday))
day$weathersit = as.factor(day$weathersit)

str(day)

################################################Univariate Analysis#################################################

library(ggplot2)

# analyze the distribution of  target variable 'cnt'
ggplot(day)+
  geom_histogram(aes(x=cnt,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=cnt,y=..density..))

# analyse the distrubution of  independence variable 'temp'
ggplot(day)+
  geom_histogram(aes(x=temp,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=temp,y=..density..))

# analyse the distrubution of  independence variable 'atemp'
ggplot(day)+
  geom_histogram(aes(x=atemp,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=atemp,y=..density..))

# analyse the distrubution of  independence variable 'hum'
ggplot(day)+
  geom_histogram(aes(x=hum,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=hum,y=..density..))

# analyse the distrubution of  independence variable 'windspeed'
ggplot(day)+
  geom_histogram(aes(x=windspeed,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=windspeed,y=..density..))

# analyse the distrubution of  independence variable 'casual'
ggplot(day)+
  geom_histogram(aes(x=casual,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=casual,y=..density..))

# analyse the distrubution of  independence variable 'registered'
ggplot(day)+
  geom_histogram(aes(x=registered,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=registered,y=..density..))

################################################Bivariate Analysis#################################################

#1st, I'll check the relation between target variable & continuous independent variables

#check the relationship between 'temp' and 'cnt' variable

ggplot(day, aes(x= temp,y=cnt)) +
  geom_point()+
  geom_smooth()
#there is good positive relation between 'temp' and 'cnt'

#check the relationship between 'atemp' and 'cnt' variable

ggplot(day, aes(x= atemp,y=cnt)) +
  geom_point()+
  geom_smooth()
#there is good positive relation between 'atemp' and 'cnt'

#check the relationship between 'hum' and 'cnt' variable

ggplot(day, aes(x= hum,y=cnt)) +
  geom_point()+
  geom_smooth()
#there is poor relation between 'hum' and 'cnt'

#check the relationship between 'windspeed' and 'cnt' variable

ggplot(day, aes(x= windspeed,y=cnt)) +
  geom_point()+
  geom_smooth()
#there is negative relation between 'windspeed' and 'cnt'

#check the relationship between 'casual' and 'cnt' variable

ggplot(day, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()
#there is somewhat good positive relation between 'casual' and 'cnt'

#check the relationship between 'registered' and 'cnt' variable

ggplot(day, aes(x= registered,y=cnt)) +
  geom_point()+
  geom_smooth()
#there is good relation between 'registered' and 'cnt'

#1st, I'll check the relation between target variable & categorical independent variables

#check relationship between season & cnt
ggplot(day, aes(x=season, y=cnt)) + 
  geom_boxplot()

#check relationship between yr & cnt
ggplot(day, aes(x=yr, y=cnt)) + 
  geom_boxplot()

#check relationship between mnth & cnt
ggplot(day, aes(x=mnth, y=cnt)) + 
  geom_boxplot()

#check relationship between holiday & cnt
ggplot(day, aes(x=holiday, y=cnt)) + 
  geom_boxplot()

#check relationship between weekday & cnt
ggplot(day, aes(x=weekday, y=cnt)) + 
  geom_boxplot()

#check relationship between workingday & cnt
ggplot(day, aes(x=workingday, y=cnt)) + 
  geom_boxplot()

#check relationship between weathersit & cnt
ggplot(day, aes(x=weathersit, y=cnt)) + 
  geom_boxplot()

######################################## MISSING VALUE ANALYSIS########################################

missing_val = data.frame(apply(day,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_Value"
missing_val = missing_val[order(-missing_val$Missing_Value),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_val.csv", row.names = F)

#So, I can see there are no missing values


########################################################Outlier Analysis##################################################################

#Detection of outliers using boxplot
boxplot(day$temp, xlab="temp outliers")
boxplot(day$atemp,xlab = "atemp outliers")
boxplot(day$hum,xlab ="hum outliers")
boxplot(day$windspeed,xlab ="windspeed outliers")
boxplot(day$casual,xlab ="casual outliers")
boxplot(day$registered,xlab ="registered outliers")
boxplot(day$cnt,xlab ="cnt outliers")

#I Can see 'hum', 'windspeed', 'casual' variables contain outliers

out_data = day[c('hum','windspeed','casual')]
cnames = colnames(out_data)
for(i in cnames){
  #print(i)
  val = day[,i][day[,i] %in% boxplot.stats(day[,i])$out]
  print(length(val))
  day[,i][day[,i] %in% val] = NA
}
missing_val_new = data.frame(apply(day,2,function(x){sum(is.na(x))}))
sum(is.na(day$hum))
day$windspeed

#lets take a known value & convert it to na, then we'll impute the same using diff. techniques
#say, we take day[2,11]
#actual value= 0.248539
#mean= 0.1863051
#median= 0.178496
#knn= 0.1300783

#day = df
#df = day

#first, I'll convert the known value to NA
#day[2,11] = NA

#Mean Method
#day$windspeed[is.na(day$windspeed)] = mean(day$windspeed, na.rm = T)

#Median Method
#day$windspeed[is.na(day$windspeed)] = median(day$windspeed, na.rm = T)

#knn imputation method
#library(DMwR)
#day = knnImputation(day, k = 3)
#sum(is.na(day))

#so, mean is closest to the missing value, hence selected for this dataset
day$hum[is.na(day$hum)] = mean(day$hum, na.rm = T)
day$windspeed[is.na(day$windspeed)] = mean(day$windspeed, na.rm = T)
day$casual[is.na(day$casual)] = mean(day$casual, na.rm = T)

sum(is.na(day))


########################################################Feature Selection##################################################################

#Correlation Analysis
num_var = c('temp', 'atemp', 'hum', 'windspeed','casual','registered','cnt')
library(corrgram)
corrgram(day[,num_var],
         order = F,  #we don't want to reorder
         upper.panel=panel.pie,
         lower.panel=panel.shade,
         text.panel=panel.txt,
         main = 'CORRELATION PLOT')
#we can see there's high correlation between 'temp' & 'atemp'; so I'll remove one of these variable
#we can see there's high correlation between 'registered' & 'cnt'; but as 'cnt' is target variable, I'll keep 'registered' variable

# #Checking dependency among different categorical variables

#anova test

anova_season =(lm(cnt ~ season, data = day))
summary(anova_season)
#p-value <0.05

anova_year =(lm(cnt ~ yr, data = day))
summary(anova_year)
#p-value <0.05

anova_month =(lm(cnt ~ mnth, data = day))
summary(anova_month)
#p-value <0.05

anova_holiday =(lm(cnt ~ holiday, data = day))
summary(anova_holiday)
#p-value >0.05

anova_weekday =(lm(cnt ~ weekday, data = day))
summary(anova_weekday)
#p-value >0.05

anova_workingday =(lm(cnt ~ workingday, data = day))
summary(anova_workingday)
#p-value >0.05

anova_weathersit =(lm(cnt ~ weathersit, data = day))
summary(anova_weathersit)
#p-value <0.05

#so, I can accept the null hypothesis for weekday, holiday & workingday, saying that the means of all categories in these variable are same. 
#I can't accept the null hypothesis for season, weathersit, yr & mnth, saying that the means of all categories in these variable are not same.
#However, as ANOVA doesn't specify which categories mean's are different,
#So, we can't conclude from the test results aboutwhich categorical variables I should remove.

#Chi squared test
cat_var = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday','weathersit')
cat_day = day[,cat_var]

for (i in cat_var){
  for (j in cat_var){
    print(i)
    print(j)
    print(chisq.test(table(cat_day[,i], cat_day[,j]))$p.value)
  }
}

# If any pair of variables have p-value less than 0.05, then that means those variables are dependent on each other
#Those variables are-
#1)"season" & "weathersit" with a p-value of 0.0211793
#2)"mnth" & "season" with a p-value of 0
#3)"mnth" & "weathersit" with a p-value of 0.01463711
#4)"holiday" & "weekday" with a p-value of 8.567055e-11
#4)"holiday" & "workingday" with a p-value of 4.033371e-11
#5)"weekday" & "workingday" with a p-value of 6.775031e-136

#so, I'll remove season, holiday
day$atemp = NULL
day$season = NULL
day$holiday = NULL

head(day)

########################################DECISION TREE########################################
#MAPE = 13.496%

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(day), 0.8 * nrow(day))
train = day[train_index,]
test = day[-train_index,]

dim(train)
dim(test)

library(rpart)
#rpart for regression
dt_model = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[,-11])

#Create dataframe for actual and predicted values
#df = data.frame("actual"=test[,11], "dt_predictions"=dt_predictions)
#head(df)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

#calculate MAE
MAE = function(actual, pred){
  print(mean(abs(actual - pred)))
}
MAE(test[,11], dt_predictions)

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,11], dt_predictions)

#calculate RMSE
RMSE = function(actual, pred){
  print(sqrt(mean((actual - pred)^2)))
}
RMSE(test[,11], dt_predictions)


########################################RANDOM FOREST########################################
#MAPE = 6.697316%

#Train the data using random forest

library(randomForest)

rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-11])

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

#Calculate MAPE
MAE(test[,11], rf_predictions)

#Calculate MAPE
MAPE(test[,11], rf_predictions)

#Calculate MAPE
RMSE(test[,11], rf_predictions)

########################################LINEAR REGRESSION########################################
#MAPE = 8.585146%

#Train the data using linear regression
lr_model = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-11])

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#Calculate MAE
MAE(test[,11], lr_predictions)

#Calculate MAPE
MAPE(test[,11], lr_predictions)

#Calculate RMSE
RMSE(test[,11], lr_predictions)

#So, we can see that Random forest is giving least amount of errors, hence 'rf_model' is chosen as most optimum model for this dataset

#################################################Output with New Input#############################################################
#MAPE = 3.407836%
#I've selected one observation from the test data ; 
#I shall use this observation as a new sample input & predict the output & at last I'll see how well the model is performing (by checking the error rate)

#Instructions for using this model on a new dataset containing all the variables present in the original dataset
#1)Drop 'instant','dteday','season','holiday' & 'atemp' variables as these are statistically insignificant
#2)Convert 'yr','mnth','weekday','workingday' & 'weathersit' variables to category type

#Predict a sample data
predict(rf_model,test[4,-11])

test[4,11]

#actual value = 1927
#predicted value = 1861.331 

#Calculate MAPE
MAPE(1927, 1861.331)

#From here, we can see the generated model is performing well with a new sample input
