#Data Science & Analytics Assignment

#shivaani Katragadda
#R00183214

#Part 2- f,g,h questions


#Here i am using this packages to read the files of given dataset and 
#to build the decision tree model so that i can plot the roc plot by using the predicted values and actual values of the models
#readxl for reading excel sheet,DataExplorer to see the missing values of data,rpart for building decision tree
#and c50 for boosting algorithm(this is giving good acuracy among all the other),caret for confusion matrix for decision tree and boosting algorithm


#installing readxl package useful to get the data from excel to R easily.

install.packages("readxl")
library(readxl)#loads and attaches the add-on packages.

#installing DataExplorer package which useful for visualizing and analysing the data

install.packages("DataExplorer")
library(DataExplorer)#loads and attaches the add-on packages.

#installing c50 package which is useful to build classification trees in R

install.packages("C50")
library(C50)#loads and attaches the add-on packages.

#installing rpart(Recursive Partitioning And Regression Trees) package which is useful for creating decision tree

install.packages("rpart")
library(rpart)#loads and attaches the add-on packages.

#installing Caret(Classification And REgression Training) package contains set of functions that are useful for creating predictive models

install.packages("caret")
library(caret)#loads and attaches the add-on packages.

#=====================================================
#Reading the scoring_Data of Credit_Risk6_final.xlsx file by using read_excel() function and it is assigned to Credit_Risk6_final1

Credit_Risk6_final1 <- read_excel("F:/Ds Assignment/Credit_Risk6_final.xlsx", 
                                  sheet = "Scoring_Data")
#Generating the dataframe for Credit_Risk6_final1 using as.data.frame() function and that dataframe is named as Credit_Scoring_Data 

Credit_Scoring_Data<- as.data.frame(Credit_Risk6_final1)

#=======================================================================
#Reading the Training_Data of Credit_Risk6_final.xlsx file by using read_excel() function and it is assigned to Credit_Risk6_final2

Credit_Risk6_final2 <- read_excel("F:/Ds Assignment/Credit_Risk6_final.xlsx", 
                                  sheet = "Training_Data")
#Generating the dataframe for Credit_Risk6_final2 using as.data.frame() function and that dataframe is named as Credit_Training_Data

Credit_Training_Data<- as.data.frame(Credit_Risk6_final2)

#Filling the NA values

#The missing data is a categorical data so i am replacing the missing values with the mode 


dim(Credit_Training_Data) #dim() will give the number of rows and columns in the dataset 

#plot_missing() function shows the percentage of missing values of each column present in the dataset

plot_missing(Credit_Training_Data)
#from plot_missing() we know that we have missing values in three columns they are Housing,Personal status   and Employment.

Credit_Training_Data$Housing#viewing Housing column of credit_Training_Data 

table(is.na(Credit_Training_Data$Housing))#checking how many misssig values are there in the column

sort(table(Credit_Training_Data$Housing))#sorting the column in order to get the total number of observation for each domain in the column in ascending order

names(table(Credit_Training_Data$Housing))[table(Credit_Training_Data$Housing)==max(table(Credit_Training_Data$Housing))]#now taking the domain which is having the high value    

Credit_Training_Data$Housing[is.na(Credit_Training_Data$Housing)] <- "Own"#Now assigining the highest value to the missing rows in the column

table(is.na(Credit_Training_Data$Housing))#checking whether all the missing values are replace by running this line again



Credit_Training_Data$Employment#viewing Employmentment column of credit_Training_Data 

table(is.na(Credit_Training_Data$Employment))#checking how many misssig values are there in the column

sort(table(Credit_Training_Data$Employment))#sorting the column in order to get the total number of observation for each domain in the column in ascending order

names(table(Credit_Training_Data$Employment))[table(Credit_Training_Data$Employment)==max(table(Credit_Training_Data$Employment))] #now taking the domain which is having the high value     

Credit_Training_Data$Employment[is.na(Credit_Training_Data$Employment)] <- "Short"#Now assigining the highest value to the missing rows in the column

table(is.na(Credit_Training_Data$Employment))#checking whether all the missing values are replace by running this line again


Credit_Training_Data$`Personal Status`#Viewing personal status column of credit_Training_Data 


table(is.na(Credit_Training_Data$`Personal Status`))#checking how many misssig values are there in the column

sort(table(Credit_Training_Data$`Personal Status`))#sorting the column in order to get the total number of observation for each domain in the column in ascending order

names(table(Credit_Training_Data$`Personal Status`))[table(Credit_Training_Data$`Personal Status`)==max(table(Credit_Training_Data$`Personal Status`))]#now taking the domain which is having the high value     

Credit_Training_Data$`Personal Status`[is.na(Credit_Training_Data$`Personal Status`)] <- "Single"#Now assigining the highest value to the missing rows in the column

table(is.na(Credit_Training_Data$`Personal Status`))#checking whether all the missing values are replace by running this line again


#All the missing data is imputated with mode check again whether any missimg data is present in the datase by using plot_missing()
plot_missing(Credit_Training_Data)


#==========================================(f) Question============================================================
#f) Develop a InfoGain algorithm that works on this dataset to calculate the variable for the first split. 
#You may use the code developed in the labs as a starting point but make sure to annotate your code with comments explaining 
#what it is doing. Note you can only used base R commands here no other packages are allowed. Comment on your results. 




#creating one function i.e tabfun which contains the proprtion table with all the columns in the Credit_Training_Data along with laplace smoothing(which allows unrepresented value to show up) with margin=1

tabfun <- function(x) {prop.table(table(Credit_Training_Data[,x],Credit_Training_Data[,14]) + 1e-6, margin = 1)}  

# The formula for  entropy is. -1 *probability of a false * log2( of this probability)

# Now we need rowSums of this, i.e 

rowSums(-tabfun(5)*log2(tabfun(5)))#checking the function by passing one column value and finding rowsums




# Now bring it altogether with one formula and writing one function to find entropy i.e entopy_tab

entopy_tab <- function(x) { tabfun <- prop.table(table(Credit_Training_Data[,x],Credit_Training_Data[,14])+ 1e-6, margin = 1)
sum(prop.table(table(Credit_Training_Data[,x]))*rowSums(-tabfun(x)*log2(tabfun(x))))}

#Here I am writiong one for loop for finding the entopy value for the columns 2 to 13

for (i in 2:13){
   print(colnames(Credit_Training_Data[i]))#printing the each column name of credit_Training_Data
   entropy =  entopy_tab(i)#finding the entropy for each column by calling enrtopy_tab function and storing the result in the variable entropy
   print (entropy)#printing the value of entopy
}

Creditstanding_prop_table <- prop.table(table(Credit_Training_Data$`Credit Standing`))#making proportion table for credit standing in Crexdit_Training_Data and storing in Creditstanding_prop_table

Creditstanding_prop_table#printing the Creditstanding_prop_table value

entopy_total <-sum(-Creditstanding_prop_table*log2(Creditstanding_prop_table))#finding the entopy value Creditstanding_prop_table,considering it as total entopy and storing in the variable entopy_total

entopy_total

infogain.list<-NULL#initializing one empty list i.e infogain.list

#Now writing one function to find infogain i.e infogain()
infogain<-function(x)
{
   gain=entopy_total-entopy_tab(x)#finding the infogain and storing it in variable gain
   return(gain)#returning the infogain value i.e gain 
}
#Here I am writing one for loop for finding the infogain value for the columns 2 to 13


for (i in 2:13){
   print(i)#printing the value of i
   info =  infogain(i)#finding the infogain for each column by calling the function infogain()  and storing the result in the variable info
   print (info)#printig the result of info
   infogain.list<-c(infogain.list,info)#assigining a vector which contains infogain.list,info to infogain.list 
   print(colnames(Credit_Training_Data[i]))#printing the each column name of credit_Training_Data

}

infogain.list#displaying the values in the infogaion.list

mainindex<-which.max(infogain.list)#getting the maximin index from the infogain.list and storing it in the variable mainindex

mainindex#printing the mainindex

print(colnames(Credit_Training_Data[mainindex+1]))#now printing the column name of the mainindex

#the column Credit History is the first split




#================================================(f) question ======================================================

#=================================================(g) Question=====================================================

#Develop code in R that illustrates how boosting works using the formulae for adabag in the attached document. 
#Use the Excel spreadsheet attached so that you have only ten data points. Use set.seed(abc) with abc being the last 3 digits of your student number to 
#generate a random prediction (each time) for 4 iterations of boosting. Include a confusion matrix at the end for your final prediction and comment. 
#Note you can only used base R commands here no other packages are allowed. 


set.seed(214)#setting the seed with the last three digits of my student number

id<-c(1,2,3,4,5,6,7,8,9,10)#Taking 10 values in a vector which are called ID's and assigining that vector to Variable ID

label<-c(0,1,1,0,1,1,0,1,0,0)#Taking 10 values in a vector which are called  label and assigining that vector to Variable Label

weights<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)#Taking 10 values in a vector which are called Weights and assigining that vector to Variable Weights

counter <-0#initializing a counter with value zero

currentDF<-data.frame()#initializing one empty dataframe i.e called as currentDF

nextDF <- data.frame()#initializing one empty dataframe i.e called as nextDF

#writing while loop for implementing adaboost 

while (counter < 4) {
   #If counteris equal 
   
   if (counter == 0){
      currentDF <- data.frame(id, label, weights)#assign is,label,weights to currentDf dataframe
   } else {
      currentDF <- nextDF#if counter not equal to zero assign nextDF dataframe to currentDF
   }
   
   currentDF$prediction <- sample(0:1, 10, replace=TRUE)#generating random numbers between 0 and 1 ten times and assigining that to currentDF$prediction
   
   currentDF$error <- ifelse(currentDF$label==currentDF$prediction,0,1)#Now finding error value if label value is equal to prediction value error value will be 0 else error value is 1,and assigning error value to CurrentDF$error
   
   currentDF$xy <- currentDF$weights*currentDF$error#now finding the product of weights and error and assigining to currentDF$xy
   
   sumofxy <- sum(currentDF$xy)#Finding sum of currentDF$xy
   
   alpha <- 0.5 * (log((1-sumofxy)/sumofxy))#finding alpha value by using the given formula
   
   incorrect <- exp(-alpha*-1)#finding the incorrect value by using  the given formula
   
   correct <- exp(-alpha*1)#finding the correct value by using  the given formula
   
   #Now finding adjustment value if label value is equal to prediction value error value will be correct else error value is incorrect,and assigning adjustment value to CurrentDF$adjustment
   
   currentDF$adjustment <- ifelse(label==currentDF$prediction,correct,incorrect)
   
   #now finding  the product of adjustment*weights and assigining it to the CurrentDF$adj_weight 
   
   currentDF$adj_weight <- (currentDF$adjustment)*(currentDF$weights)
   
   #Now Finding the new_weight value i.e adj_weight divided by  sum of adj_weight and assigining it to currentDF$new_weight
   
   currentDF$new_weight <- currentDF$adj_weight/sum(currentDF$adj_weight)
   
   
   nextDF <- currentDF#Assigining all the currentDF dataframe values to nextDF dataframe
   
   nextDF$weights <- currentDF$new_weight#assigining the new_weights value of currentDF dataframe to weights of nextDF dataframe value 
   
   cat("\n")
   
   print(currentDF)#printing the currentDF dataframe
   
   counter <- counter + 1#incrementing the value of counter
}

print(currentDF)#Printing the final result of currentDF

#NOW confusion matrix

table(currentDF$label,currentDF$prediction)#making the table for label and prediction of currentDF

#we can find true positive and true negatives from table and Add the true positives and true negatives and then divide by all the values to find the confusion matrix
#here the values will not change because we are using set.seed(214) so directly finding confusion matrix by using the values from the table 

cat("The confusion matrix is",(3+1)/(3+2+4+1))#finding the confusion matrix and printing the result

#===================================================(g) question=====================================================

#====================================================(h) Question========================================================
#h) Generate prediction probabilities obtained in your best model aboveand  use R code to create and plot an ROC curve, 
#note you can only used base R commands here no other packages are allowed.  Comment on the ROC curve. 

#Here i am building decision tree again to show the model 

#decision tree 

#The seed number is the starting point used in the generation of a sequence of random numbers, and the same results will be given if the same seed number is used.

set.seed(214)#setting the seed with the last three digits of my student number

#here I am making two samples by dividing the rows of Credit_Training_Data into 80% and 20% probability and storing in to variable id

id<-sample(2,nrow(Credit_Training_Data),prob=c(0.8,0.2),replace=TRUE)

#I am creating training data with the first sample(80%) i.e id==1 and storing that in to credit_train

Credit_train=Credit_Training_Data[id==1,]

nrow(Credit_train)#here by using nrow we can see how many rows the credit_train is taking

#after execution it show it is taking 633 rows for training the model

#I am creating testing data with the second sample(20%) i.e id==2 and storing that in to credit_test

Credit_test=Credit_Training_Data[id==2,]

#here by using nrow we can see how many rows the credit_train is taking

nrow(Credit_test)

#after execution it show it is taking 147 rows for testing the model

#now creating the model by using the credit standing column with the data credit_train i.e training data(we will train the model by using this training data)
#here i am using rpart package for creating the decision tree
#the model is stored in the  variable Credit_model

Credit_model<-rpart(`Credit Standing`~.,data=Credit_train)

Credit_model#viewing the model 

plot(Credit_model,margin=0.1)#plotting the Credit_model,the plot will give the outline of the decision treei.e the graphical view of tree 

text(Credit_model,use.n=TRUE,pretty=TRUE,cex=0.9)#the graphical view of the tree is filled with the text by using the text() function

#prp-Plot An Rpart Model.
#The prp function plots rpart trees. It automatically scales and adjusts the displayed tree for best fit. 
#This function is in the rpart.plot R package. 
#plotting the credit_model using prp() function

prp(Credit_model,box.col=c("Grey", "Orange")[Credit_model$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE,main="Decision tree")

# rpart.plot is also automatically scales and adjusts the displayed tree for best fit
#plotting the credit_model again by using rpart.plot() function and this function is also available in rpart.plot

rpart.plot(Credit_model,main="Decision tree")

#NOw predicting the model by using test data i.e by using credit_test(we will always predict the values by using testing data)
#Predicted values are stored in pred_Credit variable

pred_Credit<-predict(Credit_model,newdata=Credit_test,type="class")

pred_Credit#printing the predicted values

plot(pred_Credit)#I am plotting the predicted values to see the ratio of bad and good 

#Now creating the table for predicted values and actual test values i.e pred_Credit and Credit_test$`Credit Standing`

table(pred_Credit,Credit_test$`Credit Standing`)

#now finding the confusion matrix by using confusionMatrix() function which is available in caret package.
##generallt confusion matrix is the sum of true positive and true negatives  divide by sum of all the values in the table.
#finding confusion matrix for pred_Credit and Credit_test$`Credit Standing`

confusionMatrix(table(pred_Credit,Credit_test$`Credit Standing`))#accuracy is 73.47%


#Among all the model boosting algorithm is giving highest accuracy
#  Boosting Algorithms

#trainControl the computational  quality that is not easy to notice but may be important to the train function
#trainControl that allow us to perform variety of cross validation

control <- trainControl(method="repeatedcv", number=10, repeats=3)

set.seed(214)#setting the seed with the last three digits of my student number


#now creating the model by using the credit standing column with the data credit_train i.e training data(we will train the model by using this training data)
#here i am using c50 package for boosting algorithm improving the  decision tree model
#the model is stored in the  variable Boost_model

Boost_model <- train(`Credit Standing`~., data=Credit_train, method="C5.0", metric="Accuracy", trControl=control)

plot(Boost_model)#plotting the model

#NOw predicting the model by using test data i.e by using credit_test(we will always predict the values by using testing data)
#Predicting the values by using Boost_model and Credit_test data and storing in the variable predict_boost

predict_boost=predict(Boost_model,newdata=Credit_test)

predict_boost#printing the predict_boost

plot(predict_boost)#I am plotting the predicted values to see the ratio of bad and good 

#Now creating the table for predicted values and actual test values i.e predict_boost and Credit_test$`Credit Standing`

table(predict_boost,Credit_test$`Credit Standing`)

#finding confusion matrix for predict_boost and Credit_test$`Credit Standing`

confusionMatrix(table(predict_boost,Credit_test$`Credit Standing`))#accuracy is 78.23%

#==============ROC curve

#I am taking predict_boost,if predict_boost is equal to bad it takes 0(bad) else take 1(good)

predict_class_roc <- ifelse(predict_boost=="Bad",0,1)

predict_class_roc#printing the predict_class_roc

#I am taking Credit_test$`Credit Standing`,if Credit_test$`Credit Standing` is equal to bad it takes 0(bad) else take 1(good)

predict_class_roc1 <- ifelse(Credit_test$`Credit Standing`=="Bad",0,1)

predict_class_roc1#printing the predict_class_roc

#making the table for both predict_class_roc and predict_class_roc1

table(predict_class_roc,predict_class_roc1)

#Creating the function ROC 

ROC <- function(predict_class_roc1,predict_class_roc)
{
   #now ordering the predict_class_roc1 and storing in predict_class_roc1
   
   predict_class_roc1 <- predict_class_roc1[order(predict_class_roc,decreasing = TRUE)]
   
   #now finding sensitivity(true positive) and specificity(false positive) and storing in the dataframe
   
   data.frame(sensitivity=cumsum(predict_class_roc1)/sum(predict_class_roc1), specifity=cumsum(!predict_class_roc1)/sum(!predict_class_roc1),predict_class_roc1)
}


#Calling the Roc function by passing actual values and predicted values for plotting the graph and storing in Roc_CURVE variable

Roc_CURVE <-ROC(predict_class_roc1,predict_class_roc)

#plotting the Roc graph by taking the specificity on x-axis and sensitivity on y axis

plot(Roc_CURVE$specifity,Roc_CURVE$sensitivity,col=1+Roc_CURVE$predict_class_roc1,main="ROC CURVE",xlab="1-Specificity",ylab="Sensitivity")

#===================================================(h) question================================================