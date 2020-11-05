#Data Science & Analytics Assignment

#shivaani Katragadda
#R00183214

#part 1- a,b,c,d,e questions

#installing readxl package useful to get the data from excel to R easily.

install.packages("readxl")
library(readxl)#loads and attaches the add-on packages.

#installing ggplot2 package,it is particularly useful for visualizing the data.

install.packages("ggplot2")
library(ggplot2)#loads and attaches the add-on packages.

#installing DataExplorer package which useful for visualizing and analysing the data

install.packages("DataExplorer")
library(DataExplorer)#loads and attaches the add-on packages.

#installing Caret(Classification And REgression Training) package contains set of functions that are useful for creating predictive models

install.packages("caret")
library(caret)#loads and attaches the add-on packages.

#installing rpart(Recursive Partitioning And Regression Trees) package which is useful for creating decision tree

install.packages("rpart")
library(rpart)#loads and attaches the add-on packages.

#installing rpart.plot packages which will  scales and adjusts the displayed tree for best fit

install.packages("rpart.plot")
library(rpart.plot)#loads and attaches the add-on packages.

#installing dplyr package which is useful for manipulating datasets in R very effectively.

install.packages("dplyr")
library(dplyr)#loads and attaches the add-on packages.


#installing c50 package which is useful to build classification trees in R

install.packages("C50")
library(C50)#loads and attaches the add-on packages.

#installing randomForest package which is useful to build classification trees in R

install.packages("randomForest")
library(randomForest)#loads and attaches the add-on packages.

#installing GGally package which extends ggplot2 by adding several functions to reduce the complexity of combining geoms with transformed data.

install.packages("GGally")
library(GGally)#loads and attaches the add-on packages


#================================================(a) Question=========================
#Exploratory Data Analysis (EDA): - Carry out some EDA on the data set; carry out at least one trivariate analysis; 
#do you notice anything unusual or any patterns with the data set?  Detail these and outline any actions you propose to take before you start model building in part b). 
#Max word count 500 words. 

#Reading the scoring_Data of Credit_Risk6_final.xlsx file by using read_excel() function and it is assigned to Credit_Risk6_final1

Credit_Risk6_final1 <- read_excel("F:/Ds Assignment/Credit_Risk6_final.xlsx", 
                                 sheet = "Scoring_Data")

View(Credit_Risk6_final1)#viewing the Credit_Risk6_final1

#Generating the dataframe for Credit_Risk6_final1 using as.data.frame() function and that dataframe is named as Credit_Scoring_Data 

Credit_Scoring_Data<- as.data.frame(Credit_Risk6_final1)

View(Credit_Scoring_Data)#Viewing the dataframe Credit_Scoring_Data

str(Credit_Scoring_Data)#structure of Credit_Scoring_Data

summary(Credit_Scoring_Data)#summary of Credit_Scoring_Data

class(Credit_Scoring_Data)#class of Credit_Scoring_Data

head(Credit_Scoring_Data)#head(displays the first few lines of data) of Credit_Scoring_Data




#Reading the Training_Data of Credit_Risk6_final.xlsx file by using read_excel() function and it is assigned to Credit_Risk6_final2

Credit_Risk6_final2 <- read_excel("F:/Ds Assignment/Credit_Risk6_final.xlsx", 
                                  sheet = "Training_Data")

View(Credit_Risk6_final2)#viewing the Credit_Risk6_final2

#Generating the dataframe for Credit_Risk6_final2 using as.data.frame() function and that dataframe is named as Credit_Training_Data

Credit_Training_Data<- as.data.frame(Credit_Risk6_final2)

View(Credit_Training_Data)#Viewing the dataframe Credit_Training_Data

str(Credit_Training_Data)#structure of Credit_Training_Data

summary(Credit_Training_Data)#summary of Credit_Training_Data

class(Credit_Training_Data)#class of Credit_Training_Data

head(Credit_Training_Data)#head(displays the first few lines of data) of Credit_Training_Data

#Performing Exploratory Data Analysis

#DATA EXPLORER Package provides some good functions to know about the data
#Introduce() function will give the outline of the data 
#it tells about the number of rows,columns,missing values,discrete columns,continous columns,all missing columns,complete rows,total observations and memory usage 

introduce(Credit_Training_Data)

#plot_str() will plot a graph with all the columns present in the data set and also it will tell how many observations and columns present in the dataset

plot_str(Credit_Training_Data)

#plot_intro() will plot a graph which will give the percentage of  discrete columns,continous columns,All missing columns,complete rows and missing observations

plot_intro(Credit_Training_Data)

#plot_missing() function shows the percentage of missing values of each column present in the dataset

plot_missing(Credit_Training_Data)

#Filling the NA values

#The missing data is a categorical data so i am replacing the missing values with the mode 

dim(Credit_Training_Data) #dim() will give the number of rows and columns in the dataset 

#plot_missing() function shows the percentage of missing values of each column present in the dataset

plot_missing(Credit_Training_Data)

#from plot_missing() we know that we have missing values in three columns they are Housing,Personal status   and Employment.

Credit_Training_Data$Housing#viewing Housing column of credit_Training_Data 

table(is.na(Credit_Training_Data$Housing))#checking how many misssig values are there in the column

sort(table(Credit_Training_Data$Housing))#sorting the column in order to get the total number of observation for each domain in the column in ascending order

#now taking the domain which is having the high value  

names(table(Credit_Training_Data$Housing))[table(Credit_Training_Data$Housing)==max(table(Credit_Training_Data$Housing))]  

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


#Performing some Exploratory data analysis
#plot_histogram() is provided by DataExplorer package which will provide histograms of all columns which are containing the  continuous  data

plot_histogram(Credit_Training_Data)#This will give histogram for 4 columns they are AGE,ID,Months since checking account opened,Residence time(in current district)

#plot_bar() is provided by DataExplorer package which will provide barchart for discrete data

plot_bar(Credit_Training_Data$`Checking Acct`)#plot_bar() for checking acct columnn of credit_Training_Data

plot_bar(Credit_Training_Data$`Credit History`)#plot_bar() for credit History columnn of credit_Training_Data

plot_bar(Credit_Training_Data$`Loan Reason`)#plot_bar() for Loan Reason columnn of credit_Training_Data

plot_bar(Credit_Training_Data$`Savings Acct`)#plot_bar() for Savings Acct columnn of credit_Training_Data

plot_bar(Credit_Training_Data$Employment)#plot_bar() for Employment columnn of credit_Training_Data

plot_bar(Credit_Training_Data$`Personal Status`)#plot_bar() for Personal Status columnn of credit_Training_Data

plot_bar(Credit_Training_Data$Housing)#plot_bar() for Housing columnn of credit_Training_Data

plot_bar(Credit_Training_Data$`Job Type`)#plot_bar() for Job Type columnn of credit_Training_Data

plot_bar(Credit_Training_Data$`Foreign National`)#plot_bar() for Foreign National columnn of credit_Training_Data

   
#qq plot i.e Quantile-Quantile plot is used to visualize the deviation from a specific probability distribution
#qq plot for ID,AGE,MONTHS SINCE CHECKING ACCT OPENED,RESIDENCE TIME all these columns are assigned to qq_data

qq_data <- Credit_Training_Data[, c("ID", "Age","Months since Checking Acct opened","Residence Time (In current district)")]

plot_qq(qq_data)#plotting qq plot for qq_data

plot_qq(qq_data, by = "ID")#plotting qq plot by using id for qq_data


#==========univariate
#univariate plots
#histogram of Months since checking Acct opened column of Credit_Training_Data dataset

hist(Credit_Training_Data$`Months since Checking Acct opened`,col="red")

#histogram of Residence Time (In current district) column of Credit_Training_Data dataset

hist(Credit_Training_Data$`Residence Time (In current district)`,col="Yellow")
#==========bivariate
#bivariate plots

#boxplot of ID and Age columns of Credit_Training_Data dataset

boxplot(Credit_Training_Data$ID,Credit_Training_Data$Age)

#boxplot of Months since Checking Acct opened and Residence Time (In current district) columns of Credit_Training_Data dataset

boxplot(Credit_Training_Data$`Months since Checking Acct opened`~Credit_Training_Data$`Residence Time (In current district)`)


#=========trivariant 
#trivariant  Analysis 

#plotting scatterplot between ID,Age, and Residence Time (In current district) using ggplot
ggplot(Credit_Training_Data, aes(x=ID, y=Age)) + geom_point(aes(col=ID,size=`Residence Time (In current district)`))+
   theme_minimal()+labs(subtitle="ID Vs Age", x="ID",y="Age",title="Scatterplot",caption = "Source: Credit_Scoring_Data")

#plotting barplot between Age,Residence Time (In current district) and ID using ggplot
ggplot(Credit_Training_Data, aes(x=`Residence Time (In current district)`, y=Age)) + geom_bar(stat="identity",aes(col=ID))+
   theme_minimal()+labs(subtitle="ID Vs Age", x="Residence Time",y="Age",title="Bar plot",caption = "Source: Credit_Scoring_Data")



#========summary,range,standard deviation of continuous data
summary(Credit_Training_Data$ID)#summary of ID column of Credit_Training_Data dataset
sd(Credit_Training_Data$ID)#standard deviation of ID column of Credit_Training_Data dataset
range(Credit_Training_Data$ID)#range of ID column of Credit_Training_Data dataset

summary(Credit_Training_Data$`Residence Time (In current district)`)#summary of Residence Time (In current district) column of Credit_Training_Data dataset
range(Credit_Training_Data$`Residence Time (In current district)`)#standard deviation of Residence Time (In current district) column of Credit_Training_Data dataset
sd(Credit_Training_Data$`Residence Time (In current district)`)#range of Residence Time (In current district) column of Credit_Training_Data dataset


summary(Credit_Training_Data$Age)#summary of Age column of Credit_Training_Data dataset
range(Credit_Training_Data$Age)#standard deviation of Age column of Credit_Training_Data dataset
sd(Credit_Training_Data$Age)#range of Age column of Credit_Training_Data dataset

summary(Credit_Training_Data$`Months since Checking Acct opened`)#summary of Months since Checking Acct opened column of Credit_Training_Data dataset
range(Credit_Training_Data$`Months since Checking Acct opened`)#standard deviation of Months since Checking Acct opened column of Credit_Training_Data dataset
sd(Credit_Training_Data$`Months since Checking Acct opened`)#range of Months since Checking Acct opened column of Credit_Training_Data dataset

#========proportion tables
#1-D table
#forming the table for Foreign National column of Credit_training_Data dataset and assigining to variable t1

t1 <- table(Credit_Training_Data$`Foreign National`)

t1#printing the t1 which gives the number of domains and its values

table(Credit_Training_Data$`Foreign National`)/nrow(Credit_Training_Data)#finding proportion table

#or

prop.table(table(Credit_Training_Data$`Foreign National`)/nrow(Credit_Training_Data))#finding proportion table

# 2-D table
#forming the table for Checking Acct and Credit History  column of Credit_training_Data dataset and assigining to variable t2

t2 <- table(Credit_Training_Data$`Checking Acct`,Credit_Training_Data$`Credit History`)

t2#printing the t2 which gives the number of domains and its values

prop.table(t2, margin = 2)#printing lproportion table for t2

#using round function to round the value to 2 decimal points and printing first few lines by using head() function for columns Checking Acct and Credit History by using margin=2 

head(round(prop.table(table(Credit_Training_Data$`Checking Acct`,Credit_Training_Data$`Credit History`),2),2))

# Now making barplot for Checking Acct and Credit History columns

barplot(prop.table(table(Credit_Training_Data$`Checking Acct`,Credit_Training_Data$`Credit History`), margin = 2))


# 3 way pivot table #
#forming the table for Savings Acct,Job Type and Foreign National columns of Credit_training_Data dataset and assigining to variable t3

t3 <- table(Credit_Training_Data$`Savings Acct`,Credit_Training_Data$`Job Type`,Credit_Training_Data$`Foreign National`)

t3#printing the t3 which gives the number of domains and its values

#using round function to round the value to 2 decimal points by using margin=2 for columns Savings Acct,Job Type and Foreign National of Credit_training_Data dataset

round(prop.table(t3, margin = 2),2)

#forming the table for Loan Reason,Personal Status and Housing columns of Credit_training_Data dataset and assigining to variable t3_1

t3_1 <- table(Credit_Training_Data$`Loan Reason`,Credit_Training_Data$`Personal Status`,Credit_Training_Data$Housing)

t3_1#printing the t3_1 which gives the number of domains and its values

#using round function to round the value to 2 decimal points by using margin=1 for Loan Reason,Personal Status and Housing columns of Credit_training_Data dataset

round(prop.table(t3_1,margin = 1),2)

# 3 way pivot table - better to use ftable for proportions 
#forming the ftable(frequency table to print the result more clearly and attractively) for Savings Acct,Job Type and Foreign National columns of Credit_training_Data dataset and assigining to variable t3_2

t3_2 <- ftable(Credit_Training_Data$`Savings Acct`,Credit_Training_Data$`Job Type`,Credit_Training_Data$`Foreign National`)

t3_2#printing the t3_2 which gives the number of domains and its values

#using round function to round the value to 2 decimal points by using margin=2 for columns Savings Acct,Job Type and Foreign National of Credit_training_Data dataset

round(prop.table(t3_2,margin = 2),2)#check this

#forming the ftable(frequency table to print the result more clearly and attractively) for Loan Reason,Personal Status and Housing columns of Credit_training_Data dataset and assigining to variable t3_3

t3_3 <- ftable(Credit_Training_Data$`Loan Reason`,Credit_Training_Data$`Personal Status`,Credit_Training_Data$Housing)

t3_3#printing the t3_3 which gives the number of domains and its values

#using round function to round the value to 2 decimal points by using margin=2 for Loan Reason,Personal Status and Housing columns of Credit_training_Data dataset

round(prop.table(t3_3,margin = 2),2)

#ggpairs
#The  ggpairs() function produces a matrix of scatter plots for visualizing the correlation between variables

ggpairs(Credit_Training_Data)#Make a matrix of plots with Credit_Training_Data data set


##ggcorrelation
#The  ggcorr() function draws a correlation matrix plot using ggplot2.

ggcorr(Credit_Training_Data, palette = "RdBu", label = TRUE)#Makes a  correlation matrix plot for Credit_Training_Data



#=======================================(a) question==================================================


#===========================================(b) Question=====================================================
#b) Build a decision tree model and give your decision tree, detailing its parameters.Explain how you decided on/fined tuned these parameters.
#(Include an image of your tree as well as a text output description.). Use set.seed(abc) where abc are the last 3 digits of your student no. 
#Use this set.seed for all other model building below. 


#Decision tree 

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
##generally confusion matrix is the sum of true positive and true negatives  divide by sum of all the values in the table.
#finding confusion matrix for pred_Credit and Credit_test$`Credit Standing`

confusionMatrix(table(pred_Credit,Credit_test$`Credit Standing`))#accuracy is 73.47%


#Now pruning the tree generally it is called cross validation

set.seed(214)#setting the seed with the last three digits of my student number

#To validate the model we use the printcp functions. 'CP' stands for Complexity Parameter of the tree.
 
printcp(Credit_model)#finding the complexity parameter of the credit_model

#We prune the tree to avoid any overfitting of the data.
#The final result is to have a small tree and the one with least cross validated error given by printcp() function i.e. 'xerror'.
#From the above printcp(), we can select the one value which have least cross-validated error and use it to prune the tree.

Credit_model$cptable[which.min(Credit_model$cptable[,"xerror"]),"CP"]#This function returns the optimal cp value associated with the minimum error.

#Plotcp() provides a graphical representation to the cross validated error summary. The cp values are plotted against the geometric mean to depict the deviation until the minimum value is reached.

plotcp(Credit_model)#plotting for the model

#now pruning the tree by using prune() and storing that in ptree variable

ptree<- prune(Credit_model,
              cp= Credit_model$cptable[which.min(Credit_model$cptable[,"xerror"]),"CP"])

ptree#printing the prune tree

#plotting the ptree,the plot will give the outline of the decision treei.e the graphical view of tree 

plot(ptree)

#the graphical view of the tree is filled with the text by using the text() function

text(ptree,use.n=TRUE,pretty=TRUE,cex=0.9)

#prp-Plot An Rpart Model.
#The prp function plots rpart trees. It automatically scales and adjusts the displayed tree for best fit. 
#This function is in the rpart.plot R package. 
#plotting the pruned tree ptree using prp() function

prp(ptree,box.col=c("Grey", "Orange")[ptree$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE)

# rpart.plot is also automatically scales and adjusts the displayed tree for best fit
#plotting the ptree again by using rpart.plot() function and this function is also available in rpart.plot

rpart.plot(ptree)

#NOw predicting the model by using test data i.e by using credit_test(we will always predict the values by using testing data)
#Predicted values are stored in tree.pred variable

tree.pred<-predict(ptree,Credit_test,type="class")

tree.pred #printing the predicted values for pruned tree

plot(tree.pred)#I am plotting the predicted values to see the ratio of bad and good 

#Now creating the table for predicted values and actual test values i.e tree.pred and Credit_test$`Credit Standing`

table(tree.pred,Credit_test$`Credit Standing`)

#finding confusion matrix for tree.pred and Credit_test$`Credit Standing`

confusionMatrix(table(tree.pred,Credit_test$`Credit Standing`))#accuracy is 74.83%


#==================================================(b) question===========================================




#=================================================(c) Question=======================================================
#c) Use the decision tree to predict results for the scoring set.Choose 5 different potential loan clients and explain 
#to Kate in plain English how the decision tree works (15 marks) and how the accuracy/probabilities of these being a good/bad loan was calculated by the decision tree, outling your assumptions (5 marks).
#Max word count 500 words. 


#renaming the Residence Time column to Residence Time (In current district) in Credit_scoring_data dataset because the same column have different names in both the sheets in a file

#Inorder to predict the values all the column names should be same in both sheets

Credit_Scoring_Data= Credit_Scoring_Data%>% rename(`Residence Time (In current district)` = `Residence Time`)

colnames(Credit_Scoring_Data)#Prints all the column names present in credit_scoring_data dataset

set.seed(214)#setting the seed with the last three digits of my student number

#predicting the values by using the  tree predict values with the Credit_scoring_data values

pred_Credit_score<-predict(Credit_model,newdata=Credit_Scoring_Data,type="class")

pred_Credit_score#printing the predicted values of credit_scoring_data

#====================================================(c) question=======================================

#==============================================(d) Question=========================================================

#d) Now try and improve your model using 2 other approaches, e.g. ensemble technique, boosting or a different model. 
#Explain your training/validation/testing methodology.
#Comment on your results and analyse why your model is giving better/worse results. 


#  Boosting Algorithms

#trainControl  is the computational  quality that is not easy to notice but may be important to the train function

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



#BAGGING Algorithm

#trainControl the computational  quality that is not easy to notice but may be important to the train function
#trainControl that allow us to perform variety of cross validation

control <- trainControl(method="repeatedcv", number=10, repeats=3)


set.seed(214)#setting the seed with the last three digits of my student number

#now creating the model by using the credit standing column with the data credit_train i.e training data(we will train the model by using this training data)
#here i am using random forest package for bagging algorithm improving the  decision tree model
#the model is stored in the  variable Bagging_model

Bagging_model <- train(`Credit Standing`~., data=Credit_train, method="rf", metric="Accuracy", trControl=control)

plot(Bagging_model)#printing the Bagging_model

#NOw predicting the model by using test data i.e by using credit_test(we will always predict the values by using testing data)
#Predicting the values by using Bagging_model and Credit_test data and storing in the variable Bagging_Predict

Bagging_Predict=predict(Bagging_model,newdata=Credit_test)

Bagging_Predict

#Now creating the table for predicted values and actual test values i.e Bagging_Predict and Credit_test$`Credit Standing`

table(Bagging_Predict,Credit_test$`Credit Standing`)

#finding confusion matrix for Bagging_Predict and Credit_test$`Credit Standing`

confusionMatrix(table(Bagging_Predict,Credit_test$`Credit Standing`))#accuracy is 77.55%

#===============================================(d) question==================================================


#===========================================(e) Question=================================================

#e)Kate's company uses a process that is a mixture of a grading system and human input to grade each past loan as good or bad. 
#Kate is suspicious that during a particular time that this process performed very poorly and produced inaccurate results. 
#Develop a strategy so that you can you find a series of consecutive or nearly consecutive ID numbers of circa 10 or more, i.e.
#where these gradings show a suspiciously incorrect pattern.Detail how you go about your investigation and how you find this pattern. 10 


set.seed(214)#setting the seed with the last three digits of my student number

Credit_test$ID#viewing he ID's of credit_test i.e testing data

pred_Credit#Viewing the predicted vlues of decision tree which is generated with the testing data i.e credit_test

Credit_test$`Credit Standing`#viewing the credit standing column values of credit_test

class(pred_Credit)#class of pred_credit is factor

class(Credit_test$`Credit Standing`)#class of Credit_test$`Credit Standing` is character 

Credit_test$`Credit Standing`<-as.factor(Credit_test$`Credit Standing`)#so converting Credit_test$`Credit Standing as factor

class(Credit_test$`Credit Standing`)#now the class of Credit_test$`Credit Standing is factor

#Making one list and placing both Pred_credit and Credit_test$`Credit Standing in to list

lst <- list(
   one = pred_Credit,
   two = Credit_test$`Credit Standing`)

lst#displaying the list

lst[1]#displaying list 1

lst[2]#displaying list 2

#writing for loop for printing mismatched values of predicted values and actual values 

for (index in 1:147) {
   
   x_cand <- lst$one[index]  #storing the values of list one in x_cand
   
   y_cand <- lst$two[index]  #storing the values of list two in y_cand
  
 #if x_cand(first list) value is not equal to y_cand(second list)
   
   if(x_cand!=y_cand){
      print(Credit_test$ID[index])  #the printing the ID's of all mismatched values
   }
}

#681 to 707 totally six consecutive IDs produced the mismatched values i.e the incorrect values.

#==========================================(e) question============================================================
