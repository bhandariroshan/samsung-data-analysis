library(tree)
setwd("D:/Data Analysis/Data Analysis Assignment 2")

#Data Source
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

load("samsungData.rda")
summary(samsungData)
names(samsungData)
head(samsungData)
summary(samsungData$subject)
samsungData$subject

#Prompt
#Your task is to build a function that predicts what activity a subject is performing 
#based on the quantitative measurements from the Samsung phone.
#For this analysis your training set must include the data from subjects 1, 3, 5, and 6.  
#But you may use more subjects data to train if you wish. 
#Your test set is the data from subjects 27, 28, 29, and 30, but you may use more data to test. 
#Be careful that your training/test sets do not overlap. 
#You should perform all of the steps in building a predictive model and describe your analysis 
#in a report as explained below. 

#About Data
#All of the columns of the data set (except the last two) represents one measurement from the 
#Samsung phone. The variable subject indicates which subject was performing the tasks when the 
#measurements were taken. The variable activity tells what activity they were performing. 

names(samsungData)
names(samsungData)[duplicated(names(samsungData))] #gives the duplicated Names of the columns
#There are altogether 84 duplicated Columns

#One way to avoid i.e remove duplicated value
data <- data.frame(samsungData)
names(data)

names(data)[duplicated(names(data))]


#Training Set construction
trainset<-data[data$subject %in% c(1,3,5,6),]

#Test Set Construction
testset<-data[data$subject %in% c(27,28,29,30),]

dim(trainset)
dim(testset)
#NROW(trainset)
#NCOL(trainset)

#Keep in mind what we are doing here. We want to get some data from a person's phone and figure out
#what they are doing. We are building a model using some test subjects, but we want it to work for 
#other people that we didn't use to build the model

#STEPS
#1. Use PCA, SVD to find the most affecting variable
#2. Create training set and test set
#3. Use those values to train and test the data
#4. Build a prediction Model using tree and linear modelling

#Model Selection - step
#lm1 <- lm(as.factor(activity) ~ ., data = trainset)
#aicData <- step(lm1)
#aicData

tree1 <- tree(as.factor(activity) ~ . , data =trainset)
summary(tree1)
plot(tree1)
text(tree1)
pruneTree <- prune.tree(tree1, best = 6)
plot(pruneTree)
text(pruneTree)
summary(pruneTree)
#plot errors
par(mfrow =c(1,2))
plot(cv.tree(tree1,FUN=prune.tree,method="misclass"))
plot(cv.tree(tree1))
#Show resubstitution error
#table(trainset$activity,predict(pruneTree,type="class"))
#sum(trainset$activity =="walk")

#TEST SET FIRING
predict1 <- predict(tree1,testset)
predict1
predict1 <- predict(tree1,testset,type="class") 
#plot(testset$activity)
predict1
plot(predict1)
summary(predict1)
#Finding Errors
#plot errors
par(mfrow =c(1,2))
plot(cv.tree(tree1,FUN=prune.tree,method="misclass"))
plot(cv.tree(tree1))
#Show resubstitution error
table(testset$activity,predict1)
sum(testset$activity == 'walkup')
sum(testset$activity == 'walk')
sum(testset$activity == 'walkdown')
sum(testset$activity == 'sitting')
sum(testset$activity == 'laying')
sum(testset$activity == 'stabding')