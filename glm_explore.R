##Analysis script

# Cleaning workspace
rm(list=ls())

# Loading the used libraries
library(tidyverse)
library(caret)
library(ggthemes)
library(mice)
library(ggrepel)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ROCR)


datasetdir<-file.path(getwd(),"Data")
datacolnames <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","target") #defining the column names

clevelanddata<-read.csv(file.path(datasetdir,"processed.cleveland.data"), header=FALSE)
swissdata<-read.csv(file.path(datasetdir,"processed.switzerland.data"), header=FALSE)
vadata<-read.csv(file.path(datasetdir,"processed.va.data"), header=FALSE)
hungariandata<-read.csv(file.path(datasetdir,"processed.hungarian.data"), header=FALSE)

#  assign the header names
names(clevelanddata)<-datacolnames
names(swissdata)<-datacolnames
names(vadata)<-datacolnames
names(hungariandata)<-datacolnames


# before concatenating, adding one column to indicate origin of data
clevelanddata<- mutate (clevelanddata, institute="Cleveland")
swissdata<- mutate (swissdata, institute="Switzerland")
vadata<- mutate (vadata, institute="Longbeach")
hungariandata<- mutate (hungariandata, institute="Hungary")

# build a unique dataset concatenating the rows from all previous 4 research institutes
heartdisease<-rbind(clevelanddata,swissdata,vadata,hungariandata)


# We can now remove the individual datasets and variables to free some workspace memory
rm(clevelanddata,swissdata,vadata,hungariandata,datacolnames,datasetdir)

# Data exploration


## Data preparation


# Adding factors to the features based on the data description. This will facilitate the creation and labelling of plots with the ggplot() package
heartdisease$sex<-as.factor(heartdisease$sex)
heartdisease$fbs<-as.factor(as.numeric(as.character(heartdisease$fbs)))
heartdisease$exang<-as.factor(as.numeric(as.character(heartdisease$exang)))

heartdisease$restecg<-as.factor(as.numeric(heartdisease$restecg))
heartdisease$slope<-as.factor(as.numeric(as.character(heartdisease$slope)))
heartdisease$thal<-as.factor(as.numeric(as.character(heartdisease$thal))) 
heartdisease$ca<-as.factor(as.numeric(as.character(heartdisease$ca)))
heartdisease$cp<-as.factor(as.numeric(as.character(heartdisease$cp)))

heartdisease$institute<-as.factor(as.character(heartdisease$institute))

levels(heartdisease$sex)[levels(heartdisease$sex)==0] <- "Female"
levels(heartdisease$sex)[levels(heartdisease$sex)==1] <- "Male"

levels(heartdisease$fbs)[levels(heartdisease$fbs)==0] <- "Fasting Blood Sugar <= 120"
levels(heartdisease$fbs)[levels(heartdisease$fbs)==1] <- "Fasting Blood Sugar > 120"

levels(heartdisease$exang)[levels(heartdisease$exang)==0] <- "No Exercise Induced Angina"
levels(heartdisease$exang)[levels(heartdisease$exang)==1] <- "Exercise Induced Angina"

levels(heartdisease$restecg)[levels(heartdisease$restecg)==0] <- "REST ECG 0"
levels(heartdisease$restecg)[levels(heartdisease$restecg)==1] <- "REST ECG 1"
levels(heartdisease$restecg)[levels(heartdisease$restecg)==2] <- "REST ECG 2"

levels(heartdisease$cp)[levels(heartdisease$cp)==1] <- "Chest Pain type 1"
levels(heartdisease$cp)[levels(heartdisease$cp)==2] <- "Chest Pain type 2"
levels(heartdisease$cp)[levels(heartdisease$cp)==3] <- "Chest Pain type 3"
levels(heartdisease$cp)[levels(heartdisease$cp)==4] <- "Chest Pain type 4"

levels(heartdisease$slope)[levels(heartdisease$slope)==1] <- "Peak Exercise Slope Upsloping"
levels(heartdisease$slope)[levels(heartdisease$slope)==2] <- "Peak Exercise Slope Flat"
levels(heartdisease$slope)[levels(heartdisease$slope)==3] <- "Peak Exercise Slope Downsloping"

levels(heartdisease$thal)[levels(heartdisease$thal)==3] <- "Thalium ST normal"
levels(heartdisease$thal)[levels(heartdisease$thal)==6] <- "Fixed defect"
levels(heartdisease$thal)[levels(heartdisease$thal)==7] <- "Reversible defect"

# Adding the presence boolean column: the binary heart disease presence column is built based on the presence column and is a simplification of the obersvation.
# While the presence takes integer values from 0 to 4 (0 being the absolute absence of disease), this new value presence will indicate presence (presence=1,2,3or4) or absence (presence=0).

heartdisease <- mutate(heartdisease,presence=1*!(target==0))

# Adding levels for the heart disease presence column
heartdisease$presence<-as.factor(heartdisease$presence)
levels(heartdisease$presence)[levels(heartdisease$presence)==0] <- "Healthy"
levels(heartdisease$presence)[levels(heartdisease$presence)==1] <- "Heart Disease"

# Converting numerical data to numeric format
heartdisease$thalach<-as.numeric(heartdisease$thalach)
heartdisease$trestbps<-as.numeric(heartdisease$trestbps)
heartdisease$chol<-as.numeric(heartdisease$chol)
heartdisease$oldpeak<-as.numeric(heartdisease$oldpeak)

# Building the training, test and validation sets
# The total count of samples in the dataset published by UCI is:

nrow(heartdisease)

# We will divide this data in 80% for the training/test set and 20% for the final validation.
# The feature exploration will then be carried out on the training set. Final performance evaluation will be carried out on the validation set.

set.seed(2810)
val_index <- createDataPartition(y=heartdisease$presence, times=1, p=0.2, list=FALSE)
hd_trainset <- heartdisease[-val_index,] #defining the heart disease training data set
hd_valset <- heartdisease[val_index,] #defining the heart disease validation data set

hd_trainset$institute <- NULL
hd_trainset$target <- NULL
hd_valset$institute <- NULL



control_glm <- trainControl(method = "cv", number = 10, p = .9)
train_glm <- train(presence ~ sex+cp+fbs+exang+slope+ca+thal, method = "glm",family=binomial, data = hd_trainset,trControl = control_glm ,na.action = na.omit)

#glm_results<-predict(log,hd_valset,type ="response")
glm_results<-predict(train_glm,hd_valset,type ="prob")

##### KNN method
control_knn <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(presence ~ sex+cp+fbs+exang+slope+ca+thal, method = "knn",data = hd_trainset,trControl = control_knn ,tuneGrid = data.frame(k=seq(1,20,1)),na.action = na.omit)

knn_results<-predict(train_knn,hd_valset,type ="prob")

##### Random Forest


control_rf <- trainControl(method = "cv", number = 10, p = .9)
train_rf <- train(presence ~ sex+cp+fbs+exang+slope+ca+thal ,
                  method = "rf",
                  data = hd_trainset,
                  na.action = na.omit,
                  trControl=control_rf)
print(train_rf)
varImpPlot(train_rf$finalModel)
# plotting the most important features and associated value, in decreasing importance
plot(varImp(train_rf), top = 10)


# predicting
rf_results<-predict(train_rf,hd_valset,type ="prob")


##### Regression Tree
control_rt <- trainControl(method = "cv", number = 10, p = .9)
train_rt <- train(presence ~ . ,
                  method = "rpart",
                  data = hd_trainset,
                  trControl = control_rt,
                  na.action = na.omit)
rt_results<-predict(train_rt,hd_valset,type ="prob")


keep_index<-complete.cases(hd_valset)
reference<-hd_valset$presence[keep_index]

#Using the RORc package to evaluate performance
pred_glm<-prediction(glm_results$`Heart Disease`,reference)
pred_knn<-prediction(knn_results$`Heart Disease`,reference)
pred_rf<-prediction(rf_results$`Heart Disease`,reference)
pred_rt<-prediction(rt_results$`Heart Disease`,reference)


#ROC Area under the curve
auc_glm = performance(pred_glm, 'auc')
auc_knn = performance(pred_knn, 'auc')
auc_rf = performance(pred_rf, 'auc')
auc_rt = performance(pred_rt, 'auc')

#Accuracies
acc_glm = max(performance(pred_glm, 'acc')@y.values[[1]])
acc_knn = max(performance(pred_knn, 'acc')@y.values[[1]])
acc_rf = max(performance(pred_rf, 'acc')@y.values[[1]])
acc_rt = max(performance(pred_rt, 'acc')@y.values[[1]])
