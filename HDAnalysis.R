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

# Let's first visualize the distribution of patients depending on the contributing research institute
heartdisease %>% group_by(institute) %>% 
  summarise(patients=n()) %>% 
  ggplot() +
  aes(institute,patients) +
  geom_col(fill = c("blue", "red", "gray", "green")) +
  theme_economist() + 
  xlab("Research institute") +
  ylab("Patient Count")

# There is no predominant quantity of patients from a given institute, which is in favor of merging all data together.

# Building the training, test and validation sets
# The total count of samples in the dataset published by UCI is:

nrow(heartdisease)

# We will divide this data in 80% for the training/test set and 20% for the final validation.
# The feature exploration will then be carried out on the training set. Final performance evaluation will be carried out on the validation set.

set.seed(2810)
val_index <- createDataPartition(y=heartdisease$presence, times=1, p=0.2, list=FALSE)
hd_trainset <- heartdisease[-val_index,] #defining the heart disease training data set
hd_valset <- heartdisease[val_index,] #defining the heart disease validation data set

## Feature exploration
# It is now possible to look closely to the different features in the training set. This first exploration will be carried out by means of data visualisation.

#Continuous features
# Age
hd_trainset %>%
  ggplot() +
  aes(age,fill=presence) +
  geom_density(alpha = 0.4) +
  xlab("Age") +
  ylab("Density") + 
  scale_fill_manual(name="Disease", labels=c("Absent","Present"),values=c("springgreen2","firebrick2"))

#Cholesterol
hd_trainset %>% 
  ggplot(aes(chol,fill=presence)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~institute, ncol=1,scale="free_y")+
  xlab("Serum cholestoral in mg/dl ") +
  ylab("Density")

#Trestbps Resting blood pressure (in mm Hg on admission to the hospital)
hd_trainset %>% 
  ggplot(aes(trestbps,fill=presence)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~institute, ncol=1,scale="free_y")+
  xlab("Resting blood pressure in mm Hg") +
  ylab("Density")

# Thalach maximum heart rate achieved 
hd_trainset %>% 
  ggplot(aes(thalach,fill=presence)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~institute, ncol=1,scale="free_y")+
  xlab("maximum heart rate achieved") +
  ylab("Density")

# Oldpeak ST depression induced by exercise relative to rest 
hd_trainset %>% 
  ggplot(aes(oldpeak,fill=presence)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~institute, ncol=1,scale="free_y")+
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Density")

#Categorical features

#Sex
hd_trainset %>% filter (presence=="Heart Disease") %>%
  ggplot(aes(age,fill=sex)) +
  geom_density(alpha = 0.4)

#Sex 2
hd_trainset %>%  
  ggplot() +
  aes(sex,fill=presence) +
  geom_histogram(stat="count") +
  scale_fill_manual(values=c("springgreen2","firebrick2"))


#Chest pain type
hd_trainset %>%  
  ggplot() +
  aes(cp,fill=presence) +
  geom_histogram(stat="count") +
  scale_fill_manual(values=c("springgreen2","firebrick2"))

# Chest pain type, in percentages
hd_trainset %>%  filter(!is.na(cp)) %>% group_by(cp,presence) %>% summarise(count=n()) %>% 
  mutate(perc=count/sum(count))%>%
  ggplot(aes(cp,y=perc,fill=presence)) +
  geom_bar(stat="identity")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  ylab("Percentages")

#Fast blood sugar
hd_trainset %>%  
  ggplot() +
  aes(fbs,fill=presence) +
  geom_histogram(stat="count")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))

# Fast blood sugar, filtering out the N/As, in percentages
hd_trainset %>%  filter(!is.na(fbs)) %>% group_by(fbs,presence) %>% summarise(count=n()) %>% 
  mutate(perc=count/sum(count))%>%
  ggplot(aes(fbs,y=perc,fill=presence)) +
  geom_bar(stat="identity")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  ylab("Percentages")

#Rest ECG
hd_trainset %>%  
  ggplot() +
  aes(restecg,fill=presence) +
  geom_histogram(stat="count")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))

#Exercise induced angina 
hd_trainset %>%  
  ggplot() +
  aes(exang,fill=presence) +
  geom_histogram(stat="count")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))

# Exercise induced angina , filtering out the N/As, in percentages
hd_trainset %>%  filter(!is.na(exang)) %>% group_by(exang,presence) %>% summarise(count=n()) %>% 
  mutate(perc=count/sum(count))%>%
  ggplot(aes(exang,y=perc,fill=presence)) +
  geom_bar(stat="identity")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  ylab("Percentages")


# the slope of the peak exercise ST segment 
hd_trainset %>%  
  ggplot() +
  aes(presence,fill=slope) +
  geom_histogram(stat="count")

# number of major vessels (0-3) colored by flourosopy 
hd_trainset %>%  
  ggplot() +
  aes(ca,fill=presence) +
  geom_histogram(stat="count") +
  scale_fill_manual(values=c("springgreen2","firebrick2"))

# number of major vessels (0-3) colored by flourosopy, filtering out the N/As 
hd_trainset %>%  filter(!is.na(ca)) %>%
  ggplot() +
  aes(ca,fill=presence) +
  geom_histogram(stat="count") +
  scale_fill_manual(values=c("springgreen2","firebrick2"))

# number of major vessels (0-3) colored by flourosopy, filtering out the N/As, in percentages
hd_trainset %>%  filter(!is.na(ca)) %>% group_by(ca,presence) %>% summarise(count=n()) %>% 
  mutate(perc=count/sum(count))%>%
  ggplot(aes(ca,y=perc,fill=presence)) +
  geom_bar(stat="identity")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  ylab("Percentages")

# Thalium stress test result, filtering out the N/As 
hd_trainset %>%  filter(!is.na(thal)) %>%
  ggplot() +
  aes(thal,fill=presence) +
  geom_histogram(stat="count") +
  scale_fill_manual(values=c("springgreen2","firebrick2"))


# Thalium stress test result, filtering out the N/As , in percentages
hd_trainset %>%  filter(!is.na(thal)) %>% group_by(thal,presence) %>% summarise(count=n()) %>% 
  mutate(perc=count/sum(count))%>%
  ggplot(aes(thal,y=perc,fill=presence)) +
  geom_bar(stat="identity")+ 
  scale_fill_manual(values=c("springgreen2","firebrick2"))+
  ylab("Percentages")

# We are facing the issue that some of the features that seem to be important contain a non negligible number of NAs. Here are some statistics:
round(colSums(is.na(hd_trainset))*100/nrow(hd_trainset),0)

# the slope, ca and thal data features are particularly scarce and we will have to be careful when chosing the right prediction method to avoid bad data fitting.


hd_trainset$institute <- NULL
hd_trainset$target <- NULL
hd_valset$institute <- NULL
log<-glm(presence ~ sex+cp+fbs+exang+slope+ca+thal, data=hd_trainset, family=binomial, na.action = na.omit)

##### GLM method
control_glm <- trainControl(method = "cv", number = 10, p = .9)
train_glm <- train(presence ~ sex+cp+fbs+exang+slope+ca+thal, method = "glm",family=binomial, data = hd_trainset,trControl = control_glm ,na.action = na.omit)

#glm_results<-predict(log,hd_valset,type ="response")
glm_results<-predict(train_glm,hd_valset,type ="prob")

##### KNN method
control_knn <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(presence ~ sex+cp+fbs+exang+slope+ca+thal, method = "knn",data = hd_trainset,trControl = control_knn ,tuneGrid = data.frame(k=seq(1,20,1)),na.action = na.omit)

knn_results<-predict(train_knn,hd_valset,type ="prob")

##### Random Forest

set.seed(123)
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

plot(train_rt)
rpart.plot(train_rt$finalModel,   
           type=5,
           fallen.leaves = FALSE,
           box.palette = "GnRd",
           nn=TRUE)



# accuracy calcuclation script
hd_accuracy<-function(fittedModelResult,fittedModelTarget,threshold){
  #accuracy calculation
  mean(1*(fittedModelResult==fittedModelTarget))
}

#AOC UAC function script
sensispeci<-function(fittedModelResulti,fittedDataSet,threshold){
  
  #Remove the NAs
  #keep_index<-complete.cases(fittedDataSet)
  #fittedModelResult<-fittedModelResult[keep_index]
  #fittedModelTarget<-fittedDataSet$presence[keep_index]
  
  keep_index<-complete.cases(fittedDataSet)
  fittedModelResult<-as.numeric(fittedModelResulti)
  fittedModelTarget<-fittedDataSet$presence[keep_index]
  
  #converting results in numeric format for evaluation
  fittedModelResult<-ifelse(fittedModelResult>=threshold,2,1)
  
  # Adding levels for the heart disease presence column
  fittedModelResult<-as.factor(fittedModelResult)
  levels(fittedModelResult)[levels(fittedModelResult)==1] <- "Healthy"
  levels(fittedModelResult)[levels(fittedModelResult)==2] <- "Heart Disease"
  
  #confusion Matrix and sensitivity/specificity calculation
  #conf_matrix<-table(fittedModelResult,fittedModelTarget)
  u <- union(fittedModelResult, fittedModelTarget)
  t <- table(factor(fittedModelResult, u), factor(fittedModelTarget, u))
  conf_matrix<-confusionMatrix(t)$table
  
  output<-c(sensitivity(conf_matrix),specificity(conf_matrix),precision(conf_matrix),F_meas(conf_matrix),hd_accuracy(factor(fittedModelResult, u), factor(fittedModelTarget, u),threshold))#
  #output<-c(sensitivity(conf_matrix),specificity(conf_matrix),precision(conf_matrix),F_meas(conf_matrix),hd_accuracy(fittedModelResult,fittedModelTarget,threshold))#
  #output<-data.frame("Sensitivity"=sensitivity(conf_matrix),"Specificity"=specificity(conf_matrix),"Precision"=precision(conf_matrix),"F_meas"=F_meas(conf_matrix))
  
}

method_metrics<-function(fittedModelResult, fittedDataSet,ml_method){
  k=seq(0,1,0.01)
  aocdata<-sapply(k,sensispeci,fittedModelResult=fittedModelResult,fittedDataSet=fittedDataSet)
  aocdata<-as.data.frame(cbind(k,t(aocdata)))
  aocdata$method<-ml_method
  aocdata<-setNames(aocdata,c("Threshold","Sensitivity","Specificity","Precision","F1_score","Accuracy","Method"))
  }

# Building method metrics
glm_metrics<-method_metrics(glm_results$`Heart Disease`,hd_valset,"glm")
knn_metrics<-method_metrics(knn_results$`Heart Disease`,hd_valset,"knn")
rf_metrics<-method_metrics(rf_results$`Heart Disease`,hd_valset,"rf")
rt_metrics<-method_metrics(rt_results$`Heart Disease`,hd_valset,"rt")


# joining the dataframes
overall_metrics<-rbind(glm_metrics,knn_metrics,rf_metrics,rt_metrics)

## plot overall Accuracy
overall_metrics %>% group_by(Method) %>% ggplot(aes(x=Threshold,y=Accuracy,label = Threshold,col=Method)) +
  geom_line()  + 
  labs(x="Decision Threshold",y="Accuracy") 

## plot overall AOC UAC
overall_metrics %>% group_by(Method) %>% ggplot(aes(x=1-Specificity,y=Sensitivity,label = Threshold,col=Method)) +
  geom_line()  + 
  labs(x="1-specificity",y="Sensitivity")

## plot overall F1_Score
overall_metrics %>% group_by(Method) %>% ggplot(aes(x=Threshold,y=F1_score,label = Threshold,col=Method)) +
  geom_line()  + 
  labs(x="Decision Threshold",y="F1_Score")

## plot AOC UAC
glm_metrics %>% ggplot(aes(x=1-Specificity,y=Sensitivity,label = Threshold)) +
  geom_line()  + 
  geom_point(shape = 21, fill = "red", color = "black", size=3) +
  labs(x="1-specificity",y="Sensitivity") #+ 
  #geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)


## plot precision
glm_metrics %>% ggplot(aes(x=Sensitivity,y=Precision,label = Threshold)) +
  geom_line()  + 
  geom_point(shape = 21, fill = "red", color = "black", size=3) +
  labs(x="sensitivity",y="Precision") + 
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

## F score
glm_metrics %>% ggplot(aes(x=Threshold,y=F1_score,label = Threshold)) +
  geom_line()  + 
  geom_point(shape = 21, fill = "red", color = "black", size=3) +
  labs(x="Decision Threshold",y="F_1 Score") + 
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

## Accuracy
glm_metrics %>% ggplot(aes(x=Threshold,y=Accuracy,label = Threshold)) +
  geom_line()  + 
  geom_point(shape = 21, fill = "red", color = "black", size=3) +
  labs(x="Decision Threshold",y="Overall Accuracy") + 
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

#TODO : add GAMLOESS, naive bayes, svm, lda, see caretList and caretEnsemble