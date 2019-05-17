##Analysis script

# Cleaning workspace
rm(list=ls())

# Loading the used libraries
library(tidyverse)
library(caret)
library(ggthemes)


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
