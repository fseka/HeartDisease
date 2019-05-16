library(tidyverse)
library(caret)

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

# Adding the presence boolean column: the binary heart disease presence column is built based on the presence column and is a simplification of the obersvation.
# While the presence takes integer values from 0 to 4 (0 being the absolute absence of disease), this new value presence will indicate presence (presence=1,2,3or4) or absence (presence=0).

heartdisease <- mutate(heartdisease,presence=!(target==0))

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
val_index <- createDataPartition(y=heartdisease$bhdisp, times=1, p=0.2, list=FALSE)
hd_trainset <- heartdisease[-val_index,] #defining the heart disease training data set
hd_valset <- heartdisease[val_index,] #defining the heart disease validation data set

## Feature exploration
# It is now possible to look closely to the different features in the training set. This first exploration will be carried out by means of data visualisation.

# Age
hd_trainset %>%
  ggplot() +
  aes(age,fill=presence) +
  geom_density(alpha = 0.4) +
  xlab("Age") +
  ylab("Density") + 
  scale_fill_manual(name="Disease", labels=c("Absent","Present"),values=c("springgreen2","firebrick2"))


#[TODO]: ajouter le notion de levels pour que les plots puissent se faire
#Sex
hd_trainset %>% filter (presence==TRUE) %>%
  ggplot(aes(age,fill=sex)) +
  geom_density(alpha = 0.4)
