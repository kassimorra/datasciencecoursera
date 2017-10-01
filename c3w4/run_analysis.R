library(dplyr)
library(tidyr)

##Read activity file
activity_labels <- read.csv("UCI HAR Dataset/activity_labels.txt", header=F, 
                            col.names = c("idactivity", "activity"), sep = "")

##Read feature file
features <- read.csv("UCI HAR Dataset/features.txt", header=F, 
                         col.names = c("id", "label"), sep = "")


##Read train and test file with measures
dsx <- rbind(
  read.csv("UCI HAR Dataset/train/X_train.txt", header=F, sep=""),  
  read.csv("UCI HAR Dataset/test/X_test.txt", header=F, sep="")
)
##rename de columns
colnames(dsx) <- t(features$label)
##keep just mean and std measures
dsx <- dsx[,grepl("mean\\(\\)|std\\(\\)|activity|subject", colnames(dsx))]

##read the activity for each row from dsx
dsy <- rbind(
  read.csv("UCI HAR Dataset/train/y_train.txt", header=F, sep="", col.names = "idactivity"),
  read.csv("UCI HAR Dataset/test/y_test.txt", header=F, sep="", col.names = "idactivity")
)

dsy <- merge(dsy, activity_labels, by = "idactivity")

#read the file that contain the subject from each row of dsx
dssubject <- rbind(
  read.csv("UCI HAR Dataset/train/subject_train.txt", header=F, sep="", col.names = 'subject'),
  read.csv("UCI HAR Dataset/test/subject_test.txt", header=F, sep="", col.names = 'subject')
)
  
##combine all datasets to create a new one
ds <- cbind(dssubject, dsy, dsx)

##create a tidy dataset to analyse and calculate mean for all measures
ds_independent <- ds %>% group_by(subject, activity) %>% summarise_all(funs (mean))



