#
# Load necessary libraries ------------------------------------------------
# 

# make sure packages are installed and if not install them
# Thanks to Shane on Stack Overflow http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
list.of.packages <- c("reshape2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(reshape2)

#
# define some useful strings so I don't fat finger them later
#

datadir <- "./data"
testdir <- paste(datadir, "/UCI HAR Dataset/test", sep="")
traindir <- paste(datadir, "/UCI HAR Dataset/train", sep="")
testsubfile <- paste(testdir, "/subject_test.txt", sep="")
testXfile <- paste(testdir, "/X_test.txt", sep="")
testYfile <- paste(testdir,"/Y_test.txt", sep="")
trainsubfile <- paste(traindir, "/subject_train.txt", sep="")
trainXfile <- paste(traindir, "/X_train.txt", sep="")
trainYfile <- paste(traindir, "/Y_train.txt", sep="")
featurefile <- paste(datadir, "/UCI HAR Dataset/features.txt", sep="")

# 
# GetData -----------------------------------------------------------------
#
# Check to see if data directory exists and, if not, create it
# Download data file and put it in destination file ./data


if(!file.exists(datadir)) {
  dir.create(datadir)
}

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Check if the file is compressed
zipExtensions <- c(".zip$", ".gz$", ".tz$", ".bz2")
for(i in zipExtensions) {
  if(grepl(i, fileURL)) {
    isZip = TRUE
    break
  }
}

if(isZip) {
  tempfile <- tempfile()
  download.file(fileURL, tempfile, method="curl")
  unzip(tempfile, exdir = datadir)
  unlink(tempfile)
}

# This assignment only uses zip files.  I'll build out this section with some kind of switch function based on file extension later and probably keep it as a separate "utility script".


# Open files and Create Tidy Data Frames -----------------------------------------------

testsub_df <- data.frame(read.table(testsubfile))
testY_df <- data.frame(read.table(testYfile))
testX_df <- data.frame(read.table(testXfile))
trainsub_df <- data.frame(read.table(trainsubfile))
trainX_df <- data.frame(read.table(trainXfile))
trainY_df <- data.frame(read.table(trainYfile))

features <- read.table(featurefile)
X_df <- rbind(testX_df, trainX_df)
colnames(X_df) <- features$V2

test_df <- cbind(testsub_df, testY_df)
train_df <- cbind(trainsub_df, trainY_df)
df_1 <- rbind(test_df, train_df)

activities <- c("walking", "walking up stairs", "walking down stairs", "sitting", "standing", "laying")
colnames(df_1) <- c("subject", "activitynum")
df_1$activity <- activities[df_1$activitynum]
df <- cbind(df_1, X_df)
drops <- c("activitynum")
df <- df[ , !(names(df) %in% drops)]

# print(head(df))

# I think the data is pretty tidy at this point.  Each row represents measurements of a subject performing a single activity and each column has only
# a single variable.  The column headers are descriptive, lower-case and without spaces or other special characters.
# 

# keep variables that calculate a mean or a standard deviation
# print(dim(df))
keepers <- grep("std\\()|mean\\()|^activity$|^subject$", names(df))
df <- df[ , keepers]
# print(dim(df))

# Group data by subject and by activity then take means and standard deviations across the measured feature columns
# I think this is what the instructions asked us to do but it was kind of a confusing description.
#
subjects <- group_by(df, subject)
tasks <- group_by(df, activity)

subject_means <- ddply(subjects, c("subject", "activity"), numcolwise(mean))
task_means <- ddply(tasks, c("subject", "activity"), numcolwise(mean))
mean_names <- paste("mean of", names(subject_means)[3:68])
colnames(subject_means) <- c("subject", "activity", mean_names)
colnames(task_means) <- c("subject", "activity", mean_names)

subject_sds <- ddply(subjects, c("subject", "activity"), numcolwise(sd))
task_sds <- ddply(tasks, c("subject", "activity"), numcolwise(sd))
sd_names <- paste("sd of", names(subject_means)[3:68])
colnames(subject_sds) <- c("subject", "activity", sd_names)
colnames(task_sds) <- c("subject", "activity", sd_names)

# Write tables out to datadir
write.table(subject_means, file=paste(datadir, "/subject_means.txt", sep=""))
write.table(subject_sds, file=paste(datadir,"/subject_sds.txt", sep=""))
write.table(task_means, file=paste(datadir,"/tasks_means.txt", sep=""))
write.table(task_sds, file=paste(datadir,"/tasks_sds.txt", sep=""))
