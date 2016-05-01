run_analysis <- function() {

## load dependent libraries
     library(dplyr)
     library(tidyr)

## set up, create directory for data

     if(basename(getwd()) != "CleaningAssignment") {
          if( !dir.exists("CleaningAssignment") ) {
               dir.create("CleaningAssignment")
          }
               setwd("CleaningAssignment")
     }

## download the relevant data set if it doesn't exist already
     if( !file.exists("fitdata.zip")) {
          fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
          download.file( fileUrl, destfile = "fitData.zip")
          dateDownloaded <- date()
     }
## extract activity files from archive
     activityLbl <- read.table(unz("fitData.zip","UCI HAR Dataset/activity_labels.txt" ))
     featuresLbl <- read.table(unz("fitData.zip","UCI HAR Dataset/features.txt" ))
     testLbl <- read.table(unz("fitData.zip","UCI HAR Dataset/test/y_test.txt" ), col.names="activities")
     trainLbl <- read.table(unz("fitData.zip","UCI HAR Dataset/train/y_train.txt" ), col.names="activities")
     test.data <- read.table(unz("fitData.zip","UCI HAR Dataset/test/X_test.txt" ), col.names = featuresLbl[,2])
     train.data <- read.table(unz("fitData.zip","UCI HAR Dataset/train/X_train.txt" ), col.names = featuresLbl[,2])

## combine test, activity tables and labels
     test.data = mutate( test.data, subj="test")
     test.data = cbind(testLbl, test.data)
     train.data = mutate( train.data, subj="train")
     train.data = cbind(trainLbl, train.data)
     alldata <- rbind(test.data, train.data)

## refactor activities column with activities names
     alldata$activities <- factor(alldata$activities)     
     levels(alldata$activities) <- factor(tolower(activityLbl[[2]]) ) 
     
## subset data so that only mean and sd measurements are in table
     alldata = select(alldata, subj, activities, contains(".mean"), contains(".std"), -contains("meanFreq"), -contains("gravityMean"))

## aggregate data by activity and subject, remove unneeded columns
     widedata <- aggregate(alldata, by=list(activity=alldata$activities, subject=alldata$subj), FUN=mean)
     widedata <- select(widedata, -activities, -subj)
     
## rename "magnitude" variables with suffix ...mag in order to use tidyr to separate
     names(widedata) <- gsub("Mag.std..", "Mag.std...mag",names(widedata))
     names(widedata) <- gsub("Mag.mean..", "Mag.mean...mag",names(widedata))

## create tall column
     tidydata <- gather(widedata, measure, value, -activity, -subject)
     tidydata <- separate(tidydata, measure, c("measurement","dimension"), sep="\\.\\.\\.")
     tidydata <- separate(tidydata, measurement, c("measurement","statistic"))
     tidydata
}