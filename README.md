# CleaningDataAssignment
This is the final assignment for the Coursera "Getting and Cleaning Data" class. The assignment consists of processing raw accelerometer data from a Samsung Galaxy 5 smartphone and creating summary data that is tidy.
 
## Study Design
Information from this study come from: Human Activity Recognition Using Smartphones Dataset
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### Study authors:
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws


The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

## Files
1. fitData.zip - Raw data for the study. This raw data file is not needed unless you are offline. The script will automatically download the data file if it is not available locally.
2. run_analysis.R - Script for cleaning and tidying data
3. tidydata.txt - the tidy data file
4. CodeBook.txt - the code book describing variables and procedures
5. Readme.md - this file

## Notes and License
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 
For more information about this dataset contact: activityrecognition@smartlab.ws

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

 
