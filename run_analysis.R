filename <- "UCIdata.zip"

#Download
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

#Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#reading features
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("n","functions"))

#reading activity
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

#reading test
test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

#reading train
train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#1.Merges the training and the test sets to create one data set
X <- rbind(x_test, x_train)
Y <- rbind(y_test, y_train)
Dataset <- rbind(test, train)
Merged <- cbind(Subject, Y, X)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
##I'm using the %>% operator 
TidyData <- Merged %>% select(subject, code, contains("mean"), contains("std"))
Dataset.tidy <- select((Merged), subject, code, contains("mean"), contains("std"))

#3.Uses descriptive activity names to name the activities in the data set
Dataset.tidy$code <- activities[Dataset.tidy$code, 2]

#4.Appropriately labels the data set with descriptive variable names.
##Open the txt file to check the variables and:
names(Dataset.tidy)[2] = "activity"
names(Dataset.tidy)<-gsub("BodyBody", "Body", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("Acc", "Accelerometer", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("gravity", "Gravity", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("Gyro", "Gyroscope", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("Mag", "Magnitude", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("^t", "Time", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("^f", "Frequency", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("tBody", "TimeBody", names(Dataset.tidy))
names(Dataset.tidy)<-gsub("-mean()", "Mean", names(Dataset.tidy), ignore.case = TRUE)
names(Dataset.tidy)<-gsub("-std()", "STD", names(Dataset.tidy), ignore.case = TRUE)
names(Dataset.tidy)<-gsub("-freq()", "Frequency", names(Dataset.tidy), ignore.case = TRUE)
names(Dataset.tidy)<-gsub("angle", "Angle", names(Dataset.tidy))

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Use magrittr library to be able to use %>% and make a chain in an easier way
library(dplyr)
library(magrittr)
Final.Dataset <- Dataset.tidy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Final.Dataset, "Final.Dataset.txt", row.name=FALSE)


                              