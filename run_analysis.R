#==============================================================================
# File for "Getting and Cleaning Data" on Coursera
#==============================================================================

# define path
subject_test <- "./UCI HAR Dataset/test/subject_test.txt"
x_test <- "./UCI HAR Dataset/test/X_test.txt"
y_test <- "./UCI HAR Dataset/test/y_test.txt"
subject_train <- "./UCI HAR Dataset/train/subject_train.txt"
x_train <- "./UCI HAR Dataset/train/X_train.txt"
y_train <- "./UCI HAR Dataset/train/y_train.txt"
activity_labels <- "./UCI HAR Dataset/activity_labels.txt"
features <- "./UCI HAR Dataset/features.txt"

# read data
subject_test.data <- read.table(subject_test, header=FALSE)
x_test.data <- read.table(x_test, header=FALSE)
y_test.data <- read.table(y_test, header=FALSE)
subject_train.data <- read.table(subject_train, header=FALSE)
x_train.data <- read.table(x_train, header=FALSE)
y_train.data <- read.table(y_train, header=FALSE)
features.data <- read.table(features, header=FALSE)
activity_labels.data <- read.table(activity_labels, header=FALSE)

# merges the training and the test sets to create one data set
test.data <- cbind(subject_test.data, y_test.data, x_test.data)
train.data <- cbind(subject_train.data, y_train.data, x_train.data)
data <- rbind(test.data, train.data)

# remove unnecessary data
remove(subject_test.data)
remove(subject_train.data)
remove(x_test.data)
remove(x_train.data)
remove(y_test.data)
remove(y_train.data)
remove(test.data)
remove(train.data)

# appropriately labels the data set with descriptive variable names
labels <- sapply(features.data[, 2], toString)
labels <- gsub("()", "Val", labels, fixed=TRUE)
labels <- gsub(")", "", labels, fixed=TRUE)
labels <- gsub("(", "_", labels, fixed=TRUE)
labels <- gsub(",", "_", labels, fixed=TRUE)
labels <- gsub("-", "_", labels, fixed=TRUE)
remove(features.data)

# put tidy names on all of the columns.
colnames(data) <- c("Subject", "Activity", labels)

# use descriptive activity names to name the activities in the data set
Activity_list <- sort(unique(data$Activity))
levels(Activity_list) <- activity_labels.data[, 2]
data$Activity <- cut(data$Activity, 6, activity_labels.data[, 2])
remove(activity_labels.data)

# extracts only the measurements on the mean and standard deviation 
# for each measurement. 
filter <- c(grep("stdVal", ignore.case=TRUE, names(data)), 
            grep("meanVal", ignore.case=TRUE, names(data)))

mean_std <- subset(data, select=c(Subject, Activity, filter))

# creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject. 
library(reshape2)
mdata <- melt(mean_std, id=c("Activity", "Subject"))
tidydata <- dcast(mdata, Activity + Subject ~ variable, mean)
write.table(tidydata, "tidydata.txt")
