
# This R script called run_analysis.R that does the following:

#Step 1) Merges the training and the test sets to create one data set.
#Step 2) Extracts only the measurements on the mean and standard deviation for each measurement.
#Step 3) Uses descriptive activity names to name the activities in the data set
#Step 4) Appropriately labels the data set with descriptive variable names.
#Step 5) From the data set in step 4, creates a second, independent tidy data set with the average 
#        of each variable for each activity and each subject.

# Step 1: Read all the data sets and merge tehm by row.

merge_subject <- function() {
        subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
        subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
        subject_complete <- rbind(subject_train, subject_test)
        names(subject_complete) <- "subject"
        subject_complete
}

merge_X <- function() {
        X_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
        X_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
        X_complete  <- rbind(X_train, X_test)
}

merge_y <- function() {
        y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
        y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
        y_complete  <- rbind(y_train, y_test)
}

subject_data <- merge_subject()
X_data <- merge_X()
y_data <- merge_y()





# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.  

extract_measurement <- function() {
        features <- read.table('./UCI HAR Dataset/features.txt', header=FALSE, col.names=c('id', 'name'))
        feature_meansd <- grep('mean\\(\\)|std\\(\\)', features$name)
        X_data_filtered <- X_data[, feature_meansd]
        names(X_data_filtered) <- features[features$id %in% feature_meansd, 2]
        X_data_filtered
}

X_measurements <- extract_measurement()

# Step 3: Uses descriptive activity names to name the activities in the data set

activity_names <- read.table('./UCI HAR Dataset/activity_labels.txt', header=FALSE, col.names=c('id', 'name'))


# Step 4: Appropriately labels the data set with descriptive activity names. 

y_data[, 1] = activity_names[y_data[, 1], 2]
names(y_data) <- "activity"


# Step 5: Create tidy data set with the average of each variable for 
#         each activity and each subject.

complete_data <- cbind(subject_data, y_data, X_measurements)
write.csv(complete_data, "./complete_data.csv")

measurements <- complete_data[, 3:dim(complete_data)[2]]
tidy_data <- aggregate(measurements, list(complete_data$subject, complete_data$activity), mean)
names(tidy_data)[1:2] <- c('subject', 'activity')
write.csv(tidy_data, "./tidy_data.csv")
write.table(tidy_data, "./tidy_data.txt", row.name=FALSE)
