# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. From the data set in step 4, create a second, independent tidy data set with the average 
#    of each variable for each activity and each subject. 

##########################################################################################################

# ad 1

#set working directory
setwd("/Users/anina/Documents/Coursera/Data Science/Getting and Cleaning Data/UCI HAR Dataset")

# Read in the data, especially train data, from files
features <- read.table("./features.txt", header = FALSE)
activity_labels <- read.table("./activity_labels.txt", header = FALSE)
subject_train <- read.table("./train/subject_train.txt", header = FALSE)
x_train <- read.table("./train/x_train.txt", header = FALSE)
y_train <- read.table("./train/y_train.txt", header = FALSE)

# Give column names
colnames(activity_labels) <- c("activity_id", "activity_type")
colnames(subject_train) <- "subject_id"
colnames(x_train) <- features[, 2] 
colnames(y_train) <- "activity_id"

# Merge training set
train_data <- cbind(y_train, subject_train, x_train)

# Read in the test data
subject_test <- read.table("./test/subject_test.txt", header = FALSE)
x_test <- read.table("./test/x_test.txt", header = FALSE)
y_test <- read.table("./test/y_test.txt", header = FALSE)

# Give column names
colnames(subject_test) <- "subject_id"
colnames(x_test) <- features[, 2] 
colnames(y_test) <- "activity_id"


# Merge test set
test_data <- cbind(y_test, subject_test, x_test)


# Merge test set and training set data
final_data <- rbind(train_data, test_data)


# ad 2

col_names <- colnames(final_data)

# Boolean vector: TRUE equals columns ID, mean() & stddev() else FALSE
log_vec <- (grepl("activity..", col_names) |
            grepl("subject..", col_names) | 
            grepl("-mean..", col_names) & 
            !grepl("-meanFreq..", col_names) &
            !grepl("mean..-", col_names) |
            grepl("-std..", col_names) &
            !grepl("-std()..-", col_names))

# Keep only the desired columns
final_data <- final_data[log_vec == TRUE]


# ad 3

# Include descriptive activity names
final_data <- merge(final_data, 
                    activity_labels, 
                    by = "activity_id",
                    all.x = TRUE)

col_names  <- colnames(final_data) 


# ad 4

# Cleaning up the variable names
for (i in 1:length(col_names)) {
  col_names[i] = gsub("\\()","", col_names[i])
  col_names[i] = gsub("-std$","StdDev", col_names[i])
  col_names[i] = gsub("-mean","Mean", col_names[i])
  col_names[i] = gsub("^(t)","time", col_names[i])
  col_names[i] = gsub("^(f)","freq", col_names[i])
  col_names[i] = gsub("([Gg]ravity)","Gravity", col_names[i])
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body", col_names[i])
  col_names[i] = gsub("[Gg]yro","Gyro", col_names[i])
  col_names[i] = gsub("AccMag","AccMagnitude", col_names[i])
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude", col_names[i])
  col_names[i] = gsub("JerkMag","JerkMagnitude", col_names[i])
  col_names[i] = gsub("GyroMag","GyroMagnitude", col_names[i])
}

colnames(final_data) <- colNames


# ad 5

# Create a new table
final_data_no_act <- final_data[, names(final_data) != "activity_type"]

# Include just the mean of each variable for activity and subject
tidy_data <- aggregate(final_data_no_act[, names(final_data_no_act) != c("activity_id", "subject_id")], 
                       by = list(activity_id = final_data_no_act$activity_id,
                                 subject_id = final_data_no_act$subject_id),
                       mean)

# Include descriptive acitvity names
tidy_data <- merge(tidy_data,
                   activity_labels,
                   by = "activity_id", all.x = TRUE)

# Export the now "tidy data" 
write.table(tidy_data, "./tidyData.txt",
            row.names = FALSE,
            sep = "\t")