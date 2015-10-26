library(reshape2)

## 1. Merges the training and the test sets to create one data set.
# improt all data sets
subject_train <- read.table("train/subject_train.txt")
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_test <- read.table("test/subject_test.txt")
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

train <- cbind(subject_train, y_train, X_train)
test <- cbind(subject_test, y_test, X_test)

raw_data <- rbind(train, test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
mean_std <- grep("-(mean|std)\\(\\)", features[, 2])
data <- raw_data[, c(1, 2, mean_std+2)]

## 3. Uses descriptive activity names to name the activities in the data set

activity_labels <- read.table("activity_labels.txt")
data[,2] <- as.character(factor(data[,2], levels=activity_labels[,1], labels=activity_labels[,2]))

## 4. Appropriately labels the data set with descriptive variable names. 

labels <- c("subject", "activity", as.character(features[mean_std,2]))
colnames(data) <- labels

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

melted_data <- melt(data, id=c("subject", "activity"), measure.vars=colnames(data)[-c(1,2)])
summary <- dcast(melted_data, subject + activity ~ variable, mean)

write.table(summary, file="summary.txt", row.names=FALSE)

