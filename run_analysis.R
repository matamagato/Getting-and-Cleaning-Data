# 1. Merges the training and the test sets to create one data set.

#Read in Train data
trainData <- read.table("./train/X_train.txt")
trainLabel <- read.table("./train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./train/subject_train.txt")

#Read in Test data
testData <- read.table("./test/X_test.txt")
testLabel <- read.table("./test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./test/subject_test.txt")

#merging Train and Test
mergedData <- rbind(trainData, testData)
mergedLabel <- rbind(trainLabel, testLabel)
mergedSubject <- rbind(trainSubject, testSubject)


# 2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("./features.txt")
meanStd <- grep("mean\\(\\)|std\\(\\)", features[, 2])
mergedData <- mergedData[, meanStd]

#remove "()"
names(mergedData) <- gsub("\\(\\)", "", features[meanStd, 2])
#remove "-" in column names
names(mergedData) <- gsub("-", "", names(mergedData))

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
activityLabel <- activity[mergedLabel[, 1], 2]
mergedLabel[, 1] <- activityLabel
names(mergedLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(mergedSubject) <- "subject"
cleanedData <- cbind(mergedSubject, mergedLabel, mergedData)
write.table(cleanedData, "Merged data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
#getting dimensions for dataFrame
subjectLen <- length(table(mergedSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
#creating matrix
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)

#row is a counter variable
row <- 1
#filling up the data frame with averages
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(mergedSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
#create second independant data set
write.table(result, "CleanedData with means.txt")