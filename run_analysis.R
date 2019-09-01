#0. Load and prepare the test and training data
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
head(features)
class(features$V1)
class(features$V2)

features <- as.character(features[,2])

xtrain <- read.table('./UCI HAR Dataset/train/X_train.txt')
head(xtrain)
dim(xtrain)

ytrain <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
head(ytrain)
dim(ytrain)

subjecttrain <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

traindata <-  data.frame(subjecttrain, ytrain, xtrain)

names(traindata) <- c(c('subject', 'activity'), features)

head(traindata)
dim(traindata)

xtest <- read.table('./UCI HAR Dataset/test/X_test.txt')
ytest <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
subjecttest <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

testdata <-  data.frame(subjecttest, ytest, xtest)
names(testdata) <- c(c('subject', 'activity'), features)


#1. Merge test and training into one big df
df <- rbind(traindata, testdata)

#2. Extract only the measurements on the mean and standard deviation for each measurement.
msd <- grep('mean|std', features)
extract <- df[,c(1,2,msd + 2)]

#3. Use descriptive activity names to name the activities in the data set
names <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
names <- as.character(names[,2])
extract$activity <- names[extract$activity]
table(extract$activity)

#4. Appropriately label the data set with descriptive variable names.
varnames <- names(extract)
varnames <- gsub("[(][)]", "", varnames)
varnames <- gsub("^t", "TimeDomain_", varnames)
varnames <- gsub("^f", "FrequencyDomain_", varnames)
varnames <- gsub("Acc", "Accelerometer", varnames)
varnames <- gsub("Gyro", "Gyroscope", varnames)
varnames <- gsub("Mag", "Magnitude", varnames)
varnames <- gsub("-mean-", "_Mean_", varnames)
varnames <- gsub("-std-", "_StandardDeviation_", varnames)
varnames <- gsub("-", "_", varnames)
names(extract) <- varnames

#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydf <- aggregate(extract[,3:81], by = list(activity = extract$activity, subject = extract$subject),FUN = mean)
write.table(x = tidydf, file = "tidydf.txt", row.names = FALSE)

