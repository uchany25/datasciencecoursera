### download file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", file.path(getwd(), "Dataset.zip"))
unzip(zipfile = "dataFiles.zip")


### check file lists in dataFiles folder
list.files("UCI HAR Dataset")
### [1] "activity_labels.txt" "features.txt"        "features_info.txt"   "README.txt"         
### [5] "test"                "train"   

### load activity_labels & features

activitylabels <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/activity_labels.txt"), col.names = c("classlabels", "Activityname"))
features <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/features.txt"), col.names = c("index", "Featurenames"))

### Extract measurements (mean & std)

selected_features <- grep("(mean|std)\\(\\)", features[,Featurenames])

measurements <- features[selected_features, Featurenames]
measurements <- gsub("[()]", "", measurements)



### check train folder
list.files("UCI HAR Dataset/train")
### [1] "Inertial Signals"  "subject_train.txt"   "X_train.txt"       "y_train.txt"    


### load train
train <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/train/x_train.txt"))[,selected_features, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
train_subject <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/train/subject_train.txt"), col.names = "subject")
train_Activity <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/train/y_train.txt"), col.names = "Activity")
train_a <- cbind(train_subject, train_Activity, train)


### check test folder
list.files("UCI HAR Dataset/test")
### [1] "Inertial Signals" "subject_test.txt" "X_test.txt"       "y_test.txt"    

### load test
test <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/test/x_test.txt"))[, selected_features, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
test_subject <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt"), col.names = "subject")
test_Activity <- data.table::fread(file.path(getwd(), "UCI HAR Dataset/test/y_test.txt"),  col.names = "Activity")
test_a <- cbind(test_subject, test_Activity, test)

### merged dataset
merged_data <- rbind(train_a, test_a)


### tidy dataset

merged_data[["Activity"]] <- factor(merged_data[, Activity]
                                 , levels = activitylabels[["classlabels"]]
                                 , labels = activitylabels[["Activityname"]])

merged_data[["subject"]] <- as.factor(merged_data[, subject])
merged_data_long <- reshape2::melt(merged_data, id = c("subject", "Activity"))
merged_data_wide <- reshape2::dcast(data = merged_data_long, subject + Activity ~ variable, fun.aggregate = mean)
data.table::fwrite(x = merged_data, file = "tidyData.txt", quote = FALSE)
