# Load general data sets

activity_type <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/activity_labels.txt")
features_names <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/features.txt")

# Load testing data sets 

volunteer_id_test <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/test/subject_test.txt")
activity_label_test <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/test/y_test.txt")
activity_records_test <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/test/X_test.txt")

# Load training data sets

volunteer_id_train <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/train/subject_train.txt")
activity_label_train <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/train/y_train.txt")
activity_records_train <- read.table("~/Online Courses/Johns Hopkins University Data Science Foundations using R Specialization/Assignment/Module 3/UCI HAR Dataset/train/X_train.txt")

#Name the columns
colnames(activity_type) <- c("Index", "Activity Type")
colnames(features_names) <- c("Index", "Feature Name")
colnames(volunteer_id_test) <- c("ID")
colnames(activity_label_test) <- c("Activity Type Index")
test_record_col_names <- features_names$`Feature Name`
colnames(activity_records_test) <- test_record_col_names
activity_records_test2 <- activity_records_test[, grepl("mean|std", colnames(activity_records_test))]
## Insert a spacing between a lower and upper case, replace hyphens with a spacing and remove parethesis from all column names 
colnames(activity_records_test2) <- gsub("([a-z])([A-Z])|-|(\\(.*\\))", "\\1 \\2", colnames(activity_records_test2))

colnames(volunteer_id_train) <- c("ID")
colnames(activity_label_train) <- c("Activity Type Index")
colnames(activity_records_train) <- test_record_col_names
activity_records_train2 <- activity_records_train[, grepl("mean|std", colnames(activity_records_train))]
colnames(activity_records_train2) <- gsub("([a-z])([A-Z])|-|(\\(.*\\))", "\\1 \\2", colnames(activity_records_train2))

# Combine test tables using cbind
test <- cbind(volunteer_id_test, activity_label_test, activity_records_test2)

# Combine train tables using cbind 
train <- cbind(volunteer_id_train, activity_label_train, activity_records_train2)

# Combine train and test tables using rbind
combinedf <- rbind(test, train)


# Create an additional column: Activity Type for easy reading (test data) 
combinedf2 <- data.frame("Activity Name" = activity_type$`Activity Type`[match(combinedf$`Activity Type Index`, activity_type$Index)])
combinedf2 <- cbind(combinedf, combinedf2)

# Change column position 
combinedf2 <- combinedf2[,c(1:2, 82, 3:81)]

# Change column name
colnames(combinedf2)[colnames(combinedf2) == "Activity.Name"] <- "Activity Name"

# Create second, independent tidy data set
## Convert ID, Activity Type Index and Activity Name to factors
combinedf2$ID <- as.factor(combinedf2$ID)
combinedf2$`Activity Type Index` <- as.factor(combinedf2$`Activity Type Index`)
combinedf2$`Activity Name` <- as.factor(combinedf2$`Activity Name`)

## Variables to compute mean 
variables_names <-colnames(combinedf2)[4:ncol(combinedf2)]

## Compute means for each variables based on unique ID, Activity Type Index and Activity Name 
means <- aggregate(. ~ combinedf2$ID + combinedf2$`Activity Type Index` + combinedf2$`Activity Name`, data = combinedf2, FUN = mean)

## Rename column names 
new_col_names <- c("ID", "Activity Type Index", "Activity Name")
colnames(means)[c(1:3)] <- new_col_names

## Drop columns and order data by ID 
tidydata <- means[, -c(4:6)]
tidydata <- tidydata[order(tidydata$ID),]

# Save as text file 
file_path <- "tidydata.txt"
write.table(tidydata, file = file_path, sep ="\t", row.names = FALSE)
cat("Dataframe saved as:", file_path, "\n")

