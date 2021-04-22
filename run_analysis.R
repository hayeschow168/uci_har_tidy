library(data.table)
library(dplyr)

#read files
test_subject <- fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
test_x <- fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/x_test.txt")
test_y <- fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

train_subject <- fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
train_x <- fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/x_train.txt")
train_y <- fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

activity_labels <- fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
features <-fread(file="getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")

# rename columns, so that it can be joined
# make activity labels more descriptive
colnames(activity_labels) <- c("activity-V1", "activity")

activity_labels$activity <- tolower(activity_labels$activity)
activity_labels$activity <- sub("_", " ", activity_labels$activity)

#make unique each each feature label by combining column 1 and 2
features <- features %>% mutate(V12 = paste(as.character(V1), V2, sep = "-") )

# rename columns to make more desriptive
colnames(test_subject) <- "subject"
colnames(test_x) <- features$V12
# rename column so it can be joined
colnames(test_y) <- "activity-V1"

# rename columns to make more desriptive
colnames(train_subject) <- "subject"
colnames(train_x) <- features$V12
# rename column so it can be joined
colnames(train_y) <- "activity-V1"

# select only mean or std columns
test_x <- test_x %>% select(grep("(M|m)ean|std", names(test_x), value=TRUE))
train_x <- train_x %>% 
select(grep("(M|m)ean|std", names(train_x), value=TRUE))
# bind columns together and merge dt horizontally
test_sy <- cbind(test_subject, test_y)
test_sya <- inner_join(test_sy, activity_labels)
test_syax <- cbind(test_sya, test_x) #%>% select(-`activity-V1`)
test_syaxt <- cbind(test_syax, `test / train` = rep("test", nrow(test_syax)))

train_sy <- cbind(train_subject, train_y)
train_sya <- inner_join(train_sy, activity_labels)
train_syax <- cbind(train_sya, train_x)
train_syaxt <- cbind(train_syax, `test / train` = rep("train", nrow(train_syax)))

# merge datasets vertically
m <- rbind(test_syaxt, train_syaxt) %>% select(-`activity-V1`)

# make colnames more descriptive
colnames(m) <- gsub("\\d+-", "", names(m))  # remove prefix digits
colnames(m) <- gsub("\\(\\)", "", names(m)) # remove parentheses ()
colnames(m) <- gsub("\\(", " ", names(m)) # remove parentheses (
colnames(m) <- gsub("\\)", "", names(m)) # remove parentheses )

colnames(m) <- gsub("-", " ", names(m))     # replace dash with spaces
colnames(m) <- gsub(",", " ", names(m))  # replace commas

colnames(m) <- gsub("([a-z])([A-Z])", "\\1 \\2", names(m)) # split names
colnames(m) <- gsub("([a-zA-Z]+) \\1", "\\1", names(m))    # remove duplicates

colnames(m) <- tolower(names(m))


# tidy dataset
tidy <- m %>% group_by(subject, activity) %>% 
  summarize(across(c("t body acc mean x" : "f body gyro jerk mag mean freq"), 
                   mean, na.rm = TRUE))

          
# output tidy data to csv
write.csv(tidy,"uci_har_tidy.txt", row.names = FALSE)
