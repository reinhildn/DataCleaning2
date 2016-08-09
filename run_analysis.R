##Data Cleaning Assignment

##training data
subjects <- read.table("./train/subject_train.txt")
names(subjects) <- "subject"
head(subjects)
X_train <- read.table("./train/X_train.txt")
features <- read.table("features.txt")
transfeat <- t(features)
names(X_train) <- transfeat[2,]
activ <- read.table("./train/y_train.txt")
names(activ) <- "activity"
trainComp <- data.frame(subjects,activ,X_train)
head(trainComp)

##test data
subjectsTest <- read.table("./test/subject_test.txt")
names(subjectsTest) <- "subject"
head(subjectsTest)
X_test <- read.table("./test/X_test.txt")
#features <- read.table("features.txt")
#transfeat <- transpose(features)
names(X_test) <- transfeat[2,]
activTest <- read.table("./test/y_test.txt")
names(activTest) <- "activity"
testComp <- data.frame(subjectsTest,activTest,X_test)
head(testComp)

CompData <- rbind(trainComp, testComp)

library(dplyr)
SelData1 <- CompData[,grepl("mean",names(CompData))]
SelData2 <- CompData[,grepl("std",names(CompData))]
SelData <- cbind(CompData[,1:2],SelData1,SelData2)
dim(SelData)

ActivLabel <- read.table("activity_labels.txt")
DataActNew <- merge(SelData,ActivLabel, by.x = "activity", by.y = "V1")
names(DataActNew)[names(DataActNew) == "V2"] <- "activityDesc"

DataActNew <- select(DataActNew, -activity)


##Tidy data set
SelDataA <- DataActNew[,grepl("mean",names(DataActNew))]
SelDataB <- DataActNew[c("subject","activityDesc")]
SelData <- cbind(SelDataB, SelDataA)

Act1 <- filter(SelData, activityDesc == "LAYING")
Act2 <- filter(SelData, activityDesc == "SITTING")
Act3 <- filter(SelData, activityDesc == "STANDING")
Act4 <- filter(SelData, activityDesc == "WALKING")
Act5 <- filter(SelData, activityDesc == "WALKING_DOWNSTAIRS")
Act6 <- filter(SelData, activityDesc == "WALKING_UPSTAIRS")

Act1B <- Act1 %>%
  group_by(subject) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), -activityDesc)
Act1B$activity <- "LAYING"

Act2B <- Act2 %>%
  group_by(subject) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), -activityDesc)
Act2B$activity <- "SITTING"

Act3B <- Act3 %>%
  group_by(subject) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), -activityDesc)
Act3B$activity <- "STANDING"

Act4B <- Act4 %>%
  group_by(subject) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), -activityDesc)
Act4B$activity <- "WALKING"

Act5B <- Act5 %>%
  group_by(subject) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), -activityDesc)
Act5B$activity <- "WALKING_DOWNSTAIRS"

Act6B <- Act6 %>%
  group_by(subject) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)), -activityDesc)
Act6B$activity <- "WALKING_UPSTAIRS"

colnames(Act1B) <- paste("LAY",colnames(Act1B),sep = "_")
colnames(Act2B) <- paste("SIT",colnames(Act2B),sep = "_")
colnames(Act3B) <- paste("STAND",colnames(Act3B),sep = "_")
colnames(Act4B) <- paste("WALK",colnames(Act4B),sep = "_")
colnames(Act5B) <- paste("UPS",colnames(Act5B),sep = "_")
colnames(Act6B) <- paste("DOWNS",colnames(Act6B),sep = "_")

TidyData <- merge(Act1B,Act2B, by.x = "LAY_subject", by.y = "SIT_subject")
TidyData <- merge(TidyData,Act3B, by.x = "LAY_subject", by.y = "STAND_subject")
TidyData <- merge(TidyData,Act4B, by.x = "LAY_subject", by.y = "WALK_subject")
TidyData <- merge(TidyData,Act5B, by.x = "LAY_subject", by.y = "UPS_subject")
TidyData <- merge(TidyData,Act6B, by.x = "LAY_subject", by.y = "DOWNS_subject")

names(TidyData)[names(TidyData) == "LAY_subject"] <- "subject"
TidyData <- select(TidyData, -LAY_activity)
TidyData <- select(TidyData, -SIT_activity)
TidyData <- select(TidyData, -STAND_activity)
TidyData <- select(TidyData, -WALK_activity)
TidyData <- select(TidyData, -UPS_activity)
TidyData <- select(TidyData, -DOWNS_activity)
dim(TidyData)

write.table(TidyData, file = "TidyData.txt", append = FALSE, sep = "|")
