trainproj = read.csv("/Users/manasa/Documents/MachineLearning/Project/traindata.csv")
testproj = read.csv("/Users/manasa/Documents/MachineLearning/Project/testdata.csv")
trainlabels = read.csv("/Users/manasa/Documents/MachineLearning/Project/trainlabels.csv")
trainproj <- merge(trainproj, trainlabels, by="id")
summary(trainproj)
#Date recorded - remove, 
#but add 2 derived features : # of days since Jan 1 2014, month recorded as factor
#############################################################################################
date_recorded_offset_days <- as.numeric(as.Date("2014-01-01") - as.Date(trainproj$date_recorded))
date_recorded_month <- factor(format(as.Date(trainproj$date_recorded), "%b"))
trainproj <- trainproj[, -which(names(trainproj) == "date_recorded")]
trainproj <- cbind(trainproj, date_recorded_offset_days)
trainproj <- cbind(trainproj, date_recorded_month)

date_recorded_offset_days <- as.numeric(as.Date("2014-01-01") - as.Date(testproj$date_recorded))
date_recorded_month <- factor(format(as.Date(testproj$date_recorded), "%b"))
testproj <- testproj[, -which(names(testproj) == "date_recorded")]
testproj <- cbind(testproj, date_recorded_offset_days)
testproj <- cbind(testproj, date_recorded_month)
#############################################################################################

trainproj$num_private = NULL
testproj$num_private = NULL
trainproj$recorded_by = NULL
testproj$recorded_by = NULL
trainproj$extraction_type_group = NULL
testproj$extraction_type_group = NULL
trainproj$extraction_type_class = NULL
testproj$extraction_type_class = NULL

trainproj$region_code = as.factor(trainproj$region_code)
testproj$region_code = as.factor(testproj$region_code)

trainproj$district_code = as.factor(trainproj$district_code)
trainproj$district_code = NULL
testproj$district_code = NULL
trainproj$region = NULL
testproj$region = NULL
trainproj$quantity = NULL
testproj$quantity = NULL
length(levels(trainproj$source_type))
length(levels(trainproj$source))
trainproj$source_type = NULL
testproj$source_type = NULL
trainproj$quality_group = NULL
testproj$quality_group = NULL
trainproj$waterpoint_type_group = NULL
testproj$waterpoint_type_group = NULL
trainproj$payment_type = NULL
testproj$payment_type = NULL

trainproj$lga = NULL
testproj$lga = NULL

trainproj$ward = NULL
testproj$ward = NULL

trainproj$scheme_name = NULL
testproj$scheme_name = NULL

trainproj$wpt_name = NULL
testproj$wpt_name = NULL

trainproj$subvillage = NULL
testproj$subvillage = NULL

sum(duplicated(trainproj)==TRUE)
#Funder - reduce factor levels
NUM_LEVELS_FUNDER = 10 #Funder will have this many + 1 levels
#############################################################################################
funderNames <- names(summary(trainproj$funder)[1:NUM_LEVELS_FUNDER])
funder <- factor(trainproj$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
trainproj$funder <- funder

funder <- factor(testproj$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
testproj$funder <- funder
#############################################################################################
#Installer - reduce factor levels
NUM_LEVELS_INSTALLER = 10 #Installer will have this many + 1 levels
#############################################################################################
installerNames <- names(summary(trainproj$installer)[1:NUM_LEVELS_INSTALLER])
installer <- factor(trainproj$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
trainproj$installer <- installer

installer <- factor(testproj$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
testproj$installer <- installer
#############################################################################################
#Construction year - turn into factor, reduce factor levels
NUM_LEVELS_CONSTRUCTION_YEAR = 20 #constructi on_year will have this many + 1 levels
#############################################################################################
trainproj$construction_year <- factor(paste0("y",as.character(trainproj$construction_year)))
cyears <- names(summary(trainproj$construction_year)[order(-summary(trainproj$construction_year))][1:NUM_LEVELS_CONSTRUCTION_YEAR])
cy <- factor(trainproj$construction_year, levels=c(cyears, "Other"))
cy[is.na(cy)] <- "Other"
trainproj$construction_year <- cy

testproj$construction_year <- factor(paste0("y",as.character(testproj$construction_year)))
cy <- factor(testproj$construction_year, levels=c(cyears, "Other"))
cy[is.na(cy)] <- "Other"
testproj$construction_year <- cy
#############################################################################################

#extraction_type - Change level 
#"other - mkulima/shinyanga"(Not present in test) to "Other"
trainproj$extraction_type[trainproj$extraction_type=="other - mkulima/shinyanga"] <- "other"
trainproj$extraction_type <- factor(as.character(trainproj$extraction_type))

write.csv(train, "myTrain.csv", row.names=FALSE)
write.csv(test, "myTest.csv", row.names=FALSE)

allData <- sample(1:nrow(trainproj), size=0.8*nrow(trainproj))
water_train <- trainproj[allData,]
water_test <- trainproj[-allData,]

rf_model <- randomForest(water_train$status_group~., data = water_train, ntree=300)
pred <- predict(rf_model, water_test)
mean(water_test$status_group==pred)

#car_naive = naiveBayes(status_group~.,data=water_train)
#car_naive_pred = predict(car_naive,water_test,type="class")
#mean(car_naive_pred==water_test$status_group)