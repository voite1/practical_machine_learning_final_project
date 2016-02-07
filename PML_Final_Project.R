library(caret)
library(rpart)
library(randomForest)

############### SET UP ENVIRONMENT AND DOWNLOAD DATA ########################

# Set appropriate working directory
# setwd('C:\\Users\\db345c\\Desktop\\Practical Machine Learning\\Final_Project')
setwd('C:\\Users\\Aleksey\\Documents\\School\\coursera\\Practical_Machine_Learning\\Final_Project')

# Check if the files exist locally, if not - download
training.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists("pml-training.csv")) {
    download.file(training.url, "pml-training.csv")
}

if(!file.exists("pml-testing.csv")){
    download.file(testing.url, "pml-testing.csv")
}

######################## LOAD DATA ###########################################

# Load training and testing data sets
training <- read.csv("pml-training.csv", na.strings = c("NA","", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings = c("NA","", "#DIV/0!"))

# cleanup unused variables
rm(training.url, testing.url)

######################## CLEANUP DATA ########################################

# identifying sparsely populated columns using nearZeroVar function
# from testing and training data sets
# Identical changes need to be made to testing and training data sets
to_skip <- nearZeroVar(training)
training <- training[, -to_skip]
testing <- testing[, -to_skip]

# cleanup unused variables
rm(to_skip)

# removing columns containing NA's only
# Identical changes need to be made to testing and training data sets
col_lst <- colSums(is.na(training)) == 0
training <- training[, col_lst]
testing <- testing[, col_lst]

# adjusting data - remove column 59 from the testing data (not needed)
testing <- testing[, -c(59)]

# creating simple dummy data with 5 levels identical to those in training data set
# iterate throug the data frame using sample function to select a sample of size 1 from
# vector called levels. Not sure how to calculate out of sample error. I will use
# my_testing set to do that, but the principle is the same. The classe variable was not
# provided as a part of the testing data set.
train_levels <- levels(training$classe)
v <- vector(mode="character", length=0)
for (i in testing[,1]) {
  x <- sample(train_levels, size=1, replace = TRUE)
  v <- c(v, x)
}

# apppending dummy data (as factor) with the same factors and number of factors as in the training data
# as well as with the same name (simply for convenience)
testing$classe <- as.factor(v)

# Remove remove index, factor, and several date columns from training and testing data set
testing <- testing[, -c(1:6)]
training <- training[, -c(1:6)]

# cleanup unused variables
rm(col_lst, v, train_levels, x)

# Explore training and testing data to assure that all fields in both training and testing
# data sets have identical classes for each of the fields.
str(training)
str(testing)

# coerse training and testing data to the same data types and fix factors in testing
for (i in 1:length(testing)) {
  for(j in 1:length(training)) {
    if( length(grep(names(training[i]), names(testing)[j])) == 1)  {
      class(testing[j]) <- class(training[i])
    }      
  }      
}

# fix coersion that failed in previous step (this is required!!!)
for (i in 1:length(testing)) {
  if (class(training[, i]) != class(testing[, i])) {
    ### try fixing coersion, so testing and training data frames match
    class(training[, i]) <- class(testing[, i])
  }
}

# Partition data into two two sets - training and testing
# Identical changes need to be made to testing and training data sets
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
my_training <- training[inTrain,]
my_testing <- training[-inTrain,]

# cleanup unused variables
rm(inTrain, i, j)

######################## BUILD MODELS #########################################

# Build decision tree model and run predictions
set.seed(212121)
treeFit <- rpart(classe ~., data=my_training, method="class")
treePredicted <- predict(treeFit, my_testing, type="class")
# print confusion matrix for treePredicted vs. my_testing$classe
confusionMatrix(treePredicted, my_testing$classe)

# Build random forest model
set.seed(212121)
rfFit <- randomForest(classe ~., data=my_training)
rfPredicted <- predict(rfFit, my_testing, type="class")
# print convusion matrix for rfPredicted vs. my_testing$classe
confusionMatrix(rfPredicted, my_testing$classe)

############### PREDICTION ON THE TEST DATA SUPPLIED BY TEACHER ################

# printing the answer for the quiz
finalAnswer <- predict(rfFit, testing, type="class")
print(finalAnswer)

############### OUT OF SAMPLE ERROR ############################################

# The classe variable was not provided as a part of the testing data set supplied
# for this assignment, but it was recommended to use classe variable to predict on.
# The principle of calculating out of sample error would be the same, but with
# testing data set (if classe variable was provied). I used my_training data set.
# I expect out of sample error to be slightly larger.
error_rate = sum((my_testing$classe != rfPredicted) / length(rfPredicted))
print(error_rate)
