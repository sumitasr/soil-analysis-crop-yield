library(caret)
library(e1071)
filename1 <- "/Users/sumit/Documents/major_work/DatasetWithCategory.csv"
trainig_dataset <- read.csv(filename1, header=TRUE)

filename2 <- "/Users/sumit/Documents/major_work/TestingDataSet.csv"
testing_dataset <- read.csv(filename2, header=TRUE)

str(trainig_dataset)
str(testing_dataset)

#method 1 to normalize the data set.
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

trainig_dataset_normalised <- as.data.frame(lapply(trainig_dataset[,c(2:12)], normalize))

trainig_dataset_normalised

#str(trainig_dataset_normalised)

#summary(trainig_dataset_normalised)

#make the targets of the training dataset.
trainig_dataset_target = trainig_dataset[1:10,13]
trainig_dataset_target

# normalize the testing dataset.

testing_dataset_normalised <- as.data.frame(lapply(testing_dataset[,c(2:12)], normalize))
testing_dataset_normalised

set.seed(1234)

#apply knn now take k=sqrt(no of row in dataset)
require(class)
m1 <- knn(train=trainig_dataset_normalised, test = testing_dataset_normalised, cl = trainig_dataset_target, k=3)
m1

data.frame(testing_dataset[1],m1)
