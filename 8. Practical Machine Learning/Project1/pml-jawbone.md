---
title: "Practical Machine Learning Project"
author: "Vikas Deshmane"
date: "8/5/2017"
output: HTML Document
---

# Predicting body movements based on accelerometer data

This project tries to predict 5 classes of behavior based on accelerometers attached to the belt, forearm, arm, and dumbell of 6 participants. Specifically, the participants were asked to perform barbell lifts in 5 different ways.

The dataset consists of 160 variables provided by the accelerometers. It has 19622 records; for each record, the correct type of behavior (from "A" to "E") is given, too.

## Design

The random forest algorithm was applied since it is well known to deliver accurate results, and its long processing time is not important for this project.

A danger of random forest is its tendency to overfit. To check that the model is not overfitted, only 70% of the training data were used for the actual training. The other 30% were used as cross validation data set to estimate the out-of-sample error.

The results were evaluated based on Accuracy.

## Data preparation

Load the training data set and the test data set.
```{r}
library(caret)
library(randomForest)
setwd('D:/Training/DataSci/')
train.total <- read.csv('pml-training.csv')
test  <- read.csv('pml-testing.csv')
```

Use 70% of the training set to actually train, 30% to create a cross validation dataset.
```{r}
set.seed(56937)
random.sample <- createDataPartition(y=train.total$classe, p=0.7, list=F)
train <- train.total[ random.sample, ]
cross.val  <- train.total[-random.sample, ]
```

## Select features: 

A short exploration of the data revealed that some variables consisted mostly of missings ("NA"). Others had invalid data (for example "#DIV/0!" or empty strings). 
Such variables were excluded, as well as variables with almost no variance, since those make for poor predictors.

Other variables that should not have any predictive values were excluded, too:

* the row number
* the user name
* time stamp data

All other variables were used for the training.

Remove columns with NAs, almost no variance or no meaningful predictive power:
```{r}
# nearZeroVar returns col-indices of columns with too little variance
col.skip <- apply(train, 2, function(c) { sum(is.na(c)) > 0 | length(nearZeroVar(c)) > 0 }) 
keep.cols <- names( train )[ which(!col.skip) ]
keep.cols <- setdiff( keep.cols, c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp'))
train <- train[,keep.cols]
cross.val <- cross.val[, keep.cols]
```

## Run random forest

Run randomForest:
```{r cache=TRUE}

# Load model if it exists already
model.file <- "rfmodel.save"
if ( !file.exists(model.file) ) {  
  fit <- randomForest( x=train[, 1:dim(train)[2]-1], y=train$classe, importance=T )
  save(fit, file=model.file)  # now save it for later ('cache')
} else {
  load(model.file)
}
fit
```

The error rate on the training set is 0.26%. 

## Estimate error on cross validation dataset

To estimate the out-of-sample error, look at the error on the cross validation dataset:
```{r}
predict.cross.val <- predict(fit, cross.val)
confusionMatrix( predict.cross.val, cross.val$classe)
```

## Conclusion

The model that was fitted using the random forest algorithm on 70% of the training dataset is able to predict the correct classes in the cross validation dataset with an accuracy of 0.997 (with 95% between 9.996 and 0.998).

So I dare to hope that it will perform good enough on the test data set.


# Annex: predict behavior on the test dataset

This section is not part of the report. Here I use the model fitted on the training dataset to predict the behaviors on the test dataset, so I can submit the predictions to Coursera. 

To be able to apply the model to the test dataset, we first need to remove the columns that weren't used to create the model. Also we need to remove the outcom variable ```classe```:
```{r}
keep.test.cols <- setdiff( keep.cols, c('classe'))
test <- test[, keep.test.cols]
predict.test <- predict(fit, test)
```

Safe predictions to files for submission.
```{r cache=TRUE}
output.dir <- 'predictions'
if (!file.exists(output.dir)) {
  dir.create(file.path( getwd(), output.dir))
}
setwd(output.dir)
pml.write.files <- function(predictions) {
  for( i in 1:length(predictions) ) {
    filename = paste0("problem_id_",i,".txt")
    write.table( predictions[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE )
  }
}
pml.write.files( predict.test )
setwd('..')
```

And these are the predictions:
```{r}
predict.test
```
