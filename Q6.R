#####################################################
## Lecture Material                                ##
## Convergence Informatics                         ##
## Topic: Q6                                       ##           
## Suhyeon Jo                                      ##
## 2023-12-23                                      ##
#####################################################

## Set environment
setRepositories(ind = 1:7)
WORK_DIR <- "/Users/jjos/Desktop/2023_Final"
setwd(WORK_DIR)

## set library
library(pheatmap)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(cluster)
library(NbClust)
library(tidyverse)
library(datarium) # for data
library(caret) # for cross-validation algorithms
library(rpart)
library(rpart.plot)
library(kknn)
library(tidyverse)
library(datarium) # for data
library(ggplot2)
library(caret) # for cross-validation algorithms
library(dplyr)
library(rpart)
library(rpart.plot)
library(pROC)

## Set data
data <- fread("DataQ6.txt", sep = "\t", head = T, stringsAsFactor = T)

data <- t(data)
data <- data[-1, ]

data <- data.frame(data)
data_feature <- data[1:9]
data_class <- data[10]

data_feature <- data.frame(lapply(data_feature, function(x) as.numeric(as.character(x))))

data <- cbind(data_feature, data_class)

## Q6-1
data_result <- prcomp(data_feature, scale = TRUE)
data_class_vector <- data_class[[1]] 

data_plot <- fviz_pca_biplot(data_result, repel = TRUE,
                             geom.ind = c("point"),
                             geom.var = c(""),
                             col.ind = data_class_vector,
                             title = "PCA plot for Q6",
                             pointshape = 20,
)
print(data_plot)


## Q6-2, 3

## Setting validation function
customSummary <- function(data, lev = NULL, model = NULL) {
  if (length(lev) == 2) {
    sens <- sensitivity(data[, "pred"], data[, "obs"], lev[1])
    acc <- sum(data$pred == data$obs) / nrow(data)
    out <- c(Accuracy = acc, Sensitivity = sens)
  } else {
    out <- defaultSummary(data, lev, model)
  }
  out
}


## Set train_control
train_control <- trainControl(method = "cv", number = 10,
                              summaryFunction = customSummary,
                              classProbs = TRUE, 
                              savePredictions = "final")

## Set Supervised Learning Model
models <- list()
model_names <- c("lda", "rpart", "rf", "svmLinear", "nnet", "knn", "gbm", "glm")


for (model_name in model_names) {
  models[[model_name]] <- train(X10 ~ ., data = data, method = model_name,
                                trControl = train_control)
}


for (model_name in model_names) {
  print(model_name)
  print(models[[model_name]]$results)
}


## Find best accuracy and sensitivity
best_accuracy <- 0
best_sensitivity <- 0
best_accuracy_model <- ""
best_sensitivity_model <- ""


for (model_name in model_names) {
  model_results <- models[[model_name]]$results
  max_accuracy <- max(model_results$Accuracy, na.rm = TRUE)
  max_sensitivity <- max(model_results$Sensitivity, na.rm = TRUE)
  
  if (max_accuracy > best_accuracy) {
    best_accuracy <- max_accuracy
    best_accuracy_model <- model_name
  }
  
  if (max_sensitivity > best_sensitivity) {
    best_sensitivity <- max_sensitivity
    best_sensitivity_model <- model_name
  }
}

## Print best result
print(paste("Best Accuracy Model:", best_accuracy_model, "with Accuracy:", best_accuracy))
print(paste("Best Sensitivity Model:", best_sensitivity_model, "with Sensitivity:", best_sensitivity))
