#####################################################
## Lecture Material                                ##
## Convergence Informatics                         ##
## Topic: Q5                                       ##           
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

## Set data
data_db <- data.frame(fread("DataQ5_DNA_Database.txt", 
                            sep = "\t", head = T, stringsAsFactor = F))

data_query <- data.frame(fread("DataQ5_DNA_Query.txt", 
                            sep = "\t", head = T, stringsAsFactor = F))

data_db <- select(data_db, -1)
data_query <- select(data_query, -1)

## Manhattan_distance function
manhattan_distance <- function(vec1, vec2) {
  sum(abs(vec1 - vec2))
}

query_vector <- unlist(data_query)

distances <- apply(data_db, 1, function(row) manhattan_distance(row, query_vector))

## Get minimum distance, minimum distance index
min_distance <- min(distances)
min_index <- which.min(distances)

print(paste("Minimum distance:", min_distance))
print(paste("Index:", min_index))


## Q5-3

## Plot result
data_vis <- data.frame(Sample = 1:length(distances), Distance = distances)
data_vis$Class <- ifelse(data_vis$Distance == min_distance, "Crime", "Suspects")

ggplot(data_vis, aes(x = Sample, y = Distance, color = Class)) +
  geom_point() +
  scale_color_manual(values = c("Crime" = "red", "Suspects" = "black")) +
  theme_minimal() +
  labs(title = "Manhattan Distance from Query", x = "Sample", y = "Distance") +
  geom_text(aes(label = ifelse(Distance == min_distance, as.character(Sample), "")), vjust = -1)
