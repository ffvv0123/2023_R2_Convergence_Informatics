#####################################################
## Lecture Material                                ##
## Convergence Informatics                         ##
## Topic: Q1 ~ Q4                                  ##           
## Suhyeon Jo                                      ##
## 2023-12-23                                      ##
#####################################################

## Set Environment
setRepositories(ind = 1:7)

WORK_DIR <- "/Users/jjos/Desktop/2023_Final"
setwd(WORK_DIR)

## Loading library
library(ggplot2)
library(data.table)
library(readxl)
library(dplyr)

data <- as.data.frame(read_excel("DataQ1.xlsx"))
data <- data[1:3]
data[, 1] = c(1:65)
View(data)

## Q1

## Professor Data Regression
prof_fit <- lm(ProfWeight_Kg~Date, data = data)
summary(prof_fit)

plot(data$ProfWeight_Kg~data$Date)
abline(summary(prof_fit)$coefficients[1,1], summary(prof_fit)$coefficients[2,1], col = "red")

## Regression with student data
stu_fit <- lm(StudentWeight_Kg~Date, data = data)
summary(stu_fit)

plot(data$StudentWeight_Kg~data$Date)
abline(summary(stu_fit)$coefficients[1,1], summary(stu_fit)$coefficients[2,1], col = "red")

## Q2

## Professor weight in 2021-11-22
new <- data.frame(Date = c(148))
data.frame(PredictedWeight = predict(prof_fit, newdata = new), new)

## Student weight in 2021-11-22
new <- data.frame(Date = c(148))
data.frame(PredictedWeight = predict(stu_fit, newdata = new), new)


## Q3

## setting data
data_professor <- cbind(data[1], data[2])
data_student <- cbind(data[1], data[3])

data_professor$Group <- "Professor"
data_student$Group <- "Student"

data_professor <- data_professor %>% rename(Weight_Kg = ProfWeight_Kg)
data_student <- data_student %>% rename(Weight_Kg = StudentWeight_Kg)

data_combined <- rbind(data_professor, data_student)

## regression model plot

model_student <- lm(Weight_Kg ~ Date, data = data_student)
model_prof <- lm(Weight_Kg ~ Date, data = data_professor)


ggplot(data_combined, aes(x = Date, y = Weight_Kg, color = Group)) +
  geom_point() +
  geom_smooth(method = "lm", data = data_student, color = "blue") +
  geom_smooth(method = "lm", data = data_professor, color = "red") +
  theme_minimal() +
  labs(title = "Weight over Time with Linear Models", x = "Date", y = "Weight (Kg)")


## Q4

## Professor weight in 2023-12-18
new <- data.frame(Date = c(904))
data.frame(PredictedWeight = predict(prof_fit, newdata = new), new)

## Student weight in 2023-12-18
new <- data.frame(Date = c(904))
data.frame(PredictedWeight = predict(stu_fit, newdata = new), new)
