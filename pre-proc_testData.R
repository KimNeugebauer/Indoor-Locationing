## Pre-Processing the test data

# Loading data and libraries

testData <- read.csv("C:/Users/kimne/Ubiqum/Module 3/Indoor_Locationing/validationData.csv")

library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(forecast)
library(scales)
library(plotly)


# Initial exploration

testing <- testData


dim(testing)

summary(testing[, 465:475])

head(testing[, 465:475], 10)

count(distinct(testing))


# Checking missing values

sum(is.na(testing))   # zero missing values detected


testing$LATITUDE <- as.numeric(testing$LATITUDE)
testing$LONGITUDE <- as.numeric(testing$LONGITUDE)


# Exchanging 100 with -100 for no signal


testing[ ,1 : (length(colnames(testing)) - 9)] [testing[ ,1 : (length(colnames(testing)) - 9)] == 100] = -100


# Identifying maximum signal strength per observation

testing %>% 
  mutate(max_signal=NA)


testing$max_signal = testing %>% 
  select(starts_with("WAP")) %>% 
  apply(1,max)

summary(testing$max_signal)

table(testing$max_signal)


# Exchanging -100 with 0 for no signal


testing[ ,1 : (length(colnames(testing)) - 10)] [testing[ ,1 : (length(colnames(testing)) - 10)] == -100] = 0



# Exchanging -100 with 0 for the variable maximum signal strength.
# Thus all rows that don´t display any signal ever can be deleted in a next step.

testing[ testing["max_signal"] == -100,"max_signal"] = 0



# Deleting all rows where max_signal > -30 

count(testing %>% select(max_signal) %>% filter(max_signal > -30))

summary(testing$max_signal)
table(testing$max_signal)

# Actually not present in the testing data but this would be the command:
#testing = testing[- which (testing %>% select(max_signal) > -30), ]



# Deleting all rows where signal strength is too weak (max_signal < -90) in order to receive better estimation results

testing = testing[- which (testing %>% select(max_signal) < -90), ]


summary(testing$max_signal)
table(testing$max_signal)



# Deleting all columns / WAP´s where variance of signal strength is 0 and

testing = testing [, - which (testing %>% select(starts_with("WAP")) %>% 
                                  apply(2, var ) == 0)]

# Detecting observations where variance of signal strength is 0 

sum(testing %>% select(starts_with("WAP")) %>% 
      apply(1, var ) == 0)


# Detecing near zero variance columns

nzv_cols <- nearZeroVar(testing, uniqueCut = 0.1)

nzv_cols    # Only unimportant variables, can be ignored



# Checking distinct rows

count(distinct(testing[,1:367]))   
count(testing) - count(distinct(testing[,1:367]))

# is okay, only 5 not distinct rows; can be kept


# Renaming some columns

testing <- testing %>% 
  rename(Longitude = LONGITUDE,
         Latitude = LATITUDE,
         Building_ID = BUILDINGID,
         User_ID = USERID,
         Floor = FLOOR,
         Timestamp = TIMESTAMP,
         Phone_ID = PHONEID)



# Calculating average signal strength for every row

testing$Mean <- rowMeans(testing[, 1:367])

