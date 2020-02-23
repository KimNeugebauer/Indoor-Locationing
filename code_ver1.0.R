## Pre-Processing the training data

# Loading data and libraries

trainingData <- read.csv("C:/Users/kimne/Ubiqum/Module 3/Indoor_Locationing/trainingData.csv")

library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(forecast)
library(scales)
library(factoextra)
library(plotly)


# Initial exploration

training <- trainingData


dim(training)

summary(training[, 465:475])

head(training[, 465:475], 10)

count(distinct(training))


# Checking missing values

sum(is.na(training))   # zero missing values detected


training$LATITUDE <- as.numeric(training$LATITUDE)
training$LONGITUDE <- as.numeric(training$LONGITUDE)


# Exchanging 100 with -100 for no signal


training[ ,1 : (length(colnames(training)) - 9)] [training[ ,1 : (length(colnames(training)) - 9)] == 100] = -100


# Identifying maximum signal strength per observation

training %>% 
  mutate(max_signal=NA)


training$max_signal = training %>% 
  select(starts_with("WAP")) %>% 
  apply(1,max)

summary(training$max_signal)

table(training$max_signal)


# Exchanging -100 with 0 for no signal


training[ ,1 : (length(colnames(training)) - 10)] [training[ ,1 : (length(colnames(training)) - 10)] == -100] = 0

  

# Exchanging -100 with 0 for the variable showing maximum signal strength.
# Thus all rows with values of max_signal between 0 and -30 are invalid or don´t display any signal ever, so they can be deleted in a next step.

training[ training["max_signal"] == -100,"max_signal"] = 0

summary(training$max_signal)
table(training$max_signal)

count(training %>% select(max_signal) %>% filter(max_signal > -30))


# Deleting all rows where max_signal > -30

training = training[- which (training %>% select(max_signal) > -30), ]



# Deleting all rows where signal strength is too weak (max_signal < -90)
# in order to receive better estimation results

training = training[- which (training %>% select(max_signal) < -90), ]


summary(training$max_signal)
table(training$max_signal)


# Deleting all columns / WAP´s where variance of signal strength is 0 and
# Deleting all observations where variance of signal strength is 0 


training = training [, - which (training %>% select(starts_with("WAP")) %>% 
                                  apply(2, var ) == 0)]


nzv_cols <- nearZeroVar(training, uniqueCut = 0.1)

nzv_cols



sum(training %>% select(starts_with("WAP")) %>% 
  apply(1, var ) == 0)



# Checking distinct rows

count(distinct(training[,1:465]))  



# Renaming some columns

training <- training %>% 
  rename(Longitude = LONGITUDE,
         Latitude = LATITUDE,
         Building_ID = BUILDINGID,
         User_ID = USERID,
         Floor = FLOOR,
         Timestamp = TIMESTAMP,
         Phone_ID = PHONEID)



# Calculating average signal strength for every row

training$Mean <- rowMeans(training[, 1:463])


# ... and per building

training %>% 
  group_by(Building_ID) %>% 
  summarise(mean(max_signal))

# ... and per floor

training %>% 
  group_by(Floor) %>% 
  summarise(mean(max_signal))

# ... and per User

training %>% 
  group_by(User_ID) %>% 
  summarise(mean(max_signal))


# Investigating User_ID somewhat more

table(training$User_ID)

User1 <- training[training$User_ID == 1 ,]
User11 <- training[training$User_ID == 11 ,]
User14 <- training[training$User_ID == 14 ,]


Userx <- training[training$User_ID == 14 |
                    training$User_ID ==11|
                    training$User_ID ==1 ,]


table(training$Phone_ID)


