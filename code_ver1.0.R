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


# Identifying maximum signal strength per observation

training <- trainingData

training %>% 
  mutate(max_signal=NA)


training$max_signal = training %>% 
  select(starts_with("WAP")) %>% 
  apply(1,min)



# Exchanging 100 with 0 for no signal


training[ ,1 : (length(colnames(training)) - 10)] [training[ ,1 : (length(colnames(training)) - 10)] == 100] = 0
  

training[ training["max_signal"] == 100,"max_signal"] = 0

summary(training$max_signal)



# Deleting all columns / WAP´s where variance of signal strength is 0

training = training [, - which (training %>% select(starts_with("WAP")) %>% 
                                  apply(2, var ) == 0)]


training [,which (training %>% select(starts_with("WAP")) %>% 
                    apply(2, var ) == 0)] = NULL


# Deleting all rows where variance of signal strength is 0

training = training [- which (training %>% select(starts_with("WAP")) %>% 
                                  apply(1, var ) == 0) ,]



# Taking only WAP columns and converting data to a "long table"


only_wap <- select(training, starts_with("WAP"))

wap_values <- stack(only_wap)


# Deleting all rows where signal strength is stronger than -30

wap_values <- wap_values %>% filter(!(values > -30 & values < 0))

# Alternatively: 
wap_values <- wap_values %>% filter(values < -30 | values == 0)



training2 <- training[-which(training[, 1:457] < 0)]  
                               #training[, 1:457] < 0)]

training1 <- training[training[, 1:457] < -30 | training[, 1:457] == 0]

# why does this not give me the same number of observations 
# as wap_values above? 9.075.828 versus 9.274.406 



table(training$WAP001)
table(training$max_signal)


## PRINCIPLE COMPONENT ANALYSIS

pca <- prcomp(only_wap)

attributes(pca)
summary(pca)

plot(pca, xlab = "Linear combinations of WAP´s")


# Percentage of explained variance in Scree Plot

fviz_eig(pca)

# ????
#fviz_pca_ind(pca, col.ind = "cos2")
#viz_pca_var(pca, col.var = "contrib")

fviz_pca_biplot(pca, col.var = "contrib")
biplot(pca)


# Calculating Eigenvalues

eigval <- get_eigenvalue(pca)

eigval
head(eigval, 10)


# Understanding the linear combinations and their (explained) variance

pca_variance <- pca$sdev^2

pca_variance

max(pca_variance)
summary(pca_variance)


# Results for Variables

pcavar <- get_pca_var(pca)

pcavar$coord          # Coordinates
pcavar$contrib        # Contributions to the PCs
pcavar$cos2           # Quality of representation

# Results for Individuals

pcaind <- get_pca_ind(pca)

# Coordinates give same results as predictions made according to pca, why ??
#pca_pred <- predict(pca)
#pca_pred

pcaind$coord          # Coordinates
pcaind$contrib        # Contributions to the PCs
pcaind$cos2           # Quality of representation 








