
## PRINCIPLE COMPONENT ANALYSIS


# Extracting WAP´s into a separate table

only_wap_train <- select(training, starts_with("WAP"))

only_wap_test <- select(testing, starts_with("WAP"))


# In case you need it: Taking only WAP columns and converting data to a "long table" called wap_values

#wap_values <- stack(only_wap_train)
#table(wap_values$values)


## PRINCIPLE COMPONENT ANALYSIS

pca <- prcomp(only_wap_train)

attributes(pca)
summary(pca)



# Percentage of explained variance in Scree Plot

plot(pca, xlab = "Linear combinations of WAP´s")

# Nicer Version

fviz_eig(pca)


# Various vizualisations 

fviz_pca_ind(pca, col.ind = "cos2")
viz_pca_var(pca, col.var = "contrib")
fviz_pca_biplot(pca, col.var = "contrib")   # takes super long
biplot(pca, scale = 0)


# Calculating Eigenvalues

eigval <- get_eigenvalue(pca)

eigval
head(eigval, 10)

# With 14 combinations of Variables (WAP´s) 50 % of total variance in the data can be explained
# With 132 combinations of Variables (WAP´s) 90 % of total variance in the data can be explained
# With 194 combinations of Variables (WAP´s) 95 % of total variance in the data can be explained


# Understanding the linear combinations and their (explained) variance

pca_variance <- pca$sdev^2

pca_variance[1:10]

summary(pca_variance)


# Results for Variables

pcavar <- get_pca_var(pca)

pcavar$coord          # Coordinates
pcavar$contrib        # Contributions to the PCs
pcavar$cos2           # Quality of representation

# Results for Individuals

pcaind <- get_pca_ind(pca)

pcaind$coord          # Coordinates
pcaind$contrib        # Contributions to the PCs
pcaind$cos2           # Quality of representation


# Coordinates for Individuals give same results as predictions made according to pca, why is that??


# Moving Longitude and Latitude to the front in both data sets

training = training[,c(465,466,append(c(1:464), c(467:475)))]

testing = testing[,c(368,369,append(c(1:367), c(370:378)))]



# Removinig negative values in Longitude in both data sets

training$Longitude = training$Longitude*(-1)

testing$Longitude = testing$Longitude*(-1)


# Creating a data frame with only Longitude / Latitude and the PC´s for training data

training_pc_long <- data.frame(Longitude = training$Longitude, pca$x)

training_pc_lat <- data.frame(Latitude = training$Latitude, pca$x)


# Taking the first 14 / 132 / 194 Principle Components, respectively for Lat and Long for training data

training_pc_long <- training_pc_long[, 1:41]

training_pc_lat <- training_pc_lat[, 1:41]


# KNN MODEL

#grid = expand.grid(mtry = c(3,9,14))   # only for RF

set.seed(400)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)


# training the model on Longitude

knnFit.long <- train(Longitude ~ ., data = training_pc_long,
               method="knn", 
               preProcess=c("center","scale"),
               trControl=ctrl,
               tuneLength = 7
)


knnFit.long
ggplot(knnFit.long)


# training the model on Latitude

knnFit.lat <- train(Latitude ~ ., data = training_pc_lat,
                method="knn", 
                preProcess=c("center","scale"),
                trControl=ctrl,
                tuneLength = 7
)


knnFit.lat
ggplot(knnFit.lat)


# Checking the importance of variables

varImp(knnFit.long)
varImp(knnFit.lat)



# Transforming also the test data into PCA format and checking eigenvalues 

pca_v <- prcomp(testing[, 3:369])
pca_v


eigval_v <- get_eigenvalue(pca_v)
head(eigval_v, 100)

fviz_eig(pca_v)



# Creating data frames out of the PC´s we have just calculated

testing_pca.long <- data.frame(Longitude = testing$Longitude, pca_v$x)

testing_pca.lat <- data.frame(Latitude = testing$Latitude, pca_v$x)


# Taking the first 14 / 132 / 194 Principle Components of the data frames
# depends on how many PC´s have been selected for the model training before

testing_pca.long <- testing_pca.long[, 1:41]
testing_pca.lat <- testing_pca.lat[, 1:41]


# Predicting Longitude and Latitude for the test data, based on our trained model

knnPred.long <- predict(knnFit.long, newdata = testing_pca.long[, 2:41])

knnPred.lat <- predict(knnFit.lat, testing_pca.lat[, 2:41])


# Comparing predicted against real values

summary(knnPred.long)
summary(testing$Longitude)

summary(knnPred.lat)
summary(testing$Latitude)


# Combining the predictions for Lat and Long and substracting them from the real values in the testing data set

acc <- (testing[, 1:2] - cbind(knnPred.long, knnPred.lat))

sd(acc$Longitude)
sd(acc$Latitude)
