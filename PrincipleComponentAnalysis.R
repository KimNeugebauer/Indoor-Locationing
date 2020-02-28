
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
testing = testing[,c(c(3:369),append(1,2, c(370:378)))]


# Removinig negative values in Longitude in both data sets

training$Longitude = training$Longitude*(-1)

testing$Longitude = testing$Longitude*(-1)


# Creating a data frame with only Longitude / Latitude and the PC´s for training data

training_pc_long <- data.frame(Longitude = training$Longitude, pca$x)

training_pc_lat <- data.frame(Latitude = training$Latitude, pca$x)


# Taking the first 14 / 132 / 194 Principle Components, respectively for Lat and Long for training data

training_pc_long <- training_pc_long[,1:15]

training_pc_lat <- training_pc_lat[,1:133]


# KNN MODEL

#grid = expand.grid(mtry = c(3,9,14))   # only for RF


ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)

knnFit <- train(Longitude ~ ., data = training_pc_long,
               method="knn", 
               preProcess=c("center","scale"),
               trControl=ctrl,
               tuneLength =9
)

set.seed(400)

knnFit

ggplot(knnFit)
table(predict(knnFit))


# Transforming also the test data into PCA format 

pca_v <- prcomp(testing[, 1:367])

testing_pca <- predict(pca_v, newdata = testing)


# Creating a data frame out of the PC´s

testing_pca <- as.data.frame(testing_pca)

#testing_pca <- as.data.frame(Longitude = testing$Longitude, pca_v$x)



# Taking the first 14 / 132 / 194 Principle Components of the test data

testing_pca <- testing_pca[,1:15]


# Predicting on the test data

knn_pred <- predict(knnFit, testing_pca)

knn_pred
plot(knn_pred, testing$Longitude)
varImp(knnFit)


# Confusion Matrix

knn_pred <- as.factor(knn_pred)

testing_longitude <- testing$Longitude                
testing_longitude <- as.factor(testing_longitude) 

confusionMatrix(table(knn_pred, testing_longitude))  # WHYYYY ????           
table(knn_pred, testing_longitude)


