

## PRINCIPLE COMPONENT ANALYSIS


# Taking only WAP columns and converting data to a "long table" called wap_values


only_wap <- select(training, starts_with("WAP"))


wap_values <- stack(only_wap)

table(wap_values$values)


training1 <- training[training[, 1:457] < -30 | training[, 1:457] == 0]

# why does this not give me the same number of observations 
# as wap_values above? 9.075.828 versus 9.274.406 



## PRINCIPLE COMPONENT ANALYSIS

pca <- prcomp(only_wap)

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

# With 132 combinations of Variables (WAP´s) 90 % of total variance in the data can be explained
# With 194 combinations of Variables (WAP´s) 95 % of total variance in the data can be explained


# Understanding the linear combinations and their (explained) variance

pca_variance <- pca$sdev^2

pca_variance[1:10]

max(pca_variance)
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

testing = testing[,c(465,466,append(c(1:464), c(467:475)))]


# Removinig negative values in Longitude in both data sets

training$Longitude = training$Longitude*(-1)

testing$Longitude = testing$Longitude*(-1)


# Creating a data frame with only Longitude eg Latitude and the PC´s for training data

training_pc_long <- data.frame(Longitude = training$Longitude, pca$x)

training_pc_lat <- data.frame(Latitude = training$Latitude, pca$x)


# Taking the first 20 / 132 / 194 Principle Components, respectively for Lat and Long for training data

training_pc_long <- training_pc_long[,1:50]

training_pc_lat <- training_pc_lat[,1:195]


# RANDOM FOREST MODEL
# KNN MODEL

grid = expand.grid(mtry = c(12,14))

ctrl <- trainControl(method = "oob", 
                     search = "grid",
                     classProbs = TRUE
)

knnFit <- train(Longitude~., data=training_pc_long, 
               method="knn", 
               preProc=c("center","scale")
               #tuneGrid = grid,
               #trControl=ctrl,
)



# Transforming also the test data into PCA format and then creating a data frame out of the PC´s

testing_pca <- predict(pca, newdata = testing)
testing_pca <- as.data.frame(testing_pca)


# Taking the first 20 / 132 / 194 Principle Components of the test data

testing_pca <- testing_pca[,1:21]


# Predicting on the test data

knn_pred <- predict(knnFit, testing_pca)
pca_pred <- predict(Longitude~., data = testing, pca)
pca_pred

