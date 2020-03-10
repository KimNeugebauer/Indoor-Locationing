
## Visualisations

library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

# All Buildings, distinct Users

User14 %>% 
  group_by(Building_ID, Floor) %>% 
  ggplot(aes( x = Longitude, y = Latitude, color = Floor)) +
  geom_point()


# Building 1

training %>% 
  filter(Building_ID == 2) %>% 
  group_by(Building_ID, Floor) %>% 
  ggplot(aes( x = Longitude, y = Latitude, color = max_signal)) +
  geom_point()

# Building 2

training %>% 
  filter(Building_ID == 2, Floor == 2) %>% 
  group_by(Building_ID, Floor) %>% 
  ggplot(aes( x = Longitude, y = Latitude, color = User_ID)) +
  geom_point()


# Relative Position

training %>% 
  group_by(Building_ID, Floor) %>% 
  ggplot(aes( x = Longitude, y = Latitude, color = RELATIVEPOSITION)) +
  geom_point()


# 3D Plots

plot_ly(training, x = ~Latitude, y = ~Longitude, z = ~Floor) %>%
  add_markers(color = ~User_ID)

plot_ly(Userx, x = ~Latitude, y = ~Longitude, z = ~Floor) %>%
  add_markers(color = ~User_ID)

training %>% filter(Phone_ID == 14) %>% 
  plot_ly(x = ~Latitude, y = ~Longitude, z = ~Floor) %>%
  add_markers()



# Creating a predictions data set

pred <- as.data.frame(cbind(knnPred.long, 
                            knnPred.lat, 
                            testing[, 370]))



pred <- pred %>% 
  rename(Longitude = knnPred.long,
         Latitude = knnPred.lat,
         Floor = V3)

pred$id <- 1
testing$id <- 0


# ... and combining it with the actual data

plot.data <- rbind(testing[, c(1,2,370,379)], pred)


# .. to show predicted and actual data in one plot

plot_ly(plot.data, x = ~Latitude, y = ~Longitude, z = ~Floor) %>%
  add_markers(color = ~id)


