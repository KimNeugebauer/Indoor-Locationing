
## Visualisations

# All Buildings, distinct Users

User14 %>% 
  group_by(Building_ID, Floor) %>% 
  ggplot(aes( x = Longitude, y = Latitude, color = Floor)) +
  geom_point()


# Building 1

training %>% 
  filter(Building_ID == 1, Floor == 0) %>% 
  group_by(Building_ID, Floor) %>% 
  ggplot(aes( x = Longitude, y = Latitude, color = User_ID)) +
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
