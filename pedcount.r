library(tidyverse)
library(reshape2)
library(ggmap)
register_google(key = Sys.getenv("google_maps_key"))
pedcount <- read_csv("PedCountLocationsMay2015.csv")

##Let's start with maps

#Add a longitude and a latitud columns
pedcount2 <- pedcount %>% separate(the_geom, c("temp", "lon", "lat"), sep = " ") %>%
  select(-temp)
pedcount2 <- mutate(pedcount2, lon = as.numeric(substr(pedcount2$lon,2,nchar(pedcount2$lon))),
         lat = as.numeric(substr(pedcount2$lat,1,nchar(pedcount2$lat)-1)))

#Create a map that I'll later overlay the data on
#Set coordinates for the city
nyc <- c(lon = -73.9559, lat = 40.7128)
#Download the map
nyc_map <- get_map(location = nyc,  zoom = 11)
ggmap(nyc_map)

#Make a map with a point for each location, color by borough
ggmap(nyc_map) +
  geom_point(aes(lon, lat), data = pedcount2)

#And now one colored by bourough and showing by the size of the marks the amount of people counted on May 2015
ggmap(nyc_map) +
  geom_point(aes(lon, lat, color = Borough, size = May15_AM, alpha = 0.6), data = pedcount2)

##YAY MAPS!

##Now we keep playing

#Create a more manageable data set to play with, with only the weekday afternoon measurements
location_count_pm <- select(pedcount,
                            Borough,
                            Loc,
                            ends_with("PM"))

#Add one column with the difference between pedestrian count in 2015 and 2007, this is to see if any glaring super-growth areas pop up. They didn't.
with_total_difference <- mutate(location_count_pm,
                                 Total_Diff = May15_PM - May07_PM)
with_total_difference <- arrange(with_total_difference, desc(Total_Diff))
head(with_total_difference)

hist(with_total_difference$Total_Diff)

#I need to reshape my data to get the plot I want
df_for_plot <- melt(location_count_pm, id.vars = c("Loc", "Borough"))
df_for_plot <- transform(df_for_plot,
                         Loc = as.character(Loc))

#I'll create one dataframe for each Borough, for easier plotting
df_Bronx <- filter(df_for_plot, Borough == "Bronx")
df_Brooklyn <- filter(df_for_plot, Borough == "Brooklyn")
df_Manhattan <- filter(df_for_plot, Borough == "Manhattan")
df_Queens <- filter(df_for_plot, Borough == "Queens")
df_Staten_Island <- filter(df_for_plot, Borough == "Staten Island")

ggplot(data = df_for_plot) + geom_line(mapping = aes(x=variable, y=value, group = Loc, color = Borough))
ggplot(data = df_Bronx) + geom_line(mapping = aes(x = variable, y = value, group = Loc, color = Loc))
ggplot(data = df_Brooklyn) + geom_line(mapping = aes(x = variable, y = value, group = Loc, color = Loc))
ggplot(data = df_Manhattan) + geom_line(mapping = aes(x = variable, y = value, group = Loc, color = Loc))
ggplot(data = df_Queens) + geom_line(mapping = aes(x = variable, y = value, group = Loc, color = Loc))
ggplot(data = df_Staten_Island) + geom_line(mapping = aes(x = variable, y = value, group = Loc, color = Loc))



