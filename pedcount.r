library(tidyverse)
library(reshape2)
library(ggmap)
pedcount <- read.csv("https://raw.githubusercontent.com/alegerosa/pedestrians-nyc/master/PedCountLocationsMay2015.csv")

#Create a more manageable data set to play with, with only the weekday afternoon measurements
location_count_pm <- select(pedcount,
                            Borough,
                            Loc,
                            ends_with("PM"))

#Add one column with the difference between pedestrian count in 2015 and 2007, this is to see if any glaring super-growth areas pop up. They didn't.
with_total_difference <- mutate(location_count_pm,
                                 Total_Diff = May15_PM - May07_PM)
with_total_difference <- arrange(with_total_difference, Total_Diff)
with_total_difference

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

#Create a map that I'll later overlay the data on
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71),  zoom = 11)
ggmap(nyc_map)
