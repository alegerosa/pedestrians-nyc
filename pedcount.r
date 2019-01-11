library(tidyverse)
library(reshape2)
library(ggmap)
library(lubridate)
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

#Playing with it before I learned about tidy data
#Create a more manageable data set to play with, with only the weekday afternoon measurements
location_count_pm <- select(pedcount2,
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

#This is... cool? But, we had to arbitrarily chose one of the measurements (weekday afternoons) to create a 'more manageable' dataset, because the original dataset is very untidy. So let's make the whole dataset tidy instead.

#We start we we'd left off, at pedcount2. I'll keep making iterative new versions of the data set as we go, to better show the process

head(pedcount2)
#Many different values are stored as column names
#Let's start with the columns that store dates as values. These are named with the first letters of the month (May or Sept), an underscore and either D or D2. D is used for weekday measurements and D2 for weekend.

pedcount3 <- pedcount2 %>%
  gather(key = "date_coding", value = "date", contains("_D")) %>%
  select(lon, lat, Loc, Borough, Street, From_, To, Index, date_coding, date, everything())

#I am confused, but I am going to hold that thought and gather the columns with pedestrian counts, to see where it takes me

pedcount4 <- pedcount3 %>%
  gather(key = "measurement_coding", value = "ped_count", 11:61)


#Now I have a ton of duplicates, because I really need only one combination of Location, date and pedcount, but how do I know which one?
#If I look at the table, I can identify some that I know are useless/redundant extra rows from seeing contradictions in the date information that was coded in the names and the actual date for that row. So, to get rid of these I need to be able to identify matches and filter by them.
#For example, I should only keep rows where the the first six characters of date_coding match the first six characters of measurement_coding

pedcount5 <- pedcount4 %>%
  filter(str_sub(date_coding, 1, 6) == str_sub(measurement_coding, 1, 6))

#YAY, we got rid of most of them. However, each value is still in two different rows only one of which has properly matching date and measurement coding. We shouldn't have two 'May07_AM' rows for the same location with he same value; the reason we do is that the dates are different, and one is a weekday measurement and the other a weekend measurement. How do we know which is the one to keep? well, Weekdays have one AM and one PM measurement, and weekends have only one measurement, coded MD. So, in this case, we need to keep the row where date_coding is _D. In general, we need to keep all rows where _D in date_coding meets _AM OR _PM in measurement_coding and all rows where _D2 in date_coding matches _MD in measurement coding

pedcount6 <- pedcount5 %>%
  filter((str_sub(date_coding, -1, -1) == "2" & str_sub(measurement_coding, -1, -1) == "D") | (str_sub(date_coding, -1, -1) == "D" & str_sub(measurement_coding, -1, -1) == "M"))

#Done! Now we just need to extract from "date_coding" and "measurement_coding" the information that is relevant and not redundant
pedcount7 <- pedcount6 %>%
  mutate(
   type_of_measurement = case_when(
     str_sub(measurement_coding, -1, -1) == "D" ~ "Weekend Noon",
     str_sub(measurement_coding, -2, -1) == "AM" ~ "Weekday Morning",
     str_sub(measurement_coding, -2, -1) == "PM" ~ "Weekday Evening"),
   season = case_when(
     str_sub(date_coding, 1, 3) == "May" ~ "Spring",
     str_sub(date_coding, 1, 3) == "Sep" ~ "Fall"))
summary(pedcount7)
sum(is.na(pedcount7$season))

table(pedcount7$date)
##to fix the dates, first change dates to first 10 characters of date, then use lubridate to change data type.
pedcount7$date <- str_sub(pedcount7$date, 1, 10)
pedcount7$date <- mdy(pedcount7$date)
table(pedcount7$date)

#Now drop the redundant 'coding' columns and we are all set
pedcount8 <- select(pedcount7, -c('measurement_coding', 'date_coding'))

#Some Exploratory graphics
#Differences between types of measurements
pedcount8 %>% ggplot(aes(x = type_of_measurement, y = ped_count)) +
  geom_boxplot()
#Differences between seasons
pedcount8 %>% ggplot(aes(x = season, y = ped_count)) +
  geom_boxplot()
##Note theis is one outlier that seems extreme enoug to be a mistake
#Differences between boroughs
pedcount8 %>% ggplot(aes(x = Borough, y = ped_count)) +
  geom_boxplot()

#Scatterplot by date
pedcount8 %>% ggplot(aes(x = date, y = ped_count)) +
  geom_point(aes(alpha = 0.6))

#Interesting, let's see the distributions, per year
pedcount8 %>% ggplot(aes(x = as.factor(year(date)), y = ped_count)) +
  geom_boxplot()
#Now let's do the same but without the bigger numbers
pedcount8 %>% filter(ped_count < 12500) %>%
  ggplot(aes(x = as.factor(year(date)), y = ped_count)) +
  geom_boxplot()

#No evidence that there're  more people in new york (as a whole).

#I'll also be doing with lat ton for fun and curiosity
pedcount8 %>% ggplot(aes(x = lat, y = ped_count, color = Borough)) +
  geom_point(alpha = 0.4)
pedcount8 %>% ggplot(aes(x = lon, y = ped_count, color = Borough)) +
  geom_point(alpha = 0.4)

#Annd now a scatterplot over the map
ggmap(nyc_map) +
  geom_point(aes(lon, lat, color = Borough, size = ped_count), alpha = 0.4, data = pedcount8)
