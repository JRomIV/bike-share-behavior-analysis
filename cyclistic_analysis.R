# Load required packages/ set graph theme
library(tidyverse)
library(geosphere)
library(leaflet)
library(leaflet.extras)
theme_set(theme_bw())

# Import the data
triplist <- list.files(path = "data/trip_data", pattern = "*.csv", full.names = TRUE)
all_trips <- map(triplist, read_csv)

# Evaluate the data structure before merging the list
glimpse(all_trips)

# Combining data into a single data frame.
all_trips <- bind_rows(all_trips)


# Verify ride_id is a unique primary key
anyDuplicated(all_trips$ride_id)

# Re-evaluate structure
head(all_trips)
summary(all_trips)
str(all_trips)


###########################  Data Wrangling  ###########################


# Extrapolation of dates
all_trips$date <- as.Date(all_trips$started_at)
all_trips$weekday <- weekdays(all_trips$started_at)
all_trips$month <- month(all_trips$started_at, label = T)
all_trips$year <- year(all_trips$started_at)

# Convert weekday into a factor, ensuring days are analyzed in a logical sequence rather than alphabetically.
all_trips$weekday <- ordered(all_trips$weekday,
                             levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# Verify the class of our weekday variable
class(all_trips$weekday)

# Calculate ride length
all_trips$ride_length_sec <- difftime(all_trips$ended_at, all_trips$started_at)

# convert ride_length_sec to numeric format
all_trips$ride_length_sec <- as.numeric(all_trips$ride_length_sec)

# rename member to subscriber
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,"member" = "subscriber"))

# Calculate straight-line geographic distance between start and end coordinates.
# Not road-network distance and does not reflect actual route traveled.
all_trips <- all_trips %>%
  mutate(
    distance_meters = geosphere::distHaversine(
      cbind(start_lng, start_lat),
      cbind(end_lng, end_lat)
    )
  )


# Identify the number of NA values in the dataset
colSums(is.na(all_trips))


# Creating & combining start and end station lists to create a complete list
full_station_list <- bind_rows(
  all_trips %>%
    select(
      station_id = start_station_id,
      station_name = start_station_name,
      lat = start_lat,
      lng = start_lng
    ),
  all_trips %>%
    select(
      station_id = end_station_id,
      station_name = end_station_name,
      lat = end_lat,
      lng = end_lng
    )
) %>%
  filter(
    !is.na(station_id),
    !is.na(station_name),
    !is.na(lat),
    !is.na(lng)
  ) %>%
  distinct()


# identifying coordinates that have one instance
# Avoids filling missing station values when the same coordinates are linked to multiple stations.
valid_coordinates <- full_station_list %>%
  distinct(lat, lng, station_id) %>%
  count(lat, lng, name = "station_count") %>%
  filter(station_count == 1) %>%
  select(lat, lng)


full_station_list <- full_station_list %>%
  semi_join(valid_coordinates, by = c("lat", "lng")) %>%
  distinct(lat, lng, .keep_all = TRUE)



# Join with the main dataset to recover missing start station names and IDs
all_trips2 <- left_join(all_trips, full_station_list,
                        by = c("start_lat" = "lat",
                               "start_lng" = "lng"))


# Coalesce joined start station names
all_trips2 <- all_trips2 %>%
  mutate(start_station_name = coalesce(start_station_name, station_name),
         start_station_id = coalesce(start_station_id, station_id)) %>% 
  select(-station_name, -station_id)


# Join with the main dataset to recover missing end station names and IDs
all_trips2 <- left_join(all_trips2, full_station_list,
                        by = c("end_lat" = "lat", "end_lng" = "lng"))

all_trips2 <- all_trips2 %>% 
  mutate(end_station_name = coalesce(end_station_name, station_name),
         end_station_id = coalesce(end_station_id, station_id)) %>% 
  select(-station_name, -station_id)


# Comparing recovered station names to the original dataframe
# Process was limited due to clustering of station coordinates
print(colSums(is.na(all_trips)))
print(colSums(is.na(all_trips2)))


########################### Identifying Extreme Outliers ##############################

# Distribution of ride length
summary(all_trips2$ride_length_sec)

# In-depth view considering there is such a large gap in the 4th quartile
# Calculate percentiles for ride length in groups of 0.05
fourth_quar_ride_length <- quantile(all_trips2$ride_length_sec, probs = c(.80, .85, .90, .95, .99, .998, .999, 1))
fourth_quar_ride_length

# Most trips fall within a few hours or less, with the exception of docked bikes
ggplot(all_trips2 %>%
    filter(
      !is.na(ride_length_sec),
      is.finite(ride_length_sec),
      ride_length_sec > 0,
      ride_length_sec / 3600 <= 600
      ),
  aes(x = ride_length_sec / 3600, fill = rideable_type)) +
  geom_histogram(binwidth = 10, color = "black") +
  facet_wrap(~rideable_type) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(
    title = "Extreme Ride Durations Were Concentrated in Docked Bike Records",
    subtitle = "Ride duration shown in hours; y-axis uses log scale to show rare extreme values.",
    x = "Ride Length (Hours)",
    y = "Ride Count (Log10 Scale)",
    fill = "Bike Type"
  ) +
  scale_fill_brewer(palette = "Set1")


# Filter and examine erroneous data
invalid_trips <- all_trips2 %>%
  transmute(
    ride_id,
    invalid_reason = case_when(
      ride_length_sec <= 0 ~ "Negative ride duration",
      is.na(distance_meters) ~ "Missing end coordinates / distance",
      distance_meters == 0 & ride_length_sec <= 60 ~ "Zero-distance ride under 60 seconds",
      start_station_name == "Pawel Bialowas - Test- PBSC charging station" ~ "Test station record",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(invalid_reason))

invalid_trips %>%
  count(invalid_reason, sort = TRUE)

# After review, remove erroneous rides and docked bikes
# Docked bike records were excluded from rider behavior analysis 
# because they contained issues with data capture expressed as trip-ending issues as previously discovered
all_trips2 <- all_trips2 %>%
  anti_join(invalid_trips, by = "ride_id") %>%
  filter(
    !is.na(ride_id),
    !is.na(member_casual),
    !is.na(rideable_type),
    !is.na(ride_length_sec),
    !is.na(distance_meters),
    rideable_type != "docked_bike"
  )


########################### Data Analysis ##############################

# ---- Building summary tables ----


general_summary <- all_trips2 %>%
  summarize(
    total_rides = n(),
    median_ride_length_min = median(ride_length_sec / 60, na.rm = TRUE),
    avg_ride_length_min = mean(ride_length_sec / 60, na.rm = TRUE),
    avg_distance_miles = mean(distance_meters * 0.00062137, na.rm = TRUE)
  )


member_summary <- all_trips2 %>%
  group_by(member_casual) %>%
  summarize(
    total_rides = n(),
    avg_ride_length_min = mean(ride_length_sec / 60, na.rm = TRUE),
    avg_distance_miles = mean(distance_meters * 0.00062137, na.rm = TRUE),
    .groups = "drop"
  )


weekday_summary <- all_trips2 %>%
  group_by(member_casual, weekday) %>%
  summarize(total_rides = n(), .groups = "drop")


month_summary <- all_trips2 %>%
  group_by(member_casual, month) %>%
  summarize(total_rides = n(), .groups = "drop")


bike_type_summary <- all_trips2 %>%
  group_by(member_casual, rideable_type) %>%
  summarize(total_rides = n(), .groups = "drop")



# Ride Count bar chart (Casual/Subscriber)
ggplot(member_summary, aes(x = member_casual, y = total_rides, fill = member_casual)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales::comma(total_rides)), vjust = -0.5, size = 4) +
  labs(
    title = "Total Rides by Rider Type",
    x = "Rider Type",
    y = "Total Rides",
    fill = "Rider Type"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "subscriber" = "#4095A5"))


# Density plot of ride length by member
subscriber_avg_ride <- member_summary$avg_ride_length_min[member_summary$member_casual == "subscriber"]
casual_avg_ride <- member_summary$avg_ride_length_min[member_summary$member_casual == "casual"]

ggplot(all_trips2 %>%
         filter(ride_length_sec / 60 <= 120),
       aes(x = ride_length_sec / 60, fill = member_casual)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = subscriber_avg_ride, linetype = "dashed", color = "#4095A5", linewidth = 1.2) +
  geom_vline(xintercept = casual_avg_ride, linetype = "dashed",color = "#D9E62C", linewidth = 1.2) +
  annotate("text", x = subscriber_avg_ride - 1.2, y = 0.062, label = paste0("Avg for Subscribers: ", round(subscriber_avg_ride, 2), " Mins"), angle = 90, color = "black") +
  annotate("text", x = casual_avg_ride - 1.2, y = 0.062, label = paste0("Avg for Casual Riders: ", round(casual_avg_ride, 2), " Mins"), angle = 90, color = "black") +
  labs(
    title = "Casual Riders Take Longer Trips Than Subscribers",
    subtitle = "Ride lengths capped at 120 minutes for readability.",
    x = "Ride Length (Minutes)",
    y = "Density",
    fill = "Rider Type"
  ) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "subscriber" = "#4095A5"))


# Avg Ride Distance
ggplot(member_summary, aes(x = member_casual, y = avg_distance_miles, fill = member_casual)) +
  geom_col(color = "black") +
  geom_text(aes(label = round(avg_distance_miles, 2)), vjust = -0.5, size = 4) +
  labs(title = "Average Ride Distance (Straight-Line Miles)",
       x = "Rider Type",
       y = "Euclidean Miles") +
  scale_fill_manual(values = c("casual" = "#F2FC67", "subscriber" = "#4095A5"))


# Bar chart for weekly ride count
ggplot(weekday_summary, aes(x = weekday, y = total_rides, fill = member_casual)) +
  geom_col(color = "black", position = "dodge") +
  labs(title = "Subscribers Ride More Consistently Throughout the Week",
       subtitle = "Casual riders show stronger weekend usage, while subscribers maintain steadier weekday activity.",
       x = "Weekday",
       y = "Total Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "subscriber" = "#4095A5"))


# Bar chart for monthly ride count
ggplot(month_summary, aes(x = month, y = total_rides, fill = member_casual)) +
  geom_col(color = "black", position = "dodge") +
  labs(title = "Bike Share Usage Peaks During Warmer Months",
       subtitle = "Activity for both groups rises sharply in warmer months, peaking in September",
       x = "Month",
       y = "Total Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "subscriber" = "#4095A5"))



# Creating a top stations list
# Combine start and end station counts into a single summary
popular_start_stations <- all_trips2 %>%
  filter(member_casual == "casual", !is.na(start_station_name)) %>%
  count(station_name = start_station_name, name = "total_rides")

popular_end_stations <- all_trips2 %>%
  filter(member_casual == "casual", !is.na(end_station_name)) %>%
  count(station_name = end_station_name, name = "total_rides")

popular_stations <- bind_rows(popular_start_stations, popular_end_stations) %>%
  group_by(station_name) %>%
  summarize(total_rides = sum(total_rides), .groups = "drop") %>%
  arrange(desc(total_rides)) %>%
  slice_head(n = 10)

# Popular stations bar chart
ggplot(popular_stations, aes(x = reorder(station_name, total_rides), y = total_rides)) +
  geom_col(color = "black", fill = "#F2FC67") +
  coord_flip() +
  labs(title = "Top 10 Stations for Casual Riders (Start and End Combined)",
       x = "Station Name",
       y = "Total Rides") +
  scale_y_continuous(labels = scales::comma_format())


# 3b. popular station map
# Create a station list to have one set of coordinates per station
map_station_list <- full_station_list %>% 
  distinct(station_id, station_name, .keep_all = T) %>% 
  mutate(is_popular = station_name %in% popular_stations$station_name)


# Create the combined map (popular stations in Lime)
leaflet(map_station_list) %>%
  addProviderTiles(provider = "Stadia.AlidadeSmoothDark") %>%
  setView(lng = -87.70, lat = 41.85, zoom = 11) %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    popup = ~paste("Station Name:", station_name),
    radius = ~ifelse(is_popular, 14, 2),
    color = ~ifelse(is_popular, "#F2FC67", "#577B8A"),
    fillOpacity = 0.9
  )


# 4. Bike type
# Create a grouped bar chart by bike type
ggplot(bike_type_summary, aes(x = reorder(rideable_type, -total_rides), y = total_rides, fill = member_casual)) +
  geom_col(color = "black", position = "dodge") +
  labs(title = "Bike Type Usage Differs by Rider Type",
       subtitle = "Subscribers used classic bikes more often, while casual riders showed stronger electric bike usage.",
       x = "Bike Type",
       y = "Total Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "subscriber" = "#4095A5"))