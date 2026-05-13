# ---- Load packages ----
library(tidyverse)
library(geosphere)
library(leaflet)
library(leaflet.extras)

theme_set(theme_bw() + theme(panel.grid = element_blank()))




# ---- Import data ----


triplist <- list.files(path = "data/trip_data", pattern = "\\.csv$", full.names = T)
all_trips <- map(triplist, read_csv)
all_trips <- bind_rows(all_trips)

stopifnot(anyDuplicated(all_trips$ride_id) == 0)




# ---- Create derived fields ----


all_trips$date <- as.Date(all_trips$started_at)
all_trips$weekday <- weekdays(all_trips$started_at)
all_trips$month <- month(all_trips$started_at, label = T)
all_trips$year <- year(all_trips$started_at)
all_trips$weekday <- ordered(all_trips$weekday,
                             levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


# Calculating trip time and distance
all_trips$ride_length_sec <- as.numeric(difftime(all_trips$ended_at, all_trips$started_at))

all_trips$distance_meters <- geosphere::distHaversine(
  cbind(all_trips$start_lng, all_trips$start_lat),
  cbind(all_trips$end_lng, all_trips$end_lat))




# ---- Recover station data ----


# Creating/combining start and end station lists to create a full list
full_station_list <- bind_rows(all_trips %>%
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
    )) %>% 
  filter(
    !is.na(station_id),
    !is.na(station_name),
    !is.na(lat),
    !is.na(lng)) %>%
  distinct()


# Creating a station list with coordinates that have one instance
valid_coordinates <- full_station_list %>%
  distinct(lat, lng, station_id) %>%
  count(lat, lng, name = "station_count") %>%
  filter(station_count == 1) %>%
  select(lat, lng)

full_station_list <- full_station_list %>%
  semi_join(valid_coordinates, by = c("lat", "lng")) %>%
  distinct(lat, lng, .keep_all = TRUE)



# Join start stations
all_trips2 <- left_join(all_trips, full_station_list,
                        by = c("start_lat" = "lat",
                               "start_lng" = "lng"))
all_trips2 <- all_trips2 %>%
  mutate(start_station_name = coalesce(start_station_name, station_name),
         start_station_id = coalesce(start_station_id, station_id)) %>% 
  select(-station_name, -station_id)



# Join end stations
all_trips2 <- left_join(all_trips2, full_station_list,
                        by = c("end_lat" = "lat", "end_lng" = "lng"))

all_trips2 <- all_trips2 %>% 
  mutate(end_station_name = coalesce(end_station_name, station_name),
         end_station_id = coalesce(end_station_id, station_id)) %>% 
  select(-station_name, -station_id)




# ---- Validate station recovery ----


# Compare missing station values before and after coordinate-based recovery
station_recovery_summary <- tibble(
  field = c("Start Station ID", "Start Station Name", "End Station ID", "End Station Name"),
  missing_before = c(
    sum(is.na(all_trips$start_station_id)),
    sum(is.na(all_trips$start_station_name)),
    sum(is.na(all_trips$end_station_id)),
    sum(is.na(all_trips$end_station_name))
  ),
  missing_after = c(
    sum(is.na(all_trips2$start_station_id)),
    sum(is.na(all_trips2$start_station_name)),
    sum(is.na(all_trips2$end_station_id)),
    sum(is.na(all_trips2$end_station_name))
  )
) %>%
  mutate(recovered = missing_before - missing_after)

station_recovery_summary


# Validate that station IDs and names are missing together
station_missing_alignment <- all_trips2 %>%
  summarize(
    start_id_only_missing = sum(is.na(start_station_id) & !is.na(start_station_name)),
    start_name_only_missing = sum(!is.na(start_station_id) & is.na(start_station_name)),
    end_id_only_missing = sum(is.na(end_station_id) & !is.na(end_station_name)),
    end_name_only_missing = sum(!is.na(end_station_id) & is.na(end_station_name))
  )

station_missing_alignment


# Check for station IDs tied to multiple station name variants
station_name_variants <- bind_rows(
  all_trips2 %>%
    transmute(station_id = start_station_id, station_name = start_station_name),
  all_trips2 %>%
    transmute(station_id = end_station_id, station_name = end_station_name)
) %>%
  filter(!is.na(station_id), !is.na(station_name)) %>%
  distinct(station_id, station_name) %>%
  count(station_id, name = "name_count") %>%
  filter(name_count > 1) %>%
  arrange(desc(name_count))

station_name_variants




# ---- Identify invalid trips ----


# Identifying large gap in the distribution, highlighting 4th quartile
ride_length_distribution <- summary(all_trips2$ride_length_sec)
ride_length_fourth_q <- quantile(all_trips2$ride_length_sec, probs = c(.80, .85, .90, .95, .99, .998, .999, 1))

# Plotting histogram: Ride length distribution by bike type
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

invalid_reason_summary <- invalid_trips %>%
  count(invalid_reason, sort = T)


# Filtering invalid data and docked bikes from the main data frame
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


# Creating a top stations list
popular_start_stations <- all_trips2 %>%
  filter(member_casual == "casual", !is.na(start_station_id), !is.na(start_station_name)) %>%
  count(station_id = start_station_id, station_name = start_station_name, name = "total_rides")

popular_end_stations <- all_trips2 %>%
  filter(member_casual == "casual", !is.na(end_station_id), !is.na(end_station_name)) %>%
  count(station_id = end_station_id, station_name = end_station_name, name = "total_rides")


# Group by station_id to avoid splitting stations with minor name variants.
# The first station_name is kept only as a display label.
popular_stations <- bind_rows(popular_start_stations, popular_end_stations) %>%
  group_by(station_id) %>%
  summarize(station_name = first(station_name), total_rides = sum(total_rides), .groups = "drop") %>%
  arrange(desc(total_rides)) %>%
  slice_head(n = 10)

# Create one map marker per station and flag the top casual rider stations
map_station_list <- full_station_list %>% 
  distinct(station_id, station_name, .keep_all = T) %>% 
  mutate(is_popular = station_name %in% popular_stations$station_name)




# ---- Rider behavior visualizations ----


# Plotting bar chart: Total rides by rider Type
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
  scale_fill_manual(values = c("casual" = "#F2FC67", "member" = "#4095A5"))


# Plotting density chart: Ride length by rider type
member_avg_ride <- member_summary$avg_ride_length_min[member_summary$member_casual == "member"]
casual_avg_ride <- member_summary$avg_ride_length_min[member_summary$member_casual == "casual"]

ggplot(all_trips2 %>%
         filter(ride_length_sec / 60 <= 120),
       aes(x = ride_length_sec / 60, fill = member_casual)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = member_avg_ride, linetype = "dashed", color = "#4095A5", linewidth = 1.2) +
  geom_vline(xintercept = casual_avg_ride, linetype = "dashed",color = "#D9E62C", linewidth = 1.2) +
  annotate("text", x = member_avg_ride - 1.2, y = 0.062, label = paste0("Avg for members: ", round(member_avg_ride, 2), " Mins"), angle = 90, color = "black") +
  annotate("text", x = casual_avg_ride - 1.2, y = 0.062, label = paste0("Avg for Casual Riders: ", round(casual_avg_ride, 2), " Mins"), angle = 90, color = "black") +
  labs(
    title = "Casual Riders Take Longer Trips Than Members",
    subtitle = "Ride lengths capped at 120 minutes for readability.",
    x = "Ride Length (Minutes)",
    y = "Density",
    fill = "Rider Type"
  ) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "member" = "#4095A5"))


# Plotting bar chart: Average ride distance by rider type
ggplot(member_summary, aes(x = member_casual, y = avg_distance_miles, fill = member_casual)) +
  geom_col(color = "black") +
  geom_text(aes(label = round(avg_distance_miles, 2)), vjust = -0.5, size = 4) +
  labs(title = "Average Ride Distance (Straight-Line Miles)",
       x = "Rider Type",
       y = "Straight-Line Miles",
       fill = "Rider Type") +
  scale_fill_manual(values = c("casual" = "#F2FC67", "member" = "#4095A5"))




# ---- Bike type visualization ----


# Plotting bar chart: Bike type usage by rider type
ggplot(bike_type_summary, aes(x = reorder(rideable_type, -total_rides), y = total_rides, fill = member_casual)) +
  geom_col(color = "black", position = "dodge") +
  labs(title = "Bike Type Usage Differs by Rider Type",
       subtitle = "Members used classic bikes more often, while casual riders showed stronger electric bike usage.",
       x = "Bike Type",
       y = "Total Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "member" = "#4095A5"))




# ---- Time-based visualizations ----


# Plotting bar chart: Monthly total rides by rider type
ggplot(month_summary, aes(x = month, y = total_rides, fill = member_casual)) +
  geom_col(color = "black", position = "dodge") +
  labs(title = "Bike Share Usage Peaks During Warmer Months",
       subtitle = "Activity for both groups rises sharply in warmer months, peaking in September.",
       x = "Month",
       y = "Total Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "member" = "#4095A5"))


# Plotting bar chart: Weekly total rides by rider type
ggplot(weekday_summary, aes(x = weekday, y = total_rides, fill = member_casual)) +
  geom_col(color = "black", position = "dodge") +
  labs(title = "Members Ride More Consistently Throughout the Week",
       subtitle = "Casual riders show stronger weekend usage, while members maintain steadier weekday activity.",
       x = "Weekday",
       y = "Total Rides",
       fill = "Rider Type") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("casual" = "#F2FC67", "member" = "#4095A5"))




# ---- Station visualizations ----


# Plotting bar chart: Top casual rider stations
ggplot(popular_stations, aes(x = reorder(station_name, total_rides), y = total_rides)) +
  geom_col(color = "black", fill = "#F2FC67") +
  coord_flip() +
  labs(title = "Top 10 Stations for Casual Riders (Start and End Combined)",
       x = "Station Name",
       y = "Total Rides") +
  scale_y_continuous(labels = scales::comma_format())


# Popular station map
stations_map_highlight <- leaflet(map_station_list) %>%
  addProviderTiles(provider = "Stadia.AlidadeSmoothDark") %>%
  setView(lng = -87.66, lat = 41.88, zoom = 11.7) %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    popup = ~paste("Station Name:", station_name),
    radius = ~ifelse(is_popular, 6, 1),
    color = ~ifelse(is_popular, "#F2FC67", "#577B8A"),
    fillOpacity = .9
  )

stations_map_highlight