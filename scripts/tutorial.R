library(strava)
library(tidyverse)

data <- process_data("data/")

# Facets ----
p1 <- plot_facets(data)
ggsave("plots/facets01.png", p1, width = 20, height = 20, units = "cm")

# Activity map ----
p2 <- plot_map(data, lon_min = -87, lon_max = -82, lat_min = 42, lat_max = 45.5)
ggsave("plots/map01.png", p2, width = 20, height = 15, units = "cm", dpi = 600)

# Elevation profile ----
p3 <- plot_elevations(data)
ggsave("plots/elevations01.png", p3, width = 20, height = 20, units = "cm")


# Ridges ----

library(ggridges)
library(lubridate)
library(tidyverse)

# Process the data
# data <- process_data(<gpx file path>)

# Summarise data
data <- data %>%
  group_by(id) %>%
  summarise(start = min(time), end = max(time)) %>%
  mutate(start_time = as.POSIXct(strftime(start, format = "%H:%M:%S"), format = "%H:%M:%S"),
         end_time = as.POSIXct(strftime(end, format = "%H:%M:%S"), format = "%H:%M:%S"),
         duration = end_time - start_time,
         wday = wday(start, week_start = 1))

# Function for processing an activity on a minute-by-minute basis; active = 1, not active = 0
compute_day_curve <- function(df_row) {
  start <- as.numeric(data[df_row, "start_time"])
  end <- as.numeric(data[df_row, "end_time"])
  wday <- as.character(data[df_row, "wday"])
  result <- data.frame(time = seq(as.POSIXct("00:00:00", format = "%H:%M:%S"),
                                  as.POSIXct("23:59:58", format = "%H:%M:%S"), by = 60)) %>%
    mutate(time_end = lead(time, default = as.POSIXct("23:59:59", format = "%H:%M:%S")),
           active = ifelse(time > start & time_end < end, 1, 0), wday = wday)
  result
}

# Process all activities
plot_data <- 1:nrow(data) %>%
  map_df(~compute_day_curve(.x), .id = "id") %>%
  filter(!is.na(active), active > 0) %>%
  mutate(wday = as.factor(wday))

plot_data$wday <- factor(plot_data$wday, levels = rev(levels(plot_data$wday)))

# Create plot
p <- ggplot() +
  geom_density_ridges(aes(x = time, y = wday), plot_data, size = 0.5) +
  theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0), labels = c("Sun", "Sat", "Fri", "Thu", "Wed", "Tue", "Mon")) +
  scale_x_datetime(expand = c(0, 0), date_labels = "%I:%M %p") +
  theme(panel.grid = element_blank(), plot.margin = unit(rep(1, 4), "cm")) +
  xlab(NULL) + ylab(NULL)

# Save plot
ggsave("plots/ridges01.png", p, width = 24, height = 20, units = "cm")

# Calendar ----

library(ggart)
library(ggthemes)
library(ggTimeSeries)
library(lubridate)
library(strava)
library(tidyverse)
library(viridis)

data <- process_data("data/")

# Summarise data
summary <- data %>%
  mutate(time = lubridate::date(data$time),
         year = strftime(data$time, format = "%Y"),
         date_without_month = strftime(data$time, format = "%j"),
         month = strftime(data$time, format = "%m"),
         day_of_month = strftime(data$time, format = "%d"),
         year_month = strftime(data$time, format = "%Y-%m")) %>%
  group_by(time, year, date_without_month, month, day_of_month, year_month) %>%
  summarise(total_dist = sum(dist_to_prev), total_time = sum(time_diff_to_prev)) %>%
  mutate(speed = (total_dist) / (total_time /60^2)) %>%
  mutate(pace = (total_time / 60) / (total_dist)) %>%
  mutate(type = "day") %>%
  ungroup %>%
  mutate(id = as.numeric(row.names(.)))

# Generate plot data
time_min <- "2015-04-18"
time_max <- today()
max_dist <- 70

daily_data <- summary %>%
  group_by(time) %>%
  summarise(dist = sum(total_dist)) %>%
  ungroup() %>%
  mutate(time = lubridate::date(time)) %>%
  filter(complete.cases(.), time > time_min, time < time_max) %>%
  mutate(dist_scaled = ifelse(dist > max_dist, max_dist, dist))

# Create plot
p <- ggplot_calendar_heatmap(daily_data, "time", "dist_scaled",
                             dayBorderSize = 0.5, dayBorderColour = "white",
                             monthBorderSize = 0.75, monthBorderColour = "transparent",
                             monthBorderLineEnd = "round") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(name = "km", low = "#DAE580", high = "#236327", na.value = "#EFEDE0") +
  facet_wrap(~Year, ncol = 1) +
  theme_tufte() +
  theme(strip.text = element_text(), axis.ticks = element_blank(), legend.position = "bottom")

# Save plot
ggsave("plots/calendar01.png", p, width = 30, height = 30, units = "cm", dpi = 300)

# Circles ----
library(packcircles)

cir_data <- summary %>% 
  select(id, year, total_dist, speed)

packing <- circleProgressiveLayout(cir_data$total_dist)
cir_data <- cbind(cir_data, packing)

plot(cir_data$radius, cir_data$total_dist)
dat.gg <- circleLayoutVertices(packing, npoints=100)s

ggplot() +
  geom_polygon(data = dat.gg, aes(x, y, fill = ))

