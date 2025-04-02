library(ggplot2)
library(viridisLite)
library(plotly)
library(here)
library(readr)

PM_10_data <- read_csv(here("My_CSVs", "PM_10_data.csv"))
#PM_10_data$Date <- as.Date(PM_10_data$Date, format = "%Y-%m-%d")
#PM_10_data$Time <- parse_time(PM_10_data$Time, format = "%H:%M:%S")

# PM10 categorize()

categorize <- function(part_values) {
  breaks <-  c (0, 16, 33, 50, 75, 100, Inf) 
  labels <- c("Low (0-16)", "Low (17-33)", "Low (34-50)", "Moderate (51-75)", "High (77-100)", "Very High (>100)")
  
  cut(part_values, breaks = breaks, labels = labels, right = FALSE,  # 50 µg/m³ is "Moderate" (not "Low")
      include.lowest = TRUE  # Includes 0 
  )
}

PM_10_data$PM10_HOUR <- na_interpolation(PM_10_data$PM10_HOUR, option = "linear")
PM_10_data <- PM_10_data %>% mutate(Category = categorize(PM10_HOUR))

PM10_plot <- ggplot(PM_10_data, aes(
  x = as.factor(Date),
  y = as.factor(Time),
  text = paste("PM10:", PM10_HOUR,"µg / m^3"))) +
  geom_point(aes(colour = Category),  size = 3) +
  scale_color_viridis_d(option = "viridis") + 
  labs(
    x = "Date",
    y = "Hour",
    title = "PM10 Hourly Measurements Over Time"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplotly(PM10_plot, tooltip = "text")

