# PM10

PM_10_data <- read.csv("PM_10_data.csv")
PM_10_data$Date <- as.Date(PM_10_data$Date, format = "%Y-%m-%d")
PM_10_data$Time <- parse_time(PM_10_data$Time, format = "%H:%M:%S")
#na_count <- PM_10_data %>% summarise_all((~sum(is.na(.))))


# as.factor() to treat these days as distinct group  
PM10_plot <- ggplot(PM_10_data, aes(x = as.factor(Date), y = PM10_HOUR))+
geom_point(aes(colour = factor(year(Date)), 
               text = paste(format(Date, "%Y-%m-%d"),
                      "<br>",format(Time, "%H:%M:%S")))) +
scale_color_viridis_d(option = "turbo") +
labs(
  x = "Date",
  y = "PM10 (Hourly)",
  title = "PM10 Hourly Measurements Over Time ",
  colour = "Year"
) +
theme_minimal()+
theme(
  axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(PM10_plot, tooltip = "text")


# as.factor() to treat these days as distinct group
PM10_boxplot <- ggplot(PM_10_data, aes(x = as.factor(Date), y = PM10_HOUR)) +
  geom_boxplot(aes(colour = factor(year(Date)))) +  # Check daily distribution
  scale_color_viridis_d(option = "turbo") +
  labs(
    x = "Date",
    y = "PM10",
    title = "PM10 Daily Distribution (Boxplot)",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplotly(PM10_boxplot, tooltip = "text")

#!!!! 
ggplot(PM_10_data, aes(x = PM10_HOUR, y = Date)) +
  geom_line(aes(colour = factor(year(Date)))) +  # Color lines by year
  scale_color_viridis_d(option = "turbo") + 
  labs(
    x = "PM10 (Hourly)",
    y = "Date",
    title = "PM10 Hourly Measurements by Year - Line Plot",
    colour = "Year"
  ) +
  theme_minimal()
