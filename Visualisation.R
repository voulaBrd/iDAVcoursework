# PM10

PM_10_data <- read.csv("PM_10_data.csv")
PM_10_data$Date <- as.Date(PM_10_data$Date, format = "%Y-%m-%d")
PM_10_data$Time <- parse_time(PM_10_data$Time, format = "%H:%M:%S")
#na_count <- PM_10_data %>% summarise_all((~sum(is.na(.))))

PM10_plot <- ggplot(PM_10_data, aes(x = Date, y = PM10_HOUR))+
  geom_point(aes(colour = factor(year(Date)), 
                 text = paste(format(Date, "%Y-%m-%d"),"<br>",format(Time, "%H:%M:%S")))) +
  scale_color_viridis_d(option = "turbo") +
  #scale_color_brewer(palette = "Set3") + 
  scale_x_date(
    date_labels = "%Y-%m-%d",  
    date_breaks = "1 day"      
  ) +
  labs(
    x = "Date",
    y = "PM10 (Hourly)",
    title = "PM10 Hourly Measurements Over Time - Turbo Viridis",
    colour = "Year"
  ) +
  theme_minimal()
ggplotly(PM10_plot, tooltip = "text")
