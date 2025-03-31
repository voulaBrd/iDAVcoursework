# PM10

PM_10_data <- read.csv("PM_10_data.csv")
PM_10_data$Date <- as.Date(PM_10_data$Date, format = "%Y-%m-%d")
PM_10_data$Time <- parse_time(PM_10_data$Time, format = "%H:%M:%S")
#na_count <- PM_10_data %>% summarise_all((~sum(is.na(.))))

NI_DX_data <-read.csv("NI_DX_data.csv")
NI_DX_data$Date <- as.Date(NI_DX_data$Date, format = "%Y-%m-%d")
NI_DX_data$Time <- parse_time(NI_DX_data$Time, format = "%H:%M:%S")

# Function Category
# PM10 (0, 50, 75, 100, Inf) Nitrogen Dioxide (0, 200, 400, 600, Inf)
categorize <- function(part_values) {
  breaks <-  c (0, 16, 33, 50, 75, 100, Inf)  #c(0, 200, 400, 600, Inf)
  labels <- c("Low (0-16)", "Low (17-33)", "Low (34-50)", "Moderate (51-75)", "High (77-100)", "Very High (>100)")
  
  cut(part_values, breaks = breaks, labels = labels, right = FALSE,  # 50 µg/m³ is "Moderate" (not "Low")
    include.lowest = TRUE  # Includes 0 
  )
}

PM_10_data$PM10_HOUR <- na_interpolation(PM_10_data$PM10_HOUR, option = "linear")
PM_10_data <- PM_10_data %>% mutate(Category = categorize(PM10_HOUR)) # Create a new named column
NI_DX_data<- NI_DX_data %>% mutate(categorize(Nitrogen_Oxides_as_Nitrogen_Dioxide))

# as.factor() to treat these days as distinct group  
PM10_plot <- ggplot(PM_10_data, aes(x = as.factor(Date), y = PM10_HOUR))+
geom_point(aes(colour = factor(year(Date)), 
               text = paste(format(Date, "%Y-%m-%d"),
                      "<br>",format(Time, "%H:%M:%S")))) +
scale_color_viridis_d(option = "turbo") +
labs(
  x = "Date",
  y = "PM10",
  title = "PM10 Measurements Over Time ",
  colour = "Year"
) +
theme_minimal()+
theme(
  axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(PM10_plot, tooltip = "text")

# Category/Hour/Day

PM10_plot <- ggplot(PM_10_data, aes(x = as.factor(Date), y = as.factor(Time)))+
  geom_point(aes(colour = Category, size = 3,
                 text = paste(format(PM10_HOUR)))) +
  scale_color_viridis_d(option = "turbo") +
  labs(
    x = element_text("Date", size = 10),
    y = element_text("Hour", size = 10),
    title = element_text("PM10 Hourly Measurements Over Time ", size = 12),
    colour = element_text("PM10 Banding", size = 10)
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(PM10_plot, tooltip = "text")

#-----------------------------------------
PM10_plot <- ggplot(
  PM_10_data,
  aes(
    x = as.factor(Date),
    y = as.factor(Time),
    text = paste("PM10:", PM10_HOUR)  
  )
) +
  geom_point(
    aes(colour = Category),  
    size = 3
  ) +
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

#Boxplot

# as.factor() to treat these days as distinct group
PM10_boxplot <- ggplot(PM_10_data, aes(x = as.factor(Date), y = PM10_HOUR)) +
  geom_boxplot(aes(colour = factor(year(Date)))) +  # Check daily distribution
  scale_color_viridis_d(option = "turbo") +
  labs(
    x = "Date",
    y = "PM10",
    title = "PM10 Daily Distribution (Boxplot)",#Add units of measurement!!!!
    colour = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplotly(PM10_boxplot, tooltip = "text")

# Linegraph

p <- ggplot(PM_10_data, aes(
  x = Time,
  y = PM10_HOUR,
  color = factor(year(Date)),
  text = format(Date, "%d-%m-%Y")
)) +
  geom_line() +  # No text aesthetic here
  scale_color_viridis_d(option = "turbo") +
  labs(
    x = "Time",
    y = "PM10",
    title = "PM10 Hourly Measurements - Line Plot",
    colour = "Year"
  ) +
  theme_minimal()

ggplotly(p, tooltip = c("text","y"))
 
# 2020 Averages

avg_20_long <- monthly_averages %>% pivot_longer(
                cols = -month, names_to = "AirParticle",
                values_to = "AverageValue")


avg20 <- ggplot(avg_20_long, aes(
  x = factor(month),
  y = AverageValue)) + 
  geom_col(fill = "steelblue") +
  labs(
    title = "Monthly Averages 2020",
    x = "Month",
    y = "Average Value (µg / m^3)"
  ) +
  scale_x_discrete(labels = month.abb) +
  facet_wrap(~AirParticle, 
             labeller = as_labeller(c(
               "PM10_avg" = "PM10 Particles",
               "NI_OX_avg" = "Nitric Oxide",
               "NI_DX_avg" = "Nitrogen Dioxide",
               "NOasNI_avg" = "Nitrogen Oxides as Nitrogen Dioxide"
             ))) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplotly(avg20, tooltip = "y")
