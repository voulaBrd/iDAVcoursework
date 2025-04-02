library(ggplot2)
library(viridisLite)
library(plotly)
library(here)
library(readr)

NI_OX_data <- read.csv(here("My_CSVs","NI_OX_data.csv"))
NI_OX_data$Date <- as.Date(NI_OX_data$Date, format = "%Y-%m-%d")
NI_OX_data$Time <- parse_time(NI_OX_data$Time, format = "%H:%M:%S")


nitric_oxide_plot <- ggplot(NI_OX_data, aes(
  x = Time,
  y = Nitric_Oxide,
  color = factor(year(Date)),
  text = format(Date, "%d-%m-%Y")
)) +
  geom_line() +  
  scale_color_viridis_d(option = "turbo") +
  labs(
    x = "Time",
    y = "Nitric Oxide (µg / m^3)",
    title = "Nitric Oxide Hourly Measurements / Day",
    colour = "Year"
  ) +
  theme_minimal()

ggplotly(nitric_oxide_plot, tooltip = c("text","y"))