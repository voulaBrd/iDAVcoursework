library(ggplot2)
library(viridisLite)
library(plotly)
library(here)
library(readr)


NI_DX_data <-read.csv(here("My_CSVs","NI_DX_data.csv"))
NI_DX_data$Date <- as.Date(NI_DX_data$Date, format = "%Y-%m-%d")
NI_DX_data$Time <- parse_time(NI_DX_data$Time, format = "%H:%M:%S")


NI_DX_boxplot <- ggplot(NI_DX_data, aes(x = as.factor(Date), y = Nitrogen_Oxides_as_Nitrogen_Dioxide)) +
  geom_boxplot(aes(colour = factor(year(Date)))) + 
  scale_color_viridis_d(option = "turbo") +
  labs(
    x = "Date",
    y = "Nitrogen Dioxide(µg / m^3)",
    title = "Nitrogen Dioxide Daily Distribution ",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplotly(NI_DX_boxplot, tooltip = "text")