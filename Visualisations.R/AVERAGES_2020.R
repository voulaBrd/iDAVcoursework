library(ggplot2)
library(viridisLite)
library(plotly)
library(here)
library(readr)

Avg_2020 <- read_csv(here("My_CSVs","2020_Avgs.csv"))

avg20 <- ggplot(Avg_2020, aes(
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