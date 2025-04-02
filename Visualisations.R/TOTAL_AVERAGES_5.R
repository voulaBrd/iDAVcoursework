library(ggplot2)
library(viridisLite)
library(plotly)
library(here)
library(readr)

Total_Avgs <- read_csv(here("My_CSVs", "Total_Averages.csv"))
                       
avg_plot <- ggplot(Total_Avgs, aes(
 x = as.factor(Month), 
 y = AverageValue, 
 color = as.factor(Year), 
 group = as.factor(Year), 
 text = paste("Month:",month.abb,"<br>", 
              round(AverageValue,2),"µg / m^3"))) +
 geom_line() +
 geom_point() +
 scale_color_viridis_d(option = "viridis") +
 labs(
   title = "Monthly Average of Air Pollutants Over 6 Years",
   x = "Month",
   y = "Average Value (µg / m^3)",
   color = "Year"
 ) +
 scale_x_discrete(labels = month.abb)+
 facet_wrap(~AirParticle, 
            labeller = as_labeller(c(
              "PM10_avg" = "PM10 Particles",
              "NI_OX_avg" = "Nitric Oxide",
              "NI_DX_avg" = "Nitrogen Dioxide",
              "NOasNI_avg" = "Nitrogen Oxides as Nitrogen Dioxide"
            ))) +
 theme_minimal(base_size = 14) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

avg_interactive <- ggplotly(avg_plot, tooltip = "text")
avg_interactive