library(tidyverse)


data2018 <- read.csv("iDAV CW Datasets\\POAR_2018.csv", 
                     skip = 4, header = TRUE)
dt18 <- rename(data2018, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
               Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt18$Date <- dmy(dt18$Date) 
dt18$Time <- hm(dt18$Time)
#Frequency Distribution
hist(dt18$PM10_HOUR, main="Histogram 2018", xlab="Values", col="lightblue", border="black")



data2019 <- read.csv("iDAV CW Datasets\\POAR_2019.csv", 
                     skip = 4, header = TRUE)
dt19 <- rename(data2019, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
               Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt19$Date <- dmy(dt19$Date)
dt19$Time <- hm(dt19$Time)
na_count19 <- dt19 %>% summarise_all((~sum(is.na(.)))) # Count NaN values
hist(dt19$PM10_HOUR, main="Histogram 2019", xlab="Values", col="lightblue", border="black")

data2020 <- read.csv("iDAV CW Datasets\\POAR_2020.csv", 
                     skip = 4, header = TRUE)
dt20 <- rename(data2020, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt20$Date <- dmy(dt20$Date)
dt20$Time <- hm(dt20$Time)
na_count20 <- dt20 %>% summarise_all((~sum(is.na(.)))) # Count NaN values
hist(dt20$PM10_HOUR, main="Histogram 2020", xlab="Values", col="lightblue", border="black")

data2021 <- read.csv("iDAV CW Datasets\\POAR_2021.csv", 
                     skip = 4, header = TRUE)
dt21 <- rename(data2021, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt21$Date <- dmy(dt21$Date)
dt21$Time <- hm(dt21$Time)
na_count21 <- dt21 %>% summarise_all((~sum(is.na(.))))
hist(dt21$PM10_HOUR, main="Histogram 2021", xlab="Values", col="lightblue", border="black")

data2022 <- read.csv("iDAV CW Datasets\\POAR_2022.csv", 
                     skip = 4, header = TRUE)
dt22 <- rename(data2022, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt22$Date <- dmy(dt22$Date)
dt22$Time <- hm(dt22$Time)
na_count22 <- dt22 %>% summarise_all((~sum(is.na(.))))
hist(dt22$PM10_HOUR, main="Histogram 2022", xlab="Values", col="lightblue", border="black")

data2023 <- read.csv("iDAV CW Datasets\\POAR_2023.csv", 
                     skip = 4, header = TRUE)
dt23 <- rename(data2023, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt23$Date <- dmy(dt23$Date)
dt23$Time <- hm(dt23$Time)
na_count23 <- dt23 %>% summarise_all((~sum(is.na(.))))
hist(dt23$PM10_HOUR, main="Histogram 2023", xlab="Values", col="lightblue", border="black")


# Data frames for each year 

PM1018 <- dt18 %>% filter(Date == "2018-12-20") %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1019 <- dt19 %>% filter(Date == "2019-01-03") %>% select(Date, Time, PM10_HOUR, Status, Unit)
#There were 2/26 null values at 2019 that I decided to discard - won't affect the result
PM1020 <- dt20 %>% filter(Date %in% c("2020-03-19", "2020-03-26", "2020-06-29", "2020-11-10", "2020-12-20")) %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1021 <- dt21 %>% filter(Date %in% c("2021-01-03", "2021-11-29")) %>% select(Date, Time, PM10_HOUR, Status, Unit)
# 29-11-21 12 values are empty---- Decide the approach!!!!
PM1022 <- dt22 %>% filter(Date == "2022-07-25") %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1023 <- dt23 %>% filter(Date == "2023-07-24") %>% select(Date, Time, PM10_HOUR, Status, Unit)

# Imputation with Median on 2019

# Function to calculate median for the same hour, 3 days before and after
calculate_hour_median <- function(data, target_date, target_time, column_name) {
  target_hour <- hour(target_time)
  target_date <- as.Date(target_date)
  
  data %>%
    filter(
      Date >= target_date - days(3) &
        Date <= target_date + days(3) &
        hour(Time) == target_hour
    ) %>%
    summarise(median_value = median(!!sym(column_name), na.rm = TRUE)) %>%
    pull(median_value)
}

# Replace NA values with the calculated median for the PM10_HOUR column
df_filled <- dt19 %>%
  mutate(
    PM10_HOUR = if_else(
      is.na(PM10_HOUR),
      sapply(1:n(), function(i) {
        calculate_hour_median(dt19, Date[i], Time[i], "PM10_HOUR")
      }),
      PM10_HOUR
    )
  )

# View the updated dataset
head(df_filled)
summary(dt19$PM10_HOUR)  # Before filling NAs
summary(df_filled$PM10_HOUR)  # After filling NAs

library(zoo)
#Time-Aware Interpolation (Same Hour)
# Custom interpolation function for same-hour values
interpolate_hour <- function(data, column_name) {
  data %>%
    group_by(hour = hour(Time)) %>%
    mutate(
      !!sym(column_name) := na.approx(
        !!sym(column_name),
        rule = 2,
        na.rm = FALSE
      )
    ) %>%
    ungroup() %>%
    select(-hour)
}

# Apply interpolation
dt19_interpolated <- dt19 %>%
  interpolate_hour("PM10_HOUR")

summary(dt19$PM10_HOUR)  # Before filling NAs
summary(df_filled$PM10_HOUR) # Median
summary(dt19_interpolated$PM10_HOUR)
hist(dt19$PM10_HOUR, main="Histogram 2019R", xlab="Values", col="lightblue", border="black")
hist(df_filled$PM10_HOUR, main="Histogram 2019 Median", xlab="Values", col="lightblue", border="black")
hist(dt19_interpolated$PM10_HOUR, main="Histogram 2019 Interpolated", xlab="Values", col="lightblue", border="black")
#Plots
library(ggplot2)

# Create the plot
ggplot(dt19, aes(x = Date, y = PM10_HOUR)) +
  geom_point(alpha = 0.1, color = "steelblue") + 
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  labs(
    x = "Date",
    y = "PM10 (Hourly)",
    title = "PM10 Hourly Measurements Over Time",
    subtitle = "Daily values with trend line"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

df_combined <- bind_rows(PM1018,PM1019, PM1020, PM1021, PM1022, PM1023)
ggplot(df_combined, aes(x = Date, y = PM10_HOUR))+
  geom_point()+ 
  labs(
    x = "Date",
    y = "PM10 (Hourly)",
    title = "PM10 Hourly Measurements Over Time"
  ) +
  theme_minimal()

