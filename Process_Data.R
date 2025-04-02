library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridisLite)
library(plotly)
library(imputeTS)  

# 2018
# \\ Win - / Linux
data2018 <- read.csv("iDAV CW Datasets/POAR_2018.csv", skip = 4, header = TRUE)
dt18 <- rename(data2018, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt18$Date <- dmy(dt18$Date) 
test18 <- dt18[!is.na(dt18$Date),]
test18$Time <- gsub("24:00", "23:59", test18$Time) 
test18$Time <- parse_time(test18$Time, format = "%H:%M")

# 2019
data2019 <- read.csv("iDAV CW Datasets/POAR_2019.csv", skip = 4, header = TRUE)
dt19 <- rename(data2019, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)

dt19$Date <- dmy(dt19$Date)
data19 <- dt19[!is.na(dt19$Date),]
data19$Time <- gsub("24:00", "23:59", data19$Time)
data19$Time <- parse_time(data19$Time, format = "%H:%M")

# 2020
data2020 <- read.csv("iDAV CW Datasets/POAR_2020.csv", skip = 4, header = TRUE)
dt20 <- rename(data2020, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt20$Date <- dmy(dt20$Date)
data20 <- dt20[!is.na(dt20$Date),]
data20$Time <- gsub("24:00", "23:59", data20$Time)
data20$Time <- parse_time(data20$Time, format = "%H:%M")

# 2021
data2021 <- read.csv("iDAV CW Datasets/POAR_2021.csv", skip = 4, header = TRUE)
dt21 <- rename(data2021, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt21$Date <- dmy(dt21$Date)
data21 <- dt21[!is.na(dt21$Date),]
data21$Time <- gsub("24:00", "23:59", data21$Time)
data21$Time <- parse_time(data21$Time, format = "%H:%M")

# 2022
data2022 <- read.csv("iDAV CW Datasets/POAR_2022.csv", skip = 4, header = TRUE)
dt22 <- rename(data2022, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt22$Date <- dmy(dt22$Date)
data22 <- dt22[!is.na(dt22$Date),]
data22$Time <- gsub("24:00", "23:59", data22$Time)
data22$Time <- parse_time(data22$Time, format = "%H:%M")

# 2023
data2023 <- read.csv("iDAV CW Datasets/POAR_2023.csv", skip = 4, header = TRUE)
dt23 <- rename(data2023, Time = time, 
               PM10_HOUR = PM.sub.10..sub..particulate.matter..Hourly.measured.,
               Status = status, Unit = unit, Nitric_Oxide = Nitric.oxide, 
               Status_NO = status.1, Unit_NO = unit.1, Nitrogen_Dioxide = Nitrogen.dioxide,
               Status_ND = status.2, Unit_ND = unit.2,Nitrogen_Oxides_as_Nitrogen_Dioxide = 
                 Nitrogen.oxides.as.nitrogen.dioxide, Status_NOasND = status.3, 
               Unit_NOasSAND = unit.3)
dt23$Date <- dmy(dt23$Date)
data23 <- dt23[!is.na(dt23$Date),]
data23$Time <- gsub("24:00", "23:59", data23$Time)
data23$Time <- parse_time(data23$Time, format = "%H:%M")

# Imputation with Median 

# Calculation of median for the same hour, 3 days before and after
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

# Data for first 3 visualizations.

# 2019 NA values with the calculated MEDIAN for PM10_HOUR 
df19_filled <- data19 %>%
  mutate(
    PM10_HOUR = if_else(
      is.na(PM10_HOUR),
      sapply(1:n(), function(i) {
        calculate_hour_median(data19, Date[i], Time[i], "PM10_HOUR")
      }),
      PM10_HOUR
    )
  )


# 2021 NA values with the calculated MEDIAN for PM10_HOUR 
df21_filled <- data21 %>%
  mutate(
    PM10_HOUR = if_else(
      is.na(PM10_HOUR),
      sapply(1:n(), function(i){
        calculate_hour_median(data21, Date[i], Time[i], "PM10_HOUR")
      }),
      PM10_HOUR
    )
  )
PM1018 <- test18 %>% filter(Date == "2018-12-20") %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1019 <- df19_filled %>% filter(Date == "2019-01-03") %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1020 <- data20 %>% filter(Date %in% c("2020-03-19", "2020-03-26", "2020-06-29", "2020-11-10", "2020-12-20")) %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1021 <- df21_filled %>% filter(Date %in% c("2021-01-03", "2021-11-29")) %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1022 <- data22 %>% filter(Date == "2022-07-25") %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1023 <- data23 %>% filter(Date == "2023-07-24") %>% select(Date, Time, PM10_HOUR, Status, Unit)

df_combined_PM10 <- bind_rows(PM1018,PM1019, PM1020, PM1021, PM1022, PM1023)
#PM10_data_clean <- drop_na(df_combined_PM10)
write.csv(df_combined_PM10, "PM_10_data.csv", row.names = FALSE)

# Nitric Oxide Data
niox18 <- test18 %>% filter(Date == "2018-12-20") %>% select(Date, Time, Nitric_Oxide, Status_NO, Unit_NO)
niox19 <- data19 %>% filter(Date == "2019-01-03") %>% select(Date, Time, Nitric_Oxide, Status_NO, Unit_NO)
niox20 <- data20 %>% filter(Date %in% c ("2020-03-19", "2020-03-26", "2020-06-29", "2020-11-10", "2020-12-20")) %>% select(Date, Time, Nitric_Oxide, Status_NO, Unit_NO)
niox21 <- data21 %>% filter(Date %in% c("2021-01-03", "2021-11-29")) %>% select(Date, Time, Nitric_Oxide, Status_NO, Unit_NO)
niox22 <- data22 %>% filter(Date == "2022-07-25") %>% select(Date, Time, Nitric_Oxide, Status_NO, Unit_NO)
niox23 <- data23 %>% filter(Date == "2023-07-24") %>% select(Date, Time, Nitric_Oxide, Status_NO, Unit_NO)

df_combined_niox <- bind_rows(niox18, niox19, niox20, niox21, niox22, niox23)
NIOX_data_clean <- drop_na(df_combined_niox) # 1 NA value
write.csv(NIOX_data_clean, "NI_OX_data.csv", row.names = FALSE)

# Nitrogen Oxides as Nitrogen Dioxide
nidx18 <- test18 %>% filter(Date == "2018-12-20") %>% select(Date, Time, Nitrogen_Oxides_as_Nitrogen_Dioxide, Status_NOasND, Unit_NOasSAND)
nidx19 <- data19 %>% filter(Date == "2019-01-03") %>% select(Date, Time, Nitrogen_Oxides_as_Nitrogen_Dioxide, Status_NOasND, Unit_NOasSAND)
nidx20 <- data20 %>% filter(Date %in% c ("2020-03-19", "2020-03-26", "2020-06-29", "2020-11-10", "2020-12-20")) %>% select(Date, Time, Nitrogen_Oxides_as_Nitrogen_Dioxide, Status_NOasND, Unit_NOasSAND)
nidx21 <- data21 %>% filter(Date %in% c("2021-01-03", "2021-11-29")) %>% select(Date, Time, Nitrogen_Oxides_as_Nitrogen_Dioxide, Status_NOasND, Unit_NOasSAND)
nidx22 <- data22 %>% filter(Date == "2022-07-25") %>% select(Date, Time, Nitrogen_Oxides_as_Nitrogen_Dioxide, Status_NOasND, Unit_NOasSAND)
nidx23 <- data23 %>% filter(Date == "2023-07-24") %>% select(Date, Time, Nitrogen_Oxides_as_Nitrogen_Dioxide, Status_NOasND, Unit_NOasSAND)

df_combined_nidx <- bind_rows(nidx18, nidx19, nidx20, nidx21, nidx22, nidx23)
NIDX_data_clean <- drop_na(df_combined_nidx) # 1 NA value
write.csv(NIDX_data_clean, "NI_DX_data.csv", row.names = FALSE)

# 2020 monthly averages
# Negative values -> NA -> impute
data20$PM10_HOUR <- ifelse(data20$PM10_HOUR < 0, NA, data20$PM10_HOUR)

data20cl <- data20 %>% mutate(
  month = month(Date),
  PM10_imputed = if_else(
    is.na(PM10_HOUR),
    sapply(1:n(), function(i) {
      calculate_hour_median(data20, Date[i], Time[i], "PM10_HOUR")
    }),
    PM10_HOUR
  ),
  NI_OX_imputed = na_interpolation(Nitric_Oxide, option = "linear"),
  NI_DX_imputed = na_interpolation(Nitrogen_Dioxide, option = "linear"),
  NOasNI_DX_imputed = na_interpolation(Nitrogen_Oxides_as_Nitrogen_Dioxide, option = "linear")
)

data20cl$PM10_imputed <-  na_interpolation(data20cl$PM10_imputed, option = "linear")

monthly_averages <- data20cl %>%
  group_by(month) %>%
  summarise(
    PM10_avg = mean(PM10_imputed),
    NI_OX_avg = mean(NI_OX_imputed),
    NI_DX_avg = mean(NI_DX_imputed),
    NOasNI_avg = mean(NOasNI_DX_imputed)
  )

avg_20_long <- monthly_averages %>% pivot_longer(
  cols = -month, names_to = "AirParticle",
  values_to = "AverageValue")

write.csv(avg_20_long, "2020_Avgs.csv", row.names = FALSE)  

# Last visualization

test18$PM10_HOUR <- ifelse(test18$PM10_HOUR < 0, NA, test18$PM10_HOUR)
test18$Nitric_Oxide <- ifelse(test18$Nitric_Oxide < 0, NA, test18$Nitric_Oxide)
curated18 <- test18 %>% mutate(
  Month = month(Date),
  PM10 = if_else(
    is.na(PM10_HOUR),
    sapply(1:n(), function(i) {
      calculate_hour_median(test18, Date[i], Time[i], "PM10_HOUR")
    }),
    PM10_HOUR
  ),
  NI_OX = if_else(
    is.na(Nitric_Oxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(test18, Date[i], Time[i], "Nitric_Oxide")
    }),
    Nitric_Oxide
  ),
  NI_DX = if_else(
    is.na(Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(test18, Date[i], Time[i], "Nitrogen_Dioxide")
    }),
    Nitrogen_Dioxide
  ),
  NOasNI_DX = if_else(
    is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(test18, Date[i], Time[i], "Nitrogen_Oxides_as_Nitrogen_Dioxide")
    }),
    Nitrogen_Oxides_as_Nitrogen_Dioxide
  ))



data19$Nitric_Oxide <- ifelse(data19$Nitric_Oxide < 0, NA, data19$Nitric_Oxide)
data19$Nitrogen_Dioxide <- ifelse(data19$Nitrogen_Dioxide < 0, NA, data19$Nitrogen_Dioxide)
data19$Nitrogen_Oxides_as_Nitrogen_Dioxide <- ifelse(data19$Nitrogen_Oxides_as_Nitrogen_Dioxide < 0, NA, data19$Nitrogen_Oxides_as_Nitrogen_Dioxide)

data19$PM10_HOUR <- ifelse(data19$PM10_HOUR < 0, NA, data19$PM10_HOUR)
curated19 <- data19 %>% mutate(
  Month = month(Date),
  PM10 = if_else(
    is.na(PM10_HOUR),
    sapply(1:n(), function(i) {
      calculate_hour_median(data19, Date[i], Time[i], "PM10_HOUR")
    }),
    PM10_HOUR
  ),
  NI_OX = if_else(
    is.na(Nitric_Oxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data19, Date[i], Time[i], "Nitric_Oxide")
    }),
    Nitric_Oxide
  ),
  NI_DX = if_else(
    is.na(Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data19, Date[i], Time[i], "Nitrogen_Dioxide")
    }),
    Nitrogen_Dioxide
  ),
  NOasNI_DX = if_else(
    is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data19, Date[i], Time[i], "Nitrogen_Oxides_as_Nitrogen_Dioxide")
    }),
    Nitrogen_Oxides_as_Nitrogen_Dioxide
  ))

data20$PM10_HOUR <- ifelse(data20$PM10_HOUR < 0, NA, data20$PM10_HOUR)
data20$Nitric_Oxide <- ifelse(data20$Nitric_Oxide < 0, NA, data20$Nitric_Oxide)
data20$Nitrogen_Dioxide <- ifelse(data20$Nitrogen_Dioxide < 0, NA, data20$Nitrogen_Dioxide)
data20$Nitrogen_Oxides_as_Nitrogen_Dioxide <- ifelse(data20$Nitrogen_Oxides_as_Nitrogen_Dioxide < 0, NA, data20$Nitrogen_Oxides_as_Nitrogen_Dioxide)

data20$PM10_HOUR <- ifelse(data20$PM10_HOUR < 0, NA, data20$PM10_HOUR)
curated20 <- data20 %>% mutate(
  Month = month(Date),
  PM10 = if_else(
    is.na(PM10_HOUR),
    sapply(1:n(), function(i) {
      calculate_hour_median(data20, Date[i], Time[i], "PM10_HOUR")
    }),
    PM10_HOUR
  ),
  NI_OX = if_else(
    is.na(Nitric_Oxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data20, Date[i], Time[i], "Nitric_Oxide")
    }),
    Nitric_Oxide
  ),
  NI_DX = if_else(
    is.na(Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data20, Date[i], Time[i], "Nitrogen_Dioxide")
    }),
    Nitrogen_Dioxide
  ),
  NOasNI_DX = if_else(
    is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data20, Date[i], Time[i], "Nitrogen_Oxides_as_Nitrogen_Dioxide")
    }),
    Nitrogen_Oxides_as_Nitrogen_Dioxide
  ))

data21$PM10_HOUR <- ifelse(data21$PM10_HOUR < 0, NA, data21$PM10_HOUR)
data21$Nitric_Oxide <- ifelse(data21$Nitric_Oxide < 0, NA, data21$Nitric_Oxide)
data21$Nitrogen_Dioxide <- ifelse(data21$Nitrogen_Dioxide < 0, NA, data21$Nitrogen_Dioxide)
data21$Nitrogen_Oxides_as_Nitrogen_Dioxide <- ifelse(data21$Nitrogen_Oxides_as_Nitrogen_Dioxide < 0, NA, data21$Nitrogen_Oxides_as_Nitrogen_Dioxide)

data21$PM10_HOUR <- ifelse(data21$PM10_HOUR < 0, NA, data21$PM10_HOUR)
curated21 <- data21 %>% mutate(
  Month = month(Date),
  PM10 = if_else(
    is.na(PM10_HOUR),
    sapply(1:n(), function(i) {
      calculate_hour_median(data21, Date[i], Time[i], "PM10_HOUR")
    }),
    PM10_HOUR
  ),
  NI_OX = if_else(
    is.na(Nitric_Oxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data21, Date[i], Time[i], "Nitric_Oxide")
    }),
    Nitric_Oxide
  ),
  NI_DX = if_else(
    is.na(Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data21, Date[i], Time[i], "Nitrogen_Dioxide")
    }),
    Nitrogen_Dioxide
  ),
  NOasNI_DX = if_else(
    is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data21, Date[i], Time[i], "Nitrogen_Oxides_as_Nitrogen_Dioxide")
    }),
    Nitrogen_Oxides_as_Nitrogen_Dioxide
  ))


data22$PM10_HOUR <- ifelse(data22$PM10_HOUR < 0, NA, data22$PM10_HOUR)
data22$Nitric_Oxide <- ifelse(data22$Nitric_Oxide < 0, NA, data22$Nitric_Oxide)
data22$Nitrogen_Dioxide <- ifelse(data22$Nitrogen_Dioxide < 0, NA, data22$Nitrogen_Dioxide)
data22$Nitrogen_Oxides_as_Nitrogen_Dioxide <- ifelse(data22$Nitrogen_Oxides_as_Nitrogen_Dioxide < 0, NA, data22$Nitrogen_Oxides_as_Nitrogen_Dioxide)


curated22 <- data22 %>% mutate(
  Month = month(Date),
  PM10 = if_else(
    is.na(PM10_HOUR),
    sapply(1:n(), function(i) {
      calculate_hour_median(data22, Date[i], Time[i], "PM10_HOUR")
    }),
    PM10_HOUR
  ),
  NI_OX = if_else(
    is.na(Nitric_Oxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data22, Date[i], Time[i], "Nitric_Oxide")
    }),
    Nitric_Oxide
  ),
  NI_DX = if_else(
    is.na(Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data22, Date[i], Time[i], "Nitrogen_Dioxide")
    }),
    Nitrogen_Dioxide
  ),
  NOasNI_DX = if_else(
    is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data22, Date[i], Time[i], "Nitrogen_Oxides_as_Nitrogen_Dioxide")
    }),
    Nitrogen_Oxides_as_Nitrogen_Dioxide
  ))

data23$PM10_HOUR <- ifelse(data23$PM10_HOUR < 0, NA, data23$PM10_HOUR)
data23$Nitric_Oxide <- ifelse(data23$Nitric_Oxide < 0, NA, data23$Nitric_Oxide)
data23$Nitrogen_Dioxide <- ifelse(data23$Nitrogen_Dioxide < 0, NA, data23$Nitrogen_Dioxide)
data23$Nitrogen_Oxides_as_Nitrogen_Dioxide <- ifelse(data23$Nitrogen_Oxides_as_Nitrogen_Dioxide < 0, NA, data23$Nitrogen_Oxides_as_Nitrogen_Dioxide)

data23$PM10_HOUR <- ifelse(data23$PM10_HOUR < 0, NA, data23$PM10_HOUR)
curated23 <- data23 %>% mutate(
  Month = month(Date),
  PM10 = if_else(
    is.na(PM10_HOUR),
    sapply(1:n(), function(i) {
      calculate_hour_median(data23, Date[i], Time[i], "PM10_HOUR")
    }),
    PM10_HOUR
  ),
  NI_OX = if_else(
    is.na(Nitric_Oxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data23, Date[i], Time[i], "Nitric_Oxide")
    }),
    Nitric_Oxide
  ),
  NI_DX = if_else(
    is.na(Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data23, Date[i], Time[i], "Nitrogen_Dioxide")
    }),
    Nitrogen_Dioxide
  ),
  NOasNI_DX = if_else(
    is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide),
    sapply(1:n(), function(i) {
      calculate_hour_median(data23, Date[i], Time[i], "Nitrogen_Oxides_as_Nitrogen_Dioxide")
    }),
    Nitrogen_Oxides_as_Nitrogen_Dioxide
  ))

m_a_18 <- select(curated18, c("Date", "Time", "Month", "PM10", "NI_OX", "NI_DX","NOasNI_DX")) 
monthly_averages_18 <- m_a_18 %>% 
  group_by(Month, Year = year(Date)) %>%
  summarise(
    PM10_avg = mean(PM10),
    NI_OX_avg = mean(NI_OX),
    NI_DX_avg = mean(NI_DX),
    NOasNI_avg = mean(NOasNI_DX)
  )

m_a_19 <- select(curated19, c("Date", "Time", "Month", "PM10", "NI_OX", "NI_DX","NOasNI_DX")) 
monthly_averages_19 <- m_a_19 %>% 
  group_by(Month, Year = year(Date)) %>%
  summarise(
    PM10_avg = mean(PM10),
    NI_OX_avg = mean(NI_OX),
    NI_DX_avg = mean(NI_DX),
    NOasNI_avg = mean(NOasNI_DX)
  )

curated20$PM10 <-  na_interpolation(curated20$PM10, option = "linear")
m_a_20 <- select(curated20, c("Date", "Time", "Month", "PM10", "NI_OX", "NI_DX","NOasNI_DX")) 
monthly_averages_20 <- m_a_20 %>% 
  group_by(Month, Year = year(Date)) %>%
  summarise(
    PM10_avg = mean(PM10),
    NI_OX_avg = mean(NI_OX),
    NI_DX_avg = mean(NI_DX),
    NOasNI_avg = mean(NOasNI_DX)
  )

curated21$PM10 <-  na_interpolation(curated21$PM10, option = "linear")
m_a_21 <- select(curated21, c("Date", "Time", "Month", "PM10", "NI_OX", "NI_DX","NOasNI_DX")) 
monthly_averages_21 <- m_a_21 %>% 
  group_by(Month, Year = year(Date)) %>%
  summarise(
    PM10_avg = mean(PM10),
    NI_OX_avg = mean(NI_OX),
    NI_DX_avg = mean(NI_DX),
    NOasNI_avg = mean(NOasNI_DX)
  )

curated22$PM10 <-  na_interpolation(curated22$PM10, option = "linear")
m_a_22 <- select(curated22, c("Date", "Time", "Month", "PM10", "NI_OX", "NI_DX","NOasNI_DX")) 
monthly_averages_22 <- m_a_22 %>% 
  group_by(Month, Year = year(Date)) %>%
  summarise(
    PM10_avg = mean(PM10),
    NI_OX_avg = mean(NI_OX),
    NI_DX_avg = mean(NI_DX),
    NOasNI_avg = mean(NOasNI_DX)
  )

curated23$PM10 <-  na_interpolation(curated23$PM10, option = "linear")
m_a_23 <- select(curated23, c("Date", "Time", "Month", "PM10", "NI_OX", "NI_DX","NOasNI_DX")) 
monthly_averages_23 <- m_a_23 %>% 
  group_by(Month, Year = year(Date)) %>%
  summarise(
    PM10_avg = mean(PM10),
    NI_OX_avg = mean(NI_OX),
    NI_DX_avg = mean(NI_DX),
    NOasNI_avg = mean(NOasNI_DX)
  )

monthly_averages <- bind_rows(monthly_averages_18, monthly_averages_19, monthly_averages_20,
                              monthly_averages_21, monthly_averages_22, monthly_averages_23)

monthly_averages_l <- monthly_averages %>% pivot_longer(
  cols = -c(Month, Year), names_to = "AirParticle",
  values_to = "AverageValue")
write.csv(monthly_averages_l, "Total_Averages.csv", row.names = FALSE )
