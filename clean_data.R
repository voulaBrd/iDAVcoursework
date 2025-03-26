library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridisLite)
library(RColorBrewer)
library(plotly)


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
test18 <- dt18[!is.na(dt18$Date),]
test18$Time <- gsub("24:00", "23:59", test18$Time)
test18$Time <- parse_time(test18$Time, format = "%H:%M")

#Frequency Distribution
#hist(dt18$PM10_HOUR, main="Histogram 2018", xlab="Values", col="lightblue", border="black")



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
data19 <- dt19[!is.na(dt19$Date),]
data19$Time <- gsub("24:00", "23:59", data19$Time)
data19$Time <- parse_time(data19$Time, format = "%H:%M")
#na_count19 <- dt19 %>% summarise_all((~sum(is.na(.)))) # Count NaN values
#hist(dt19$PM10_HOUR, main="Histogram 2019", xlab="Values", col="lightblue", border="black")

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
data20 <- dt20[!is.na(dt20$Date),]
data20$Time <- gsub("24:00", "23:59", data20$Time)
data20$Time <- parse_time(data20$Time, format = "%H:%M")
#na_count20 <- dt20 %>% summarise_all((~sum(is.na(.)))) # Count NaN values
#hist(dt20$PM10_HOUR, main="Histogram 2020", xlab="Values", col="lightblue", border="black")

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
data21 <- dt21[!is.na(dt21$Date),]
data21$Time <- gsub("24:00", "23:59", data21$Time)
data21$Time <- parse_time(data21$Time, format = "%H:%M")
#na_count21 <- dt21 %>% summarise_all((~sum(is.na(.))))
#hist(dt21$PM10_HOUR, main="Histogram 2021", xlab="Values", col="lightblue", border="black")

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
data22 <- dt22[!is.na(dt22$Date),]
data22$Time <- gsub("24:00", "23:59", data22$Time)

#na_count22 <- dt22 %>% summarise_all((~sum(is.na(.))))
#hist(dt22$PM10_HOUR, main="Histogram 2022", xlab="Values", col="lightblue", border="black")

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
data23 <- dt23[!is.na(dt23$Date),]
data23$Time <- gsub("24:00", "23:59", data23$Time)
data23$Time <- parse_time(data23$Time, format = "%H:%M")
#na_count23 <- dt23 %>% summarise_all((~sum(is.na(.))))
#hist(dt23$PM10_HOUR, main="Histogram 2023", xlab="Values", col="lightblue", border="black")

#----------------------------------------

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

#-------------------------------------------
# Data frames for each year 

PM1018 <- test18 %>% filter(Date == "2018-12-20") %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1019 <- df19_filled %>% filter(Date == "2019-01-03") %>% select(Date, Time, PM10_HOUR, Status, Unit)
#There were 2/26 null values at 2019 that I decided to use the median of the specific hours 3 days before and after.
PM1020 <- data20 %>% filter(Date %in% c("2020-03-19", "2020-03-26", "2020-06-29", "2020-11-10", "2020-12-20")) %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1021 <- df21_filled %>% filter(Date %in% c("2021-01-03", "2021-11-29")) %>% select(Date, Time, PM10_HOUR, Status, Unit)
# 29-11-21 12 values are empty---- I decided to use the median of the specific hours 3 days before and after.
PM1022 <- data22 %>% filter(Date == "2022-07-25") %>% select(Date, Time, PM10_HOUR, Status, Unit)
PM1023 <- data23 %>% filter(Date == "2023-07-24") %>% select(Date, Time, PM10_HOUR, Status, Unit)


# PM10 at given dates DATASET
#There are 3 random NA values at 2018 & 2020 that I will discard as it won't affect the result. 

df_combined_PM10 <- bind_rows(PM1018,PM1019, PM1020, PM1021, PM1022, PM1023)
PM10_data_clean <- drop_na(df_combined_PM10)
write.csv(PM10_data_clean, "PM_10_data.csv", row.names = FALSE)


# View the updated dataset

#summary(dt19$PM10_HOUR)  # Before filling NAs
#summary(df_filled$PM10_HOUR) # Median
#summary(dt19_interpolated$PM10_HOUR)
#hist(dt19$PM10_HOUR, main="Histogram 2019R", xlab="Values", col="lightblue", border="black")
#hist(df_filled$PM10_HOUR, main="Histogram 2019 Median", xlab="Values", col="lightblue", border="black")
#hist(dt19_interpolated$PM10_HOUR, main="Histogram 2019 Interpolated", xlab="Values", col="lightblue", border="black")











