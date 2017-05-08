rm(list = ls())
# change the path to your Load_zips path
library(readr)
setwd("/Users/Rene/Desktop/NovaAnalyticsGroup/NAG_PROJECT_02/R_Exploration/Load_zips/")
list.files("Load_zips")
LargeHotel = read_csv("RefBldgLargeHotel.zip")
ServiceRestaurant = read_csv("RefBldgFullServiceRestaurant.zip")
Hospital = read_csv("RefBldgHospital.zip")
LargeOffice = read_csv("RefBldgLargeOffice.zip")
Patient = read_csv("RefBldgOutPatient.zip")   #Clarify meaning
School = read_csv("RefBldgSecondarySchool.zip")
Supermarket = read_csv("RefBldgSupermarket.zip")
Warehouse = read_csv("RefBldgWarehouse.zip")

library(ggplot2)
library(reshape)
library(tidyr)
Hospital <- separate(data = Hospital, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Hospital$Time <- as.factor(Hospital$Time)

# start plotting
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1)
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + geom_smooth(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`))
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) +geom_jitter(alpha=0.1)
