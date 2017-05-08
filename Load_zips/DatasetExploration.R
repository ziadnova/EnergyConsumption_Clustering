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

#library
library(ggplot2)
library(reshape)
library(tidyr)
#Splitting the Time Frame for all categories and factoring
Hospital <- separate(data = Hospital, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Hospital$Time <- as.factor(Hospital$Time)
LargeHotel <- separate(data = LargeHotel, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
LargeHotel$Time <- as.factor(LargeHotel$Time)
ServiceRestaurant <- separate(data = ServiceRestaurant, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
ServiceRestaurant$Time <- as.factor(ServiceRestaurant$Time)
LargeOffice <- separate(data = LargeOffice, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
LargeOffice$Time <- as.factor(LargeOffice$Time)
Patient <- separate(data = Patient, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Patient$Time <- as.factor(Patient$Time)
School <- separate(data = School, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
School$Time <- as.factor(School$Time)
Supermarket <- separate(data = Supermarket, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Supermarket$Time <- as.factor(Supermarket$Time)
Warehouse <- separate(data = Warehouse, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Warehouse$Time <- as.factor(Warehouse$Time)


# start plotting
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1)
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + geom_smooth(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`))
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) +geom_jitter(alpha=0.1)

#Plotting all categories
