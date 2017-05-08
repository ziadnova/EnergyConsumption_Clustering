rm(list = ls())
# change the path to your Load_zips path
library(readr)
setwd("Users/andrew_talas/Documents/1_NOVA/Analytics_Group/EDP_Project/Load_zips")
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

#DFs
Hospital <- separate(data = Hospital, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Hospital$Time <- as.factor(Hospital$Time)

LargeOffice = separate(data = LargeOffice, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
LargeOffice$Time = as.factor(LargeOffice$Time)

ServiceRestaurant = separate(data = ServiceRestaurant, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
ServiceRestaurant$Time = as.factor(ServiceRestaurant$Time)

LargeHotel = separate(data = LargeHotel, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
LargeHotel$Time = as.factor(LargeHotel$Time)

Patient = separate(data = Patient, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Patient$Time = as.factor(Patient$Time)

School = separate(data = School, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
School$Time = as.factor(School$Time)

Supermarket = separate(data = Supermarket, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Supermarket$Time = as.factor(Supermarket$Time)

Warehouse = separate(data = Warehouse, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
Warehouse$Time = as.factor(Warehouse$Time)

# start plotting with titles 
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Hospital")
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + geom_smooth(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`)) + ggtitle("Hospital")
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) +geom_jitter(alpha=0.1) + ggtitle("Hospital")

ggplot(data = LargeOffice, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Large Office")
ggplot(data = ServiceRestaurant, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Service Restaurant")
ggplot(data = LargeHotel, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Large Hotel")
ggplot(data = Patient, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Patient")
ggplot(data = School, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("School")
ggplot(data = Supermarket, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Supermarket")
ggplot(data = Warehouse, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Warehouse")