rm(list = ls())
# change the path to your Load_zips path
library(readr)
library(ggplot2)
library(reshape)
library(tidyr)

####################################################################
######################### FUNCTIONS ################################
####################################################################

# join_column_names
joint_colnames <- function (df_list) {
  cnames <- colnames(df_list[[2]])
  for (index in 3: length(df_list)) {
    cnames <- cnames[sapply (cnames, function(x) {
      x <- any(x == colnames(df_list[[index]]))
    })]
  }
  return (cnames)
}

#join datasets
join_data <- function (df_list) {
  cnames <- joint_colnames(df_list)
  joint_dataset <- df_list[[2]][cnames]
  
  for (i in 3:length(df_list)) {
    joint_dataset <- rbind(joint_dataset, df_list[[i]][cnames])
  }
  return (joint_dataset)
}

# split date and time and date/time variable into two variables
split_date_time <- function (df) {
  df <- separate(data = df, col = `Date/Time`, into = c("Date", "Time"), sep = "  ")
  df$Time <- as.factor(df$Time)
  return (df)
}

# 
split_all <- function (df_list) {
  return(sapply(df_list, function(x) split_date_time(x)))
}

setwd("C:\\Users\\jonat\\OneDrive\\Documents\\GitHub\\EnergyConsumption_Clustering\\Load_zips")
# list.files("Load_zips")
data <- list()
data$all <- data.frame()
data$LargeHotel = read_csv("RefBldgLargeHotel.zip")
data$ServiceRestaurant = read_csv("RefBldgFullServiceRestaurant.zip")
data$Hospital = read_csv("RefBldgHospital.zip")
data$LargeOffice = read_csv("RefBldgLargeOffice.zip")
data$Patient = read_csv("RefBldgOutPatient.zip")   #Clarify meaning
data$School = read_csv("RefBldgSecondarySchool.zip")
data$Supermarket = read_csv("RefBldgSupermarket.zip")
data$Warehouse = read_csv("RefBldgWarehouse.zip")

data$all <- join_data(data)
data <- split_all (data)
data

# start plotting
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1)
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + geom_smooth(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`))
ggplot(data = Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) +geom_jitter(alpha=0.1)


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

