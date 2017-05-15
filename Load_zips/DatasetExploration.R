rm(list = ls())
# change the path to your Load_zips path
library(readr)
library(ggplot2)
library(reshape)
library(tidyr)
library(fpc)

setwd("C:\\Users\\jonat\\OneDrive\\Documents\\GitHub\\EnergyConsumption_Clustering\\Load_zips")


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

detectWeekdays <- function(data) {
  ###
  # label days according to week days
  # assumption: weekend consumption significantly lower than weekdays --> low low high high high high high low low : low -> weekend
  dates <- as.data.frame(unique(data$all[,"Date"]))
  day_avg <- list()
  for (k in 1:length(names(data))) {
    data_p <- data[[k]]
    temp_matrix <- matrix(c(0),ncol=(ncol(data_p)-2), nrow=dim(dates)[1])
    rownames(temp_matrix) <- dates[,1]
    for (i in 1:dim(dates)[1]) {
      date <- as.data.frame(dates)[i,]
      # TODO: replace 1:8 and 3:10 with dynamic numbers - could be source of error if data changes
      temp_matrix[date,1:ncol(temp_matrix)] <- sapply(data_p[data_p[,"Date"]==date,3:ncol(data_p)], mean)
    }
    temp_matrix <- scale(temp_matrix)
    day_avg[[names(data)[k]]] <- temp_matrix
    j <- j+1
  }
  weekday_pattern <- apply((day_avg$all), MARGIN = 1, sum)
  days <- list()
  loadings <- c()
  for (i in 1:7) {
    days[[paste("day", i, sep="")]]$indices <- seq(from=i, to=365, by=7)
    days[[paste("day", i, sep="")]]$norm_sum_consump <- sum(weekday_pattern[seq(from=i, to=365, by=7)])
    loadings <- c(loadings, sum(weekday_pattern[seq(from=i, to=365, by=7)]))
  }
  weekdays <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  for (i in which(loadings == min(loadings)):(which(loadings == min(loadings)) + 6)) {
    days[[i]]$weekday <- weekdays[i]
  }
  
  for (day in 1:dim(dates)[1]) {
    dates[day, "Weekday"] <- days[[(day%%7)+1]]$weekday
  }
  for (row in 1:dim(data$all)[1]) {
    data$all[row,"Weekday"] <- dates[dates[,"Date"]==as.data.frame(data$all)[row,"Date"],"Weekday"]
  }
  return (data)
}

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
data <- detectWeekdays(data)

# TODO: make graph method generic
# start plotting
ggplot(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1)
ggplot(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + geom_smooth(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`))
ggplot(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) +geom_jitter(alpha=0.1)


# start plotting with titles 
ggplot(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Hospital")
ggplot(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + geom_smooth(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`)) + ggtitle("Hospital")
ggplot(data = data$Hospital, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) +geom_jitter(alpha=0.1) + ggtitle("Hospital")

ggplot(data = data$LargeOffice, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Large Office")
ggplot(data = data$ServiceRestaurant, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Service Restaurant")
ggplot(data = data$LargeHotel, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Large Hotel")
ggplot(data = data$Patient, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Patient")
ggplot(data = data$School, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("School")
ggplot(data = data$Supermarket, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Supermarket")
ggplot(data = data$Warehouse, aes(x = Time, y = `Electricity:Facility [kW](Hourly)`, group = Date)) + geom_line(alpha= 0.1) + ggtitle("Warehouse")
############################################################################
#                               CLUSTERING                                 #
############################################################################
# Helpful resource: http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clustersAdded clustering methods: kMeans method, number cluster determination and tried out - plotting possible

# not more than 65536 observations are allowed for this package (should use less - 65000 rows returned error that vector needs 15,7 Gb space)
# if deployed in live version, number of clusters should be determined with representative subset of the whole data
## as done here: not a random sample
  data_exc <- data$all[1:5000,3:10]
  pamk.best <- pamk(data_exc)
  cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
  # don't know why this is not working
  plot(pam(data_exc, pamk.best$nc))

# random clustering with three centers on numerical data from data frame
# (date and time would have to be converted to numbers to be included in clustering)
  kmeans(data$all[, 3:10], centers = 3, iter.max = 100)

# draw elbow graph to determine number of clusters
  mydata <- data$all[, 3:10]
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                       centers=i, iter.max = 1000)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
