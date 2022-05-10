#Q1
#Imported libraries
library(aimsir17)
library(tibble)

#Q2
#Finding atomic vectors for Dates, Mace Head, Dublin Airport, and SherkinIsland.
#Used unique to get only distinct dates from dataset.
dates <- unique(observations$date)
str(dates)

#Subset is used to filter the temperature of Mace Head station only. Since it returns list, hence, 
#unlist method is used to convert it to vector.
temp_MH <- unlist(subset(observations, station == "MACE HEAD", select = c("temp"))
                  , use.names = FALSE)
str(temp_MH)

temp_DA <- unlist(subset(observations, station == "DUBLIN AIRPORT", select = c("temp"))
                  , use.names = FALSE)
str(temp_DA)

temp_SI <- unlist(subset(observations, station == "SherkinIsland", select = c("temp"))
                  , use.names = FALSE)
str(temp_SI)

#Q3
#Plot method plots graph for Mace Head and then points method add datasets of Dublin Airport and SherkinIsland
plot(dates, temp_MH, col = "black")
points(dates, temp_DA, col = "red")
points(dates, temp_SI, col = "green")



#Q4
#Creating a tibble from the four atomic vectors.
data_set <- tibble(Date = dates, MaceHead =  temp_MH, DublinAirport = temp_DA,
                   SherkinIsland = temp_SI)
data_set

#Q5
#Creating a copy of tibble.
input_data <- subset(data_set, select = c("MaceHead", "DublinAirport", "SherkinIsland"))
input_data

#Q6
#Finding the station having minimum temperature and converting it to factor.
min <- as.factor(apply(input_data, 1, function(temp)colnames(input_data[which.min(temp)])))
data_set[, "MinTemp"] <- min
data_set

#Q7
#Finding the station having maximum temperature and converting it to factor.
max <- as.factor(apply(input_data, 1, function(temp)colnames(input_data[which.max(temp)])))
data_set[, "MaxTemp"] <- max
data_set

#Q8
#Finding the percentage when stations were minimum and maximum.
min_stations <- round(table(data_set$MinTemp)/nrow(data_set) * 100, 2)
min_stations

max_stations <- round(table(data_set$MaxTemp)/nrow(data_set) * 100, 2)
max_stations

#Getting the summary of data set
summary(data_set)
