# Loading the libraries
library(dplyr)
library(ggplot2)
library(nycflights13)
library(lubridate)

# Q1: Select the columns from the flights tibble.
my_flights <- select(flights, time_hour, origin, carrier, dep_delay)
my_flights

# Q2: Using the lubridate package, process the time_hour column to 
# add the additional columns.
# Using as.integer() on month to convert it into an integer.
my_flights <- my_flights                                         %>% 
               mutate(month = as.integer(month(time_hour)),
                      month_name = month(time_hour, label = T),
                      day = mday(time_hour),
                      hour = hour(time_hour))
my_flights

# Q3: Perform a join operation with the airlines tibble to get the tibble.
my_data <- inner_join(my_flights, airlines, by="carrier")
my_data

# Q4: Remove any incomplete records
my_data <- my_data[complete.cases(my_data),]
my_data

# Q5: Get the top six airlines in terms of flight frequency from the cleaned dataset.
top_6 <- my_data                              %>%
          group_by(name)                      %>%
          summarise(TotalFlights = n())       %>%
          arrange(desc(TotalFlights))         %>% 
          slice(1:6)
top_6

# Q6: Use a filtering join to filter these airlines from the cleaned data set.
# Using semi_join() in order to keep all observations in my_data that have a match in top_6.
final_data <- semi_join(my_data, top_6, by = "name")
final_data

# Q7: Using the quantile function, Calculate the 95% intervals for departure times for 
# each month of the year.
quant_month_95 <- final_data                                             %>%
                   group_by(month_name)                                  %>%
                   summarise(Q2.5 = quantile(dep_delay, probs = 0.025),
                             Q97.5 = quantile(dep_delay, probs = 0.975))
quant_month_95

# Q8: Calculate the 95% intervals for departure times for each carrier.
# In order to keep 95% interval, the quartiles will be 2.5% and 97.5%.
quant_carrier_95 <- final_data                                           %>%
                     group_by(name)                                      %>%
                     summarise(Q2.5 = quantile(dep_delay, probs = 0.025),
                               Q97.5 = quantile(dep_delay, probs = 0.975))
quant_carrier_95

# Q9: Calculate the 95% intervals for departure delays for each airport.
quant_airport_95 <- final_data                                           %>%
                     group_by(origin)                                    %>%
                     summarise(Q2.5 = quantile(dep_delay, probs = 0.025),
                               Q97.5 = quantile(dep_delay, probs = 0.975))
quant_airport_95

# Q10: Create your own plot from final_data that shows an insightful relationship.
# Finding the average departure delay for each month for top 6 airlines.
final_data                                                    %>%
  group_by(month, name)                                       %>%
  summarise(Average_Delay= mean(dep_delay))                   %>%
  ggplot(aes(x=month, y= Average_Delay, colour= name))+
    ggtitle("Monthly Departure Delay for Airlines")   +
    geom_point()                                      +
    geom_smooth()                                     +
    facet_wrap(~name)                                 +
    scale_x_continuous(breaks= 1:12)