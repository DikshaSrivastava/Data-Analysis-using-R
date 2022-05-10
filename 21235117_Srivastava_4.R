#Loading the required libraries
library(dplyr)
library(ggplot2)
library(aimsir17)

#Q1: Gather the summary annual information for each weather station, store in the 
#tibble annual.
annual <- observations                             %>% 
          group_by(station)                        %>%
          summarise(TotalRain= sum(rain, na.rm=T),
                    AvrWind= mean(wdsp, na.rm=T),
                    AvrTemp= mean(temp, na.rm=T))  %>%
          print(n=25)

#Q2: For each observation, add the ranking (highest to lowest), using the R function rank().
#Used mutate to add columns to the tibble.
annual <- annual                                   %>%
          mutate(Rank_Rain = rank(-TotalRain),
                 Rank_Wind = rank(-AvrWind),
                 Rank_Temp = rank(-AvrTemp))
#rowMeans calculates mean of selected columns row-wise.
annual <- annual                                   %>% 
          mutate(Avr_Rank = rowMeans(annual[5:7])) %>% 
          print(n=25)

#Q3: Find the values observations with the highest and lowest rank.
target <- annual                                   %>% 
          filter(Avr_Rank == max(Avr_Rank) | Avr_Rank == min(Avr_Rank))
target

#Q4: Extract the station names from the tibble, make use of the function pull().
target <- pull(target, station)
target

#Q5: Based on the variable target filter the observations tibble so that it contains only those records for
#the highest and lowest ranked stations.
my_obs <- observations                            %>% 
          filter(station %in% target)
my_obs

#Q6: Plot the line graph showing the distribution of temperature.
ggplot(my_obs, aes(x=temp, colour= station))          +
  ggtitle("Temperature Profile for Weather Stations") +
  geom_freqpoly(bins= 30)

#Q7: Plot the boxplot graph showing the distribution of windspeed.
ggplot(my_obs, aes(x=station, y=wdsp, colour=station))+
  ggtitle("Windspeed Profile for Weather Stations")   +
  geom_boxplot(na.rm = T)
 

#Q8: Plot the boxplot graph showing the distribution of rainfall.
#Grouping Rainfall Data by Day and station
my_obs                                          %>% 
 group_by(day, station)                         %>%
 summarise(Rain= sum(rain, na.rm=T))            %>%
  ggplot(aes(x=station, y=Rain, colour=station))  +
  ggtitle("Rainfall Profile for Weather Stations")+
  geom_boxplot(na.rm = T)


#Q9: Generate a summary tibble of monthly statistics for each station.
monthly <- my_obs                                 %>% 
           group_by(station, month)               %>%
           summarise(Rain= sum(rain, na.rm=T),
                     AvrWind= mean(wdsp, na.rm=T),
                     AvrTemp= mean(temp, na.rm=T))%>%
           print(n=24)

#Q10: Generate the line graph of rainfall for each month.
ggplot(monthly, aes(x= month, y= Rain, colour= station))  +
  ggtitle("Monthly Rainfall Profile for Weather Stations")+
  geom_line()                                             +
  geom_point()                                            +
  scale_x_continuous(breaks= 1:12)                      

#Q11: Generate the line graph of temperature for each month.
ggplot(monthly, aes(x= month, y= AvrTemp, colour= station))  +
  ggtitle("Monthly Temperature Profile for Weather Stations")+
  geom_line()                                                +
  geom_point()                                               +
  scale_x_continuous(breaks= 1:12)
  

#Q12: Generate the line graph of windspeed for each month.
ggplot(monthly, aes(x= month, y= AvrWind, colour= station))+
  ggtitle("Monthly Windspeed Profile for Weather Stations")+
  geom_line()                                              +
  geom_point()                                             +
  scale_x_continuous(breaks= 1:12)
  
