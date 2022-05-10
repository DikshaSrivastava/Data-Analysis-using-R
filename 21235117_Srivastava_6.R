# Loading libraries
library(aimsir17)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Q1: Calculate the average daily windspeed for all stations.
observations                                              %>% 
  group_by(station, month, day)                           %>% 
  summarise(AvrWind= mean(wdsp, na.rm= T)) -> daily_wind
daily_wind

# Q2: Calculate the average daily wind power generated nationally.
eirgrid17                                                 %>% 
  group_by(month, day)                                    %>%
  summarise(AvrGen = mean(IEWindGeneration, na.rm = T)) -> daily_gen
daily_gen

# Q3: Join the two tibbles, making sure that the resulting tibble is ungrouped.
ds <- ungroup(inner_join(daily_wind, daily_gen, by=c("month","day")))
ds

# Q4: Remove any incomplete cases from the dataset.
summary(ds)
ds_clean <- ds[complete.cases(ds),]
ds_clean

# Q5: Plot the x-y values, along with a linear model for each station.
ggplot(ds_clean, aes(x = AvrWind, y = AvrGen)) +
  geom_point()                                 +
  geom_smooth(method="lm")                     +
  facet_wrap(~station)

# Q6: Create a nested tibble for each station.
ds_n <- ds_clean    %>% 
  group_by(station) %>% 
  nest()
ds_n

# Q7: Add four new columns to the dataset, via a succession of mutate calls, 
# and the use of map_* functions from purrr. These new columns will include: 
# the linear model for each station (independent variable is AvrWind, dependent 
# variable AvrGen), the intercept coefficient (beta_0), the slope coefficient (beta_1),
# and the RSquared value. Sort the tibble so that the best RSquared values are shown.
ds_n <- ds_n                                                   %>% 
  mutate(EnerMod = map(data, ~lm(AvrGen ~ AvrWind, data = .))) %>%
  mutate(Beta_0 = map_dbl(EnerMod, ~.$coefficients[1]),
         Beta_1 = map_dbl(EnerMod, ~.$coefficients[2]),
         RSquared= map_dbl(EnerMod, ~summary(.)$r.squared))    %>% 
  arrange(desc(RSquared))  
ds_n