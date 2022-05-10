#pre-processing
#install.packages("repurrrsive")
library(repurrrsive)
str(sw_people[1])
my_list <- sw_people

#Q1
#This function creates a data set of name and height only from my_list
get_sw_people_heights <- function(my_list){
  data_set <- vector(mode = "list")
  for(index in seq_along(my_list)){
    name <- my_list[[index]]$name
    height <- my_list[[index]]$height
    #creating a list of name and height.
    #replacing unknown values to NA in height and converting the type of height to num using as.numeric().
    data_list <- list(name = name, height = as.numeric(replace(height, height == "unknown", NA)))
    #appending the list of name and height as a list of list in data set.
    data_set <- append(data_set, list(data_list))
  }  
  data_set
}
data_set <- get_sw_people_heights(my_list)
str(data_set[1:2])

#Q2
# This function removes data set which contains NA value in height
clean_heights <- function(data_set){
  logical_vector <- vector(mode = "logical")
  for(index in seq_along(data_set)){
    #creating a logical vector which stores FALSE if height is NA
    logical_vector <- append(logical_vector, !is.na(data_set[[index]]$height))
  }
  #accessing those elements whose value in logical
  clean_data_set <- data_set[logical_vector]
  clean_data_set
}
clean_data_set <- clean_heights(data_set)
length(data_set)
length(clean_data_set)

#Q3
# This function filters the clean data set which the given range of height
get_height_range <- function(clean_data_set, min_height, max_height){
  logical_vector <- vector(mode = "logical")
  for(index in seq_along(clean_data_set)){
    #creating a logical vector which identifies whether the height is in range or not
    logical_vector <- append(logical_vector, clean_data_set[[index]]$height >= min_height & clean_data_set[[index]]$height <= max_height)
  }
  range <- clean_data_set[logical_vector]
  range
}
range <- get_height_range(clean_data_set, 174, 175)
range

#Q4
#This function returns the output in a tidy manner
tidy_output <- function(range){
  result <- vector()
  for(index in seq_along(range)){
    result <- c(result, paste0("Name = <", range[[index]]$name,"> Height = ", range[[index]]$height))
  }
  result
}
result <- tidy_output(range)
result


