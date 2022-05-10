library(dplyr)

# Creating Closure for simple timer.

simpleTimer <- function(){
  
  # Initializing the state of closure.
  # The start time for the timer
  start_time <- 0.0
  # The finish time for the timer
  finish_time <- 0.0
  # The actual time taken
  duration <- 0.0
  # The name of the person for whom the timer works
  name <- ""
  # A tibble which stores each result
  all_times <- tibble(
    Name = character(),
    StartTime = character(),
    FinishTime = character(),
    Duration = character()
  )

  # A function which starts the timer for the given person.
  start <- function(name = "Unknown"){
    start_time <<- Sys.time()
    name <<- name
  }
  
  # An archive function which adds the timer history to the tibble. It is not
  # accessible outside the scope of finish function.
  archive <- function(){
    all_times <<- all_times                                      %>% 
      add_row(Name = name, 
              StartTime = as.character(start_time),
              FinishTime = as.character(finish_time),
              Duration = as.character(duration))
  }
  
  # A function to complete the timer.
  finish <- function(){
    # Allowing the system to sleep for 5 seconds.
    Sys.sleep(5)
    finish_time <<- Sys.time()
    duration <<- finish_time-start_time
    # Calling the archive function to store history.
    archive()
  }
  
  # A function which returns the duration of the current timing.
  get_time <- function(){
    duration
  }
  
  # A function which returns all the stored results contained in a tibble.
  get_all_times <- function(){
    all_times
  }
  
  # Creating a list of functions so that they all have access to a common state.
  list(start = start,
       finish = finish,
       get_time = get_time,
       get_all_times = get_all_times
    
  )
}

# Calling the constructor function.
t <- simpleTimer()
str(t)

# Checking the empty tibble.
environment(t$start)$all_times

# Calling the timer functions for Person1.
t$start("Person1")
t$finish()
t$get_time()
t$get_all_times()

# Calling the timer function for Person2.
t$start("Person2")
t$finish()
t$get_time()
t$get_all_times()

# While printing the environment of list of functions, we observe that the 
# environment of all four functions is same. It is same because all the functions
# have the execution environment of the parent function as their environment i.e.
# the execution environment of simpleTimer function.
print(environment(t$start))
print(environment(t$finish))
print(environment(t$get_time))
print(environment(t$get_all_times))
