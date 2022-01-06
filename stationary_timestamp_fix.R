### Solution for fixing when stationary timestamps are being read in wonky
#
#gets the rows that are have problematic ARR
problem_times <- Stationary %>%
  filter(str_length(ARR) < 8) %>%
  mutate(month111 = month(DTY)) 

#gets which exact days are problematic
problem_times %>%
  distinct(DTY) 

# new times is a csv file made with only the problematic times from the April download file and december 10
new_times <- read.csv("new_times.csv", colClasses = c(rep("character",11)))
#should be about 30030 entries
new_times1 <- new_times %>%
  filter(!TAG %in% c("900_230000228791"))

no_problem_times <- Stationary %>%
  filter(str_length(ARR) >= 8)

new_Stationary <- bind_rows(new_times1, no_problem_times)

#should be 30030 entries that are different for the december and april stuff
diferences <- anti_join(new_Stationary, Stationary)

write_csv(new_Stationary, "New_Stationary.csv")


#max sure it's not being read in incorrectly again
new_Stationary <- read.csv("New_Stationary.csv", colClasses = c(rep("character",11)))
#shouldn't get any entries
x <- new_Stationary %>%
  filter(str_length(ARR) < 8)