# Setting up US States vector and US House seats, add state abbreviations in later version!!!
us_house_seats <- 435
us_states <- c(
  'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado',
  'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho',
  'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine',
  'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi',
  'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey',
  'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio',
  'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina',
  'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia',
  'Washington', 'West Virginia', 'Wisconsin', 'Wyoming'
)

# Loading required packages 
library(tidyverse)

# Importing .csv file from US Census website into R using tidyverse
setwd('/Users/homebase/projects-data/US House Apportionment')
us_2010_app_dir <- 'pop_change.csv'
us_2010_app_raw <- read.csv(us_2010_app_dir, skip = 2)

# Cleaning raw file to get the 2010 apportionment population by state 
us_2010_app <- select(us_2010_app_raw, STATE_OR_REGION, X2010_POPULATION)
us_2010_app <- us_2010_app %>% rename('State' = 'STATE_OR_REGION', 'Population' = 'X2010_POPULATION')
us_2010_app <- filter(us_2010_app, State %in% us_states)
us_2010_app$State <- as.character(us_2010_app$State)
us_2010_app$Population <- as.numeric(us_2010_app$Population)

# Setting up "Representatives", "Pop_per_Rep", "Pop_per_Rep_Plus_1" columns
us_2010_app$Representatives <- 1
us_house_seats <- us_house_seats - length(us_states)

us_2010_app$PpR <- us_2010_app$Population / us_2010_app$Representatives
us_2010_app$PpR_Plus1 <- us_2010_app$Population / (us_2010_app$Representatives + 1)

# Setting up initial values for "app_round", "pre_loop_us_house_seats" variables for dynamic indexing in loops
app_round <- 2
pre_loop_us_house_seats <- us_house_seats

# Setting up the "sum_comparison" and "state_rep" tables
sum_comparison <- tibble(
  Round = 0:pre_loop_us_house_seats
)

state_reps <- tibble(
  Round = 0:pre_loop_us_house_seats
)

for (j in 1:nrow(us_2010_app)) {
  
  # column_name <- paste(toString(us_2010_app$State[j]), 'comparison', sep = '_')
  column_name <- us_2010_app$State[j]
  us_2010_app[, column_name] <- NA
  
  sum_comparison[1, column_name] <- 0
  sum_comparison[2:nrow(sum_comparison), column_name] <- NA
  
  state_reps[1, column_name] <- us_2010_app$Representatives[j]
  state_reps[2:nrow(state_reps), column_name] <- NA
  
}  

# Setting up "sum_comparison_col_adj", "us_2010_app_col_adj" variables for dynamic column indexing in loop 
sum_comparison_col_adj = ncol(sum_comparison) - length(us_states)
us_2010_app_col_adj = ncol(us_2010_app) - length(us_states)

# The combined calculations that determine which state gets a representative
while (us_house_seats > 0) {
  for (j in 1:nrow(us_2010_app)) {
    for (i in 1:nrow(us_2010_app)) {
      
      # Calculates change in relative representation (CRR) for base state_vs 
      CRR_base_vs <- (us_2010_app$PpR[i] - us_2010_app$PpR_Plus1[j]) / us_2010_app$PpR_Plus1[j]
      CRR_base_vs <- abs(CRR_base_vs)
      
      # Calculates change in relative representation (CRR) for vs_base state
      CRR_vs_base <- (us_2010_app$PpR[j] - us_2010_app$PpR_Plus1[i]) / us_2010_app$PpR_Plus1[i]
      CRR_vs_base <- abs(CRR_vs_base)
      
      # Compares whether or not giving representative to base state causes a smaller impact to PpR difference than if given to other state (1 = True, 0 = False)
      us_2010_app[i, j + us_2010_app_col_adj] <- as.numeric(CRR_base_vs < CRR_vs_base)
      
    }
  
    # Sums up each states "base state_comparison" values 
    sum_comparison[app_round, j + sum_comparison_col_adj] <- sum(us_2010_app[, j + us_2010_app_col_adj])
    
  }
  
  # Finds which state beats out all the other states in each round from the "sum_comparison" table, then adds a representative to that state in the "us_2010_app" table
  max <- as.numeric(which.max(sum_comparison[app_round, 2:ncol(sum_comparison)]))
  us_2010_app$Representatives[max] <- us_2010_app$Representatives[max] + 1
  us_house_seats <- us_house_seats - 1
  state_reps[app_round, 2:ncol(state_reps)] <- us_2010_app$Representatives
  
  # Refreshes "PpR" and "PpR_Plus1" columns in "us_2010_app" table
  us_2010_app$PpR <- us_2010_app$Population / us_2010_app$Representatives
  us_2010_app$PpR_Plus1 <- us_2010_app$Population / (us_2010_app$Representatives + 1)
  
  app_round <- app_round + 1
  print(paste(toString(us_house_seats), 'house seats left', sep = ' '))
  
}

# Pivots state_reps table to get Representatives by State by Round
state_reps <- state_reps %>%
  pivot_longer(-Round, names_to = "State", values_to = "Representatives")

# Pivots sum_comparison table to get Match-up by State by Round
sum_comparison <- sum_comparison %>%
  pivot_longer(-Round, names_to = "State", values_to = "Match-up")

print('Done')
print(us_2010_app)
print(sum_comparison)
print(state_reps)

write.csv(
  sum_comparison, 
  file = 'sum_comparison.csv', 
)
write.csv(
  state_reps, 
  file = 'state_reps.csv', 
)