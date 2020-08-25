# Setting up US States vector and US House seats
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
us_states_formatted <- paste0('.', us_states)

# Loading required packages 
library(tidyverse)

# Importing .csv file from US Census website into R using tidyverse
setwd('/Users/homebase/Projects/Data/Just Common Census')
us_pop_dir <- 'nst-est2019-01.csv'
us_pop_raw <- read.csv(us_pop_dir, skip = 3)

# Cleaning raw file to get the 2019 Census population by state 
us_pop <- select(us_pop_raw, X, X2019)
us_pop <- us_pop %>% rename('State' = 'X', 'Population' = 'X2019')
us_pop <- filter(us_pop, State %in% us_states_formatted)
us_pop$Year <- 2019

# Transforming data type and cleaning up format
us_pop$State <- as.character(us_pop$State)
us_pop$State <- sub('.', '', us_pop$State)
us_pop$Population <- gsub(',', '', us_pop$Population)
us_pop$Population <- as.numeric(us_pop$Population)
us_pop <- us_pop[, c(3, 1, 2)]

# Setting up "Representatives", "PpR", "PpR_Plus1" columns
us_pop$Representatives <- 1
us_pop$PpR <- us_pop$Population / us_pop$Representatives
us_pop$PpR_Plus1 <- us_pop$Population / (us_pop$Representatives + 1)

# Setting up initial values for "app_round", "pre_loop_us_house_seats" variables for dynamic indexing in loops
app_round <- 2
pre_loop_us_house_seats <- us_house_seats - length(us_states)
us_house_seats <- pre_loop_us_house_seats

# Setting up the "sum_comparison" and "state_rep" tables
sum_comparison <- tibble(
  Round = 0:pre_loop_us_house_seats
)
state_reps <- tibble(
  Round = 0:pre_loop_us_house_seats
)

for (j in 1:nrow(us_pop)) {
  
  column_name <- us_pop$State[j]
  us_pop[, column_name] <- NA
  
  sum_comparison[1, column_name] <- 0
  sum_comparison[2:nrow(sum_comparison), column_name] <- NA
  
  state_reps[1, column_name] <- us_pop$Representatives[j]
  state_reps[2:nrow(state_reps), column_name] <- NA
  
}

# Setting up "sum_comparison_col_adj", "us_pop_col_adj" variables for dynamic column indexing in loop 
sum_comparison_col_adj = ncol(sum_comparison) - length(us_states)
us_pop_col_adj = ncol(us_pop) - length(us_states)

# The combined calculations that determine which state gets a representative
while (us_house_seats > 0) {
  for (j in 1:nrow(us_pop)) {
    for (i in 1:nrow(us_pop)) {
      
      # Calculates change in relative representation (CRR) for base state_vs 
      CRR_base_vs <- (us_pop$PpR[i] - us_pop$PpR_Plus1[j]) / us_pop$PpR_Plus1[j]
      CRR_base_vs <- abs(CRR_base_vs)
      
      # Calculates change in relative representation (CRR) for vs_base state
      CRR_vs_base <- (us_pop$PpR[j] - us_pop$PpR_Plus1[i]) / us_pop$PpR_Plus1[i]
      CRR_vs_base <- abs(CRR_vs_base)
      
      # Compares whether or not giving representative to base state causes a smaller impact to PpR difference than if given to other state (1 = True, 0 = False)
      us_pop[i, j + us_pop_col_adj] <- as.numeric(CRR_base_vs < CRR_vs_base)
      
    }
  
    # Sums up each state's "base state_comparison" values 
    sum_comparison[app_round, j + sum_comparison_col_adj] <- sum(us_pop[, j + us_pop_col_adj])
    
  }
  
  # Finds which state beats out all the other states in each round from the "sum_comparison" table, then adds a representative to that state in the "us_pop" table
  max <- as.numeric(which.max(sum_comparison[app_round, 2:ncol(sum_comparison)]))
  us_pop$Representatives[max] <- us_pop$Representatives[max] + 1
  us_house_seats <- us_house_seats - 1
  state_reps[app_round, 2:ncol(state_reps)] <- us_pop$Representatives
  
  # Refreshes "PpR" and "PpR_Plus1" columns in "us_pop" table
  us_pop$PpR <- us_pop$Population / us_pop$Representatives
  us_pop$PpR_Plus1 <- us_pop$Population / (us_pop$Representatives + 1)
  
  app_round <- app_round + 1
  print(paste(toString(us_house_seats), 'house seats left', sep = ' '))
  
}
print('Done')

# Pivots state_reps table to get Representatives by State by Round
state_reps <- state_reps %>%
  pivot_longer(-Round, names_to = 'State', values_to = 'Representatives')

# Pivots sum_comparison table to get Match-up by State by Round
sum_comparison <- sum_comparison %>%
  pivot_longer(-Round, names_to = 'State', values_to = 'Standing')

# Merges two tables together into data table for Tableau
data_table <- add_column(state_reps, Standing = sum_comparison$Standing)
data_table$Year <- 2019

# Changes column order of data_table before writing it to .csv file
data_table <- data_table[, c(5, 1, 2, 3, 4)]
write.csv(
  data_table,
  file = 'data_table_2019.csv'
)