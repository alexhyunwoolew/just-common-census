# Setting up working directory and data table variables
setwd('/Users/homebase/Projects/Data/Just Common Census')
data_table_2010 <- read.csv('data_table_2010.csv', row.names = 1)
data_table_2019 <- read.csv('data_table_2019.csv', row.names = 1)

# Creating data_table variable before writing it to .csv file
data_table <- rbind(data_table_2010, data_table_2019)
write.csv(
  data_table,
  file = 'data_table.csv'
)