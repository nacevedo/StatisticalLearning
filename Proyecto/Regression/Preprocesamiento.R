library(readr)
Train_FIFA <- read_delim("Train_FIFA.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
Test_FIFA <- read_delim("Test_FIFA.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

