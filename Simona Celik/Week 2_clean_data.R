### PURPOSE: Directed Reading, Fall 2023
### BY: Simona Celik
### DATE: October, 2023

# load libraries
library(tidyverse)
library(stargazer)
library(vroom)
library(here)
library(dplyr)


# Read data
S1 = vroom::vroom("https://www.dropbox.com/scl/fi/g1bveytab3cm5xfp4dg8u/WVS_Cross-National_Wave_7_csv_v5_0.csv?rlkey=wn19tczhyvjltokf7673gw1er&dl=1")

# Load the data you created (regime type, democracy index, corruption index and GINI)
S2 <-read_csv("https://www.dropbox.com/scl/fi/80jngitqcrh8rcpb1fooq/Wave-7.xlsx-Sheet1.csv?rlkey=0tlrkrwhqxrhfkxg5r2zz5tz8&dl=1")

# Combine the to datasets by country code
S3 <- left_join(S1, S2, by = 'B_COUNTRY')

# Variables you want to keep
vars_to_keep <- c("COUNTRY", "REGION", "CORRUPTION_INDEX", "GINI", "REGIME") 

# Subset the columns you want to keep (vars_to_keep)
S3 <- S3[, c(vars_to_keep)]

# Save clean data
write_rds(S3, file = here("Simona Celik", "clean_data.rds"))
