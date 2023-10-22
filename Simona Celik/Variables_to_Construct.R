# load libraries
library(tidyverse)
library(here)
library(psych)
library(stargazer)

# Read data
df = read_rds(here("Simona Celik", "clean_data.rds"))

# Calculate the correlation matrix for the corruption columns of interest 
#"Q177, "Q178", "Q179", "Q180", and "Q181"
corrupt_vars =  c("Q177", "Q178", "Q179", "Q180", "Q181")

correlation <- cor(df[,corrupt_vars], use = "complete.obs")

# Display the correlation matrix
correlation

# Cronbach's alpha for these questions
chron = alpha(df[ , corrupt_vars])

# Extract table
chron$total |> 
  round(2) |> 
  stargazer(summary = FALSE, label = "chron", 
            out = here("Figures", "chron_corrupt.tex"))

# Calculate the composite corruption score by averaging the variables of interest
df$corruption_permisiveness <- rowMeans(df[, corrupt_vars], 
                                        na.rm = TRUE)

# Public sector dummy factor
df = 
  df |> 
  mutate(public_sector = ifelse(Q284 == 1, "public", "private"),
         public_sector = fct_relevel(public_sector, c("private", "public")))

# Rename the variables
df <- df %>%
  rename(democracy_support = Q250, 
         vote = Q222, 
         gender = Q260,
         income = Q288R,
         trust = Q291G1,
         age = X003R2, 
         education = Q275R)

# Output data
write_rds(df, file = here("Simona Celik", "final_path.rds"))
