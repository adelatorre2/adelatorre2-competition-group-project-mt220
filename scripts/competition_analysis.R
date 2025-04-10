# competition_analysis.R
# Author: Alejandro De La Torre
# Purpose: Load and explore the competition data

# Load packages
library(readxl)
library(dplyr)

# Load data
competition <- read_excel("../data/competition_last.xlsx")

# Glimpse structure
glimpse(competition)

# Add your analysis code below