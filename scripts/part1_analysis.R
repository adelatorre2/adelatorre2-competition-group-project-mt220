# part1_analysis.R
# 
# Description: 
#   Script for Part 1 of the MT220-01 Group Project. This script performs 
#   theoretical probability calculations and empirical analysis of 
#   Rock-Paper-Scissors game data.
#   It includes:
#     - Theoretical matchup probabilities
#     - Observed frequencies of each hand
#     - Empirical matchup matrix
#     - Comparison between theoretical and empirical results
#
# Author(s): 
#   Alejandro De La Torre
#   Isabel Nold
#   Brian Tobin
#   Matt Schwartz
#   Meredith Kendall
#
# Course: 
#   MT220-01 Introduction to Probability and Statistics (Spring 2025)
#
# Instructor: 
#   Pep Mateu
#
# Last Updated: April 12, 2025
#
# Dependencies: 
#   Requires the `readxl`, `dplyr`, and `tidyr` packages.
#
# Inputs:
#   - data/competition_last.xlsx
#
# Outputs:
#   - Tables and matrices for report inclusion (export optional)
#   - Console output for verification
#
# Usage:
#   Source this script in RStudio or run sections interactively.

library(readxl)
library(dplyr)
library(tidyr)

# Load data
df <- read_excel("data/competition_last.xlsx")

# View the first few rows to confirm structure
head(df)

# Theoretical 3x3 matrix where each player picks R, P, or S independently
theoretical_probs <- matrix(
  rep(1/9, 9),  # Since 1/3 * 1/3 = 1/9 for each combination
  nrow = 3, 
  ncol = 3,
  dimnames = list(
    Player1 = c("R", "P", "S"),
    Player2 = c("R", "P", "S")
  )
)
print("Theoretical Matrix:")
print(theoretical_probs)

# Initialize matrix to count frequencies of each matchup
empirical_matrix <- matrix(0, 3, 3,
                           dimnames = list(Player1 = c("R", "P", "S"), Player2 = c("R", "P", "S"))
)

# Loop through rows 2 at a time (player1 vs player2)
for (i in seq(1, nrow(df), by = 2)) {
  row1 <- df[i, ]
  row2 <- df[i + 1, ]
  
  for (col in 2:ncol(df)) {
    move1 <- row1[[col]]
    move2 <- row2[[col]]
    
    if (is.na(move1) || is.na(move2)) next
    
    empirical_matrix[move1, move2] <- empirical_matrix[move1, move2] + 1
  }
}

print("Empirical Matrix (Counts):")
print(empirical_matrix)

# Convert counts to proportions
total_rounds <- sum(empirical_matrix)
empirical_proportions <- empirical_matrix / total_rounds

print("Empirical Matrix (Proportions):")
print(round(empirical_proportions, 3))

# Save for the report in CSV tables
write.csv(empirical_matrix, "report/empirical_matrix.csv")
write.csv(empirical_proportions, "report/empirical_proportions.csv")
write.csv(theoretical_probs, "report/theoretical_matrix.csv")