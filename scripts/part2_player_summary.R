# part2_player_summary.R
#
# Description: 
#   Script for Part 2 of the MT220-01 Group Project. This script summarizes player behavior 
#   across all games in the Rock-Paper-Scissors dataset. It includes:
#     - Total hand counts (R/P/S) per player
#     - First two rounds: win/loss/tie hands per player
#     - First two rounds: R/P/S usage per player
#     - Remaining rounds: win/loss/tie hands per player
#     - Remaining rounds: R/P/S usage per player
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
# Last Updated: April 18, 2025
#
# Dependencies: 
#   Requires the `readxl`, `dplyr`, and `tidyr` packages.
#
# Inputs:
#   - data/competition_last.xlsx
#
# Outputs:
#   - Summary tables per player for report inclusion
#   - Console output for verification
#
# Usage:
#   Source this script in RStudio or run sections interactively.

# Load libraries
library(readxl)
library(dplyr)

# Read dataset
df <- read_excel("data/competition_last.xlsx")

# Extract match data
players <- df$game
match_data <- df[, -1]
unique_players <- unique(players)

# Initialize trackers
initialize_tracker <- function() {
  tracker <- list()
  for (p in unique_players) {
    tracker[[p]] <- list(R = 0, P = 0, S = 0)
  }
  return(tracker)
}
initialize_outcomes <- function() {
  tracker <- list()
  for (p in unique_players) {
    tracker[[p]] <- list(Win = 0, Loss = 0, Draw = 0)
  }
  return(tracker)
}

overall_counts <- initialize_tracker()
first2_counts <- initialize_tracker()
rest_counts <- initialize_tracker()
first2_outcomes <- initialize_outcomes()
rest_outcomes <- initialize_outcomes()

# Define result logic
get_result <- function(p1, p2) {
  if (p1 == p2) return(c("Draw", "Draw"))
  win_map <- list(R = "S", P = "R", S = "P")
  if (win_map[[p1]] == p2) return(c("Win", "Loss"))
  return(c("Loss", "Win"))
}

# Main processing
for (i in seq(1, nrow(df), by = 2)) {
  p1 <- df[i, 1, drop = TRUE][[1]]
  p2 <- df[i + 1, 1, drop = TRUE][[1]]
  game <- df[c(i, i + 1), -1]
  
  for (col_idx in seq_along(game)) {
    move1 <- game[1, col_idx, drop = TRUE][[1]]
    move2 <- game[2, col_idx, drop = TRUE][[1]]
    if (is.na(move1) || is.na(move2)) next
    
    # Overall throws
    overall_counts[[p1]][[move1]] <- overall_counts[[p1]][[move1]] + 1
    overall_counts[[p2]][[move2]] <- overall_counts[[p2]][[move2]] + 1
    
    # First two rounds
    if (col_idx <= 2) {
      first2_counts[[p1]][[move1]] <- first2_counts[[p1]][[move1]] + 1
      first2_counts[[p2]][[move2]] <- first2_counts[[p2]][[move2]] + 1
      result <- get_result(move1, move2)
      first2_outcomes[[p1]][[result[1]]] <- first2_outcomes[[p1]][[result[1]]] + 1
      first2_outcomes[[p2]][[result[2]]] <- first2_outcomes[[p2]][[result[2]]] + 1
    } else {
      # Post-round 2 stats
      rest_counts[[p1]][[move1]] <- rest_counts[[p1]][[move1]] + 1
      rest_counts[[p2]][[move2]] <- rest_counts[[p2]][[move2]] + 1
      result <- get_result(move1, move2)
      rest_outcomes[[p1]][[result[1]]] <- rest_outcomes[[p1]][[result[1]]] + 1
      rest_outcomes[[p2]][[result[2]]] <- rest_outcomes[[p2]][[result[2]]] + 1
    }
  }
}

# Convert to dataframes
list_to_df <- function(lst) {
  data.frame(t(sapply(lst, unlist)))
}
overall_df <- list_to_df(overall_counts)
first2_df <- list_to_df(first2_counts)
rest_df <- list_to_df(rest_counts)
first2_out_df <- list_to_df(first2_outcomes)
rest_out_df <- list_to_df(rest_outcomes)

# Print Results Locally
print(overall_df)
print(first2_df)
print(rest_df)
print(first2_out_df)
print(rest_out_df)

# Write outputs into CSVs
write.csv(overall_df, "report/part2_overall_counts.csv")
write.csv(first2_df, "report/part2_first2_counts.csv")
write.csv(rest_df, "report/part2_rest_counts.csv")
write.csv(first2_out_df, "report/part2_first2_outcomes.csv")
write.csv(rest_out_df, "report/part2_rest_outcomes.csv")