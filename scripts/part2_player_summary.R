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
# Last Updated: April 19, 2025
#
# Dependencies: 
#   Requires the `readxl`, `dplyr`, and `readr` packages.
#
# Inputs:
#   - data/competition_last.xlsx
#
# Outputs:
#   - Summary tables per player for report inclusion (saved in report/tables/)
#   - Console output for verification
#
# Usage:
#   Source this script in RStudio or run sections interactively.

# -----------------------------
# Setup
# -----------------------------
library(readxl)
library(dplyr)
library(readr)

# Read dataset
df <- read_excel("data/competition_last.xlsx")
players <- df$game
match_data <- df[, -1]
unique_players <- unique(players)

# -----------------------------
# Initialize helper functions
# -----------------------------
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

get_result <- function(p1, p2) {
  if (p1 == p2) return(c("Draw", "Draw"))
  win_map <- list(R = "S", P = "R", S = "P")
  if (win_map[[p1]] == p2) return(c("Win", "Loss"))
  return(c("Loss", "Win"))
}

list_to_df <- function(lst) {
  df <- data.frame(t(sapply(lst, unlist)))
  df$Player <- rownames(df)
  rownames(df) <- NULL
  return(df)
}

# -----------------------------
# Trackers
# -----------------------------
overall_counts <- initialize_tracker()
first2_counts <- initialize_tracker()
rest_counts <- initialize_tracker()
first2_outcomes <- initialize_outcomes()
rest_outcomes <- initialize_outcomes()

# -----------------------------
# Main processing loop
# -----------------------------
for (i in seq(1, nrow(df), by = 2)) {
  p1 <- df[i, 1, drop = TRUE][[1]]
  p2 <- df[i + 1, 1, drop = TRUE][[1]]
  game <- df[c(i, i + 1), -1]
  
  for (col_idx in seq_along(game)) {
    move1 <- game[1, col_idx, drop = TRUE][[1]]
    move2 <- game[2, col_idx, drop = TRUE][[1]]
    if (is.na(move1) || is.na(move2)) next
    
    # Overall
    overall_counts[[p1]][[move1]] <- overall_counts[[p1]][[move1]] + 1
    overall_counts[[p2]][[move2]] <- overall_counts[[p2]][[move2]] + 1
    
    # First 2 rounds
    if (col_idx <= 2) {
      first2_counts[[p1]][[move1]] <- first2_counts[[p1]][[move1]] + 1
      first2_counts[[p2]][[move2]] <- first2_counts[[p2]][[move2]] + 1
      result <- get_result(move1, move2)
      first2_outcomes[[p1]][[result[1]]] <- first2_outcomes[[p1]][[result[1]]] + 1
      first2_outcomes[[p2]][[result[2]]] <- first2_outcomes[[p2]][[result[2]]] + 1
    } else {
      # Remaining rounds
      rest_counts[[p1]][[move1]] <- rest_counts[[p1]][[move1]] + 1
      rest_counts[[p2]][[move2]] <- rest_counts[[p2]][[move2]] + 1
      result <- get_result(move1, move2)
      rest_outcomes[[p1]][[result[1]]] <- rest_outcomes[[p1]][[result[1]]] + 1
      rest_outcomes[[p2]][[result[2]]] <- rest_outcomes[[p2]][[result[2]]] + 1
    }
  }
}

# -----------------------------
# Convert to DataFrames
# -----------------------------
overall_df <- list_to_df(overall_counts)
first2_df <- list_to_df(first2_counts)
rest_df <- list_to_df(rest_counts)
first2_out_df <- list_to_df(first2_outcomes)
rest_out_df <- list_to_df(rest_outcomes)

# -----------------------------
# Save outputs
# -----------------------------
output_dir <- "report/tables/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write.csv(overall_df, file.path(output_dir, "part2_overall_counts.csv"), row.names = FALSE)
write.csv(first2_df, file.path(output_dir, "part2_first2_counts.csv"), row.names = FALSE)
write.csv(rest_df, file.path(output_dir, "part2_rest_counts.csv"), row.names = FALSE)
write.csv(first2_out_df, file.path(output_dir, "part2_first2_outcomes.csv"), row.names = FALSE)
write.csv(rest_out_df, file.path(output_dir, "part2_rest_outcomes.csv"), row.names = FALSE)

# -----------------------------
# Chi-Square Test of Independence
# -----------------------------
first2 <- read_csv(file.path(output_dir, "part2_first2_counts.csv"))
rest <- read_csv(file.path(output_dir, "part2_rest_counts.csv"))

# Preserve player names
player_names <- first2$Player
first2 <- first2[, c("R", "P", "S")]
rest <- rest[, c("R", "P", "S")]

# Prepare results
results <- data.frame(
  Player = player_names,
  p_value = NA,
  Chi_Square_Stat = NA,
  Significant = NA
)

for (i in seq_len(nrow(first2))) {
  test_matrix <- rbind(
    as.numeric(first2[i, ]),
    as.numeric(rest[i, ])
  )
  colnames(test_matrix) <- c("R", "P", "S")
  rownames(test_matrix) <- c("First2", "Rest")
  
  test <- suppressWarnings(chisq.test(test_matrix))
  results$p_value[i] <- round(test$p.value, 4)
  results$Chi_Square_Stat[i] <- round(test$statistic, 3)
  results$Significant[i] <- ifelse(test$p.value < 0.05, "Yes", "No")
}

# Save test results
print(results)
write.csv(results, file.path(output_dir, "part2_strategy_shift_significance.csv"), row.names = FALSE)