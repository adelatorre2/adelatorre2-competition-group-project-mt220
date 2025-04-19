# part3_standings_strategy.R
#
# Description: 
#   Script for Part 3 of the MT220-01 Group Project. This script analyzes the final standings 
#   of players in the Rock-Paper-Scissors dataset and compares strategic behavior across groups. 
#   It includes:
#     - Final player standings based on total wins and win-loss differential
#     - Comparison of R/P/S usage proportions for top 5 vs. bottom 5 players
#     - Comparison of first two rounds' outcomes (win/loss/draw) for top 5 vs. bottom 5 players
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
#   Requires the `dplyr` and `readr` packages, and assumes the Part 2 script 
#   has been run successfully to generate intermediate CSVs.
#
# Inputs:
#   - report/tables/part2_rest_outcomes.csv
#   - report/tables/part2_overall_counts.csv
#   - report/tables/part2_first2_outcomes.csv
#
# Outputs:
#   - Final standings and group comparisons (saved in report/tables/)
#     • part3_standings.csv
#     • part3_top5_vs_bottom5_strategy.csv
#     • part3_top5_vs_bottom5_outcomes.csv
#
# Usage:
#   Source this script in RStudio or run sections interactively.

# Ensure output directory exists
output_dir <- "report/tables/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 1. Compute total wins and win-loss differential
win_data <- rest_out_df
win_data$Wins <- win_data$Win
win_data$Losses <- win_data$Loss
win_data$Draws <- win_data$Draw
win_data$W_L_Diff <- win_data$Wins - win_data$Losses

# 2. Merge with overall R/P/S counts
merged_data <- merge(win_data, overall_df, by = "Player")

# 3. Rank players
standings <- merged_data %>%
  arrange(desc(Wins), desc(W_L_Diff)) %>%
  select(Player, Wins, Losses, Draws, W_L_Diff, R, P, S)

# Save to CSV
write.csv(standings, file.path(output_dir, "part3_standings.csv"), row.names = FALSE)

# 4. Extract top 5 and bottom 5 players
top5 <- standings$Player[1:5]
bottom5 <- standings$Player[(nrow(standings)-4):nrow(standings)]

# 5. Compare R/P/S strategy (as proportions)
get_props <- function(df, players) {
  subset <- df[df$Player %in% players, c("R", "P", "S")]
  rownames(subset) <- df$Player[df$Player %in% players]
  sweep(subset, 1, rowSums(subset), FUN = "/")
}

top5_strategy <- get_props(overall_df, top5)
bottom5_strategy <- get_props(overall_df, bottom5)

# Combine for export
strategy_comparison <- rbind(
  cbind(Group = "Top 5", Player = rownames(top5_strategy), round(top5_strategy, 2)),
  cbind(Group = "Bottom 5", Player = rownames(bottom5_strategy), round(bottom5_strategy, 2))
)

write.csv(strategy_comparison, file.path(output_dir, "part3_top5_vs_bottom5_strategy.csv"), row.names = FALSE)

# 6. Compare win/loss/draw in first two rounds
first2_out_subset <- first2_out_df %>%
  filter(Player %in% c(top5, bottom5)) %>%
  mutate(Group = ifelse(Player %in% top5, "Top 5", "Bottom 5")) %>%
  select(Group, Player, Win, Loss, Draw)

write.csv(first2_out_subset, file.path(output_dir, "part3_top5_vs_bottom5_outcomes.csv"), row.names = FALSE)

# Done!
cat("✅ Part 3 completed. Standings, strategy, and outcomes saved to report/tables/\n")