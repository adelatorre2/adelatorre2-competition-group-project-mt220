# part3_standings_strategy.R
#
# Description:
#   Script for Part 3 of the MT220-01 Group Project. This script calculates and summarizes
#   the final standings of a Rock-Paper-Scissors competition based on both game outcomes and
#   individual match outcomes. The script includes:
#     - Total match wins, losses, and ties for each player
#     - Total game wins and losses for each player
#     - Final standings sorted by game wins and match wins
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
#   - Player-level match statistics: report/tables/part3_total_match_results.csv
#   - Player-level game outcomes: report/tables/part3_total_game_results.csv
#
# Usage:
#   Source this script in RStudio or run sections interactively.

# -----------------------------
# Setup
# -----------------------------
library(readxl)
library(dplyr)
library(readr)

# Load data
df <- read_excel("data/competition_last.xlsx")

players <- unique(df$game)
match_wins <- setNames(rep(0, length(players)), players)
match_losses <- setNames(rep(0, length(players)), players)
match_draws <- setNames(rep(0, length(players)), players)

game_wins <- setNames(rep(0, length(players)), players)
game_losses <- setNames(rep(0, length(players)), players)

# Define result function
get_result <- function(p1, p2) {
  if (p1 == p2) return(c("Draw", "Draw"))
  win_map <- list(R = "S", P = "R", S = "P")
  if (win_map[[p1]] == p2) return(c("Win", "Loss"))
  return(c("Loss", "Win"))
}

# Analyze games
for (i in seq(1, nrow(df), by = 2)) {
  p1 <- df[i, 1, drop = TRUE][[1]]
  p2 <- df[i + 1, 1, drop = TRUE][[1]]
  game <- df[c(i, i + 1), -1]
  
  p1_wins <- 0
  p2_wins <- 0
  
  for (col_idx in seq_along(game)) {
    move1 <- game[1, col_idx, drop = TRUE][[1]]
    move2 <- game[2, col_idx, drop = TRUE][[1]]
    if (is.na(move1) || is.na(move2)) next
    
    result <- get_result(move1, move2)
    
    if (result[1] == "Win") {
      match_wins[p1] <- match_wins[p1] + 1
      match_losses[p2] <- match_losses[p2] + 1
      p1_wins <- p1_wins + 1
    } else if (result[1] == "Loss") {
      match_losses[p1] <- match_losses[p1] + 1
      match_wins[p2] <- match_wins[p2] + 1
      p2_wins <- p2_wins + 1
    } else {
      match_draws[p1] <- match_draws[p1] + 1
      match_draws[p2] <- match_draws[p2] + 1
    }
    
    if (p1_wins == 3 || p2_wins == 3) break
  }
  
  if (p1_wins == 3) {
    game_wins[p1] <- game_wins[p1] + 1
    game_losses[p2] <- game_losses[p2] + 1
  } else if (p2_wins == 3) {
    game_wins[p2] <- game_wins[p2] + 1
    game_losses[p1] <- game_losses[p1] + 1
  }
}

# -----------------------------
# Match Results
# -----------------------------
match_df <- data.frame(
  Player = names(match_wins),
  Match_Wins = match_wins,
  Match_Losses = match_losses,
  Match_Draws = match_draws
) %>%
  arrange(desc(Match_Wins), Match_Losses)

# -----------------------------
# Game Results with Corrected Ranking
# -----------------------------
game_df <- data.frame(
  Player = names(game_wins),
  Game_Wins = game_wins,
  Game_Losses = game_losses
) %>%
  arrange(desc(Game_Wins), Game_Losses)

# Compute proper rankings with ties
rank_vals <- unique(game_df[, c("Game_Wins", "Game_Losses")]) %>%
  arrange(desc(Game_Wins), Game_Losses)

rank_vals$Rank <- seq_len(nrow(rank_vals))

# Join ranks back to main df
game_df <- left_join(game_df, rank_vals, by = c("Game_Wins", "Game_Losses"))

# -----------------------------
# Save CSVs
# -----------------------------
output_dir <- "report/tables/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write.csv(match_df, file.path(output_dir, "part3_total_match_results.csv"), row.names = FALSE)
write.csv(game_df, file.path(output_dir, "part3_total_game_results.csv"), row.names = FALSE)

# -----------------------------
# Groupmate Strategy Summary Table
# -----------------------------
# Define your group; you can edit based on who you want
group <- c("alejandro", "isabel", "brian", "matt", "meredith")

# Load data
game_df <- read_csv("report/tables/part2_strategy_shift_significance.csv")
overall <- read_csv("report/tables/part2_overall_counts.csv")
shifts <- read_csv("report/tables/part2_strategy_shift_significance.csv")
first_outcomes <- read_csv("report/tables/part2_first2_outcomes.csv")
rest_outcomes <- read_csv("report/tables/part2_rest_outcomes.csv")
game_standings <- read_csv("report/tables/part3_total_game_results.csv")

# Filter for group members
game_stats <- game_standings %>%
  filter(Player %in% group) %>%
  select(Player, Game_Wins, Game_Losses, Rank)

hand_pref <- overall %>%
  filter(Player %in% group) %>%
  rowwise() %>%
  mutate(
    Most_Used_Throw = {
      throws <- c(R = R, P = P, S = S)
      names(which.max(throws))
    }
  ) %>%
  ungroup() %>%
  select(Player, Most_Used_Throw)

shift_sig <- shifts %>%
  filter(Player %in% group) %>%
  select(Player, Significant)

first_summary <- first_outcomes %>%
  filter(Player %in% group) %>%
  rename(First_Win = Win, First_Loss = Loss, First_Draw = Draw)

rest_summary <- rest_outcomes %>%
  filter(Player %in% group) %>%
  rename(Rest_Win = Win, Rest_Loss = Loss, Rest_Draw = Draw)

# Join all together
summary_df <- game_stats %>%
  left_join(hand_pref, by = "Player") %>%
  left_join(first_summary, by = "Player") %>%
  left_join(rest_summary, by = "Player") %>%
  left_join(shift_sig, by = "Player") %>%
  select(
    Player,
    Game_Wins,
    Game_Losses,
    Rank,
    Most_Used_Throw,
    First_Win, First_Loss, First_Draw,
    Rest_Win, Rest_Loss, Rest_Draw,
    Significant
  )

# Print to console
print(summary_df)

# Optional: Save to CSV
write.csv(summary_df, "report/tables/part3_groupmate_strategy_summary.csv", row.names = FALSE)

# -----------------------------
# Groupmate Interpretation Paragraphs
# -----------------------------

interpretation_text <- summary_df %>%
  mutate(
    Significant = ifelse(Significant == "Yes", "did", "did not"),
    Paragraph = paste0(
      "**", tools::toTitleCase(Player), "** ranked **#", Rank, "** with **", Game_Wins, 
      "** wins and **", Game_Losses, "** losses. Their most-used hand was **", Most_Used_Throw, "**.\n",
      "In early rounds, they had **", First_Win, "** wins, **", First_Loss, "** losses, and **", First_Draw, "** draws.\n",
      "In later rounds, they had **", Rest_Win, "** wins, **", Rest_Loss, "** losses, and **", Rest_Draw, "** draws.\n",
      "They **", Significant, " significantly shift strategy** between early and late rounds.\n"
    )
  ) %>%
  pull(Paragraph)

# Print to console
cat(paste(interpretation_text, collapse = "\n\n"))

# Optional: Save to a markdown or text file
writeLines(interpretation_text, "report/tables/part3_groupmate_interpretation.md")