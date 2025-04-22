# part4_independent_analysis.R
#
## Description:
#   Script for Part 4 of the MT220-01 Group Project. This script independently analyzes strategic behavior
#   in a Rock-Paper-Scissors tournament using logistic regression and visualization. It includes:
#     - Summary of match length distributions across games
#     - Reformatting the data into long format with one row per player per match
#     - Construction of lagged variables to track prior throws and outcomes
#     - A multinomial logistic regression model predicting throw choice
#     - Visualization of predicted probabilities conditioned on previous results
#
## Author(s): 
#   Alejandro De La Torre
#   Isabel Nold
#   Brian Tobin
#   Matt Schwartz
#   Meredith Kendall
#
## Course:
#   MT220-01 Introduction to Probability and Statistics (Spring 2025)
#
## Instructor:
#   Pep Mateu
#
## Last Updated: April 22, 2025
#
## Dependencies:
#   Requires the `dplyr`, `ggplot2`, `readxl`, `writexl`, `nnet`, and `tidyr` packages.
#
## Inputs:
#   - data/competition_last.xlsx
#
## Outputs:
#   - report/figures/match_length_distribution_exact.png
#   - data/competition_last_long.xlsx
#   - report/figures/predicted_throw_probabilities_by_result.png
#
## Usage:
#   Source this script in RStudio or run sections interactively.

# -----------------------------
# Match Count Distribution Analysis
# -----------------------------

# Load required libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)

# Load the raw match data
raw_df <- read_excel("data/competition_last.xlsx")

# Initialize match count storage
match_counts <- c()

# For each row pair (game), count number of valid match rounds
for (i in seq(1, nrow(raw_df), by = 2)) {
  game_rows <- raw_df[i:(i+1), -1]  # drop player names column
  non_na_cols <- colSums(!is.na(game_rows)) >= 2  # keep only full matches
  match_counts <- c(match_counts, sum(non_na_cols))
}

# Mean number of matches per game
mean_matches <- mean(match_counts)
cat(paste("Mean number of matches per game:", round(mean_matches, 2), "\n"))

# Count frequency of each exact match length
df_plot <- data.frame(Match_Length = match_counts) %>%
  group_by(Match_Length) %>%
  summarise(Game_Count = n())

# Ensure output directory exists
if (!dir.exists("report/figures")) {
  dir.create("report/figures", recursive = TRUE)
}

# Plot the bar chart of exact match counts
plot_path <- "report/figures/match_length_distribution_exact.png"
ggplot(df_plot, aes(x = factor(Match_Length), y = Game_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Exact Match Length Distribution Across Games",
       x = "Exact Number of Matches in a Game",
       y = "Count of Games") +
  theme_minimal()

# Save the plot
ggsave(plot_path, width = 8, height = 6)

cat(paste("Bar chart saved to:", plot_path, "\n"))

# NOTE: we have crossed/nested panel data

# -----------------------------
# Reformat raw data into long format
# -----------------------------

# Load the original data again (to ensure independence from previous steps)
raw_df <- read_excel("data/competition_last.xlsx")

# Initialize long format data frame
long_list <- list()

match_id <- 1
for (i in seq(1, nrow(raw_df), by = 2)) {
  player1 <- raw_df[i, ]
  player2 <- raw_df[i + 1, ]
  game_id <- (i + 1) / 2

  for (j in 2:ncol(raw_df)) {
    throw1 <- as.character(player1[[j]])
    throw2 <- as.character(player2[[j]])
    if (!is.na(throw1) && !is.na(throw2)) {
      long_list[[length(long_list) + 1]] <- data.frame(
        Game_ID = game_id,
        Match_ID = j - 1,
        Player_ID = player1[[1]],
        Throw = throw1,
        Opponent_ID = player2[[1]],
        Opponent_Throw = throw2,
        stringsAsFactors = FALSE
      )
      long_list[[length(long_list) + 1]] <- data.frame(
        Game_ID = game_id,
        Match_ID = j - 1,
        Player_ID = player2[[1]],
        Throw = throw2,
        Opponent_ID = player1[[1]],
        Opponent_Throw = throw1,
        stringsAsFactors = FALSE
      )
    }
  }
}

long_df <- do.call(rbind, long_list)

# Save to new Excel file
if (!dir.exists("data")) {
  dir.create("data")
}

writexl::write_xlsx(long_df, "data/competition_last_long.xlsx")
cat("Reformatted long dataset saved to data/competition_last_long.xlsx\n")

# -----------------------------
# Model Likelihood Player Throws R/P/S
# -----------------------------

# Add Lag Variables
# Load long data
df_lagged <- readxl::read_xlsx("data/competition_last_long.xlsx")

# Add Previous Throw and Previous Result
df_lagged <- df_lagged %>%
  arrange(Player_ID, Game_ID, Match_ID) %>%
  group_by(Player_ID) %>%
  mutate(
    Previous_Throw = lag(Throw),
    Previous_Opponent_Throw = lag(Opponent_Throw),
    Previous_Result = lag(case_when(
      Throw == Opponent_Throw ~ "Draw",
      (Throw == "R" & Opponent_Throw == "S") |
      (Throw == "P" & Opponent_Throw == "R") |
      (Throw == "S" & Opponent_Throw == "P") ~ "Win",
      TRUE ~ "Loss"
    ))
  ) %>%
  ungroup() %>%
  filter(!is.na(Previous_Throw), !is.na(Previous_Result))

# Make sure Throw is a factor (categorical)
df_lagged$Throw <- as.factor(df_lagged$Throw)

# Load the Modeling library
library(nnet)  # for multinom()

# Multinomial Logistic Regression Model
model_rps <- multinom(
  Throw ~ Previous_Throw + Previous_Opponent_Throw + Previous_Result,
  data = df_lagged
)

# Interpretation of coefficients

summary(model_rps)

# -----------------------------
# Visualize Predicted Probabilities
# -----------------------------

# Calculate predicted probabilities
df_probs <- df_lagged %>%
  mutate(
    Prob_R = predict(model_rps, newdata = df_lagged, type = "probs")[, "R"],
    Prob_P = predict(model_rps, newdata = df_lagged, type = "probs")[, "P"],
    Prob_S = predict(model_rps, newdata = df_lagged, type = "probs")[, "S"]
  )

# Reshape for plotting
library(tidyr)
df_probs_long <- df_probs %>%
  pivot_longer(cols = starts_with("Prob_"), 
               names_to = "Predicted_Throw", 
               names_prefix = "Prob_", 
               values_to = "Probability")

# Plot average predicted probabilities by previous result
if (!dir.exists("report/figures")) {
  dir.create("report/figures", recursive = TRUE)
}

plot_path_probs <- "report/figures/predicted_throw_probabilities_by_result.png"
ggplot(df_probs_long, aes(x = Previous_Result, y = Probability, fill = Predicted_Throw)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  labs(title = "Predicted Probability of Throw by Previous Result",
       x = "Previous Result",
       y = "Predicted Probability") +
  theme_minimal()

ggsave(plot_path_probs, width = 8, height = 6)
cat(paste("Predicted probabilities plot saved to:", plot_path_probs, "\n"))

# Reorder reference category; R as Baseline
df_lagged$Throw <- relevel(df_lagged$Throw, ref = "R")

# Run the model again
model_rps <- multinom(
  Throw ~ Previous_Throw + Previous_Opponent_Throw + Previous_Result,
  data = df_lagged
)
# Interpretation of coefficients w/ R as baseline
summary(model_rps)