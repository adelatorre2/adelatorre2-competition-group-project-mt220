# -----------------------------
# Match Count Distribution Analysis
# -----------------------------

# Load required libraries
library(dplyr)
library(ggplot2)
library(readxl)

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