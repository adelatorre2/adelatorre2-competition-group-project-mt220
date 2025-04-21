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