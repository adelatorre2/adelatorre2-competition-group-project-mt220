# -----------------------------
 # Part 4: Independent Analysis
 # -----------------------------
 # Author: Alejandro De La Torre
 # Date: April 20, 2025
 #
 # Description:
 # This script explores three independent aspects of player strategy and outcomes in the
 # Rock-Paper-Scissors competition dataset using R. These include:
 # (1) Decision tree modeling of player throw behavior,
 # (2) Transition patterns of throws (Markov chain),
 # (3) Match length vs. outcome type analysis.
 #
 # Note: Datasets should be preprocessed or loaded accordingly.
 
 # Load required packages
 library(dplyr)
 library(ggplot2)
 library(rpart)
 library(rpart.plot)
 library(tidyr)
 library(readxl)
 
 # -----------------------------
 # 1. Decision Tree Modeling
 # -----------------------------
 # Objective: Predict player throw based on opponent’s previous throw and prior result

 # Load match data
 raw_df <- read_excel("data/competition_last.xlsx")

 # Reformat data into tidy long format (cleaner and structurally consistent)
 num_rounds <- ncol(raw_df) - 1
 num_games <- nrow(raw_df) / 2

long_df <- raw_df %>%
  mutate(Game = rep(1:num_games, each = 2),
         Player_ID = rep(1:2, times = num_games)) %>%
  pivot_longer(cols = -c(Game, Player_ID), names_to = "Round", values_to = "Throw") %>%
  mutate(Round = as.integer(Round)) %>%
  filter(!is.na(Throw))

 # Add Player_ID (1 for first row of each game pair, 2 for second)
 long_df$Player_ID <- rep(rep(1:2, each = 1), length.out = nrow(long_df))

# Rebuild game index
num_rounds <- ncol(raw_df) - 1
num_games <- nrow(raw_df) / 2
total_throws_per_game <- 2 * num_rounds

game_index <- rep(1:num_games, each = total_throws_per_game)

# Adjust to ensure game_index matches number of rows in long_df
if (length(game_index) > nrow(long_df)) {
  game_index <- game_index[1:nrow(long_df)]
} else if (length(game_index) < nrow(long_df)) {
  game_index <- rep(game_index, length.out = nrow(long_df))
}

long_df$Game_ID <- game_index

 # Separate throws into wide format
 wide_df <- long_df %>%
   pivot_wider(names_from = Player_ID, values_from = Throw, names_prefix = "Player_") %>%
   rename(Player_Throw = Player_1, Opponent_Throw = Player_2) %>%
   filter(!is.na(Player_Throw) & !is.na(Opponent_Throw))

 # Derive previous result for player
 wide_df <- wide_df %>%
   mutate(
     Player_Throw = as.character(Player_Throw),
     Opponent_Throw = as.character(Opponent_Throw)
   ) %>%
   group_by(Game_ID) %>%
   mutate(
     Previous_Result = lag(case_when(
       Player_Throw == Opponent_Throw ~ "Draw",
       (Player_Throw == "R" & Opponent_Throw == "S") |
       (Player_Throw == "P" & Opponent_Throw == "R") |
       (Player_Throw == "S" & Opponent_Throw == "P") ~ "Win",
       TRUE ~ "Loss"
     ))
   ) %>%
   ungroup() %>%
   filter(!is.na(Previous_Result))

 # Build model
 df_model <- wide_df %>%
   select(Player_Throw, Opponent_Throw, Previous_Result)

 fit <- rpart(Player_Throw ~ Opponent_Throw + Previous_Result,
              data = df_model, method = "class")

 # Plot decision tree
 rpart.plot(fit)

# Ensure output directory exists
if (!dir.exists("report/figures")) {
  dir.create("report/figures", recursive = TRUE)
}

# Save plot to PNG
png_path <- "report/figures/decision_tree_plot.png"
ggsave(filename = png_path, plot = rpart.plot(fit), width = 8, height = 6)

# Message to user
cat(paste("Decision tree plot saved to:", png_path, "\n"))

 # Optionally save model or predictions
 # saveRDS(fit, "report/models/decision_tree_model.rds")
 
 # -----------------------------
 # 2. Throw Transition Matrix (Markov Chain)
 # -----------------------------
 # Objective: Examine how often players transition from one throw to another
 # Use lag function to create previous and current throw columns per player
 # transition_df <- competition_df %>%
 #   group_by(Player) %>%
 #   mutate(Previous_Throw = lag(Throw)) %>%
 #   filter(!is.na(Previous_Throw))
 
 # transition_matrix <- table(transition_df$Previous_Throw, transition_df$Throw)
 # print(transition_matrix)
 # ggplot(transition_df, aes(x = Previous_Throw, fill = Throw)) + geom_bar(position = "fill")
 
 # -----------------------------
 # 3. Match Length vs. Outcome Analysis
 # -----------------------------
 # Objective: Analyze whether shorter or longer matches are associated with wins/losses
 # match_lengths_df <- data.frame(Player, Match_Length, Outcome)
 # ggplot(match_lengths_df, aes(x = Outcome, y = Match_Length)) +
 #   geom_boxplot() +
 #   labs(title = "Match Length by Outcome", y = "Number of Rounds")
 
 # -----------------------------
 # Notes:
 # - Each section above needs appropriate data loaded from cleaned match datasets
 # - Add write.csv() or ggsave() as needed for report deliverables