

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
 
 # -----------------------------
 # 1. Decision Tree Modeling
 # -----------------------------
 # Objective: Predict player throw based on opponent’s previous throw and prior result
 # Data prep (example structure - replace with actual columns)
 # df_model <- data.frame(Opponent_Throw, Previous_Result, Round, Player_Throw)
 
 # Example model (to be customized based on available columns)
 # fit <- rpart(Player_Throw ~ Opponent_Throw + Previous_Result + Round,
 #              data = df_model, method = "class")
 # rpart.plot(fit)
 
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