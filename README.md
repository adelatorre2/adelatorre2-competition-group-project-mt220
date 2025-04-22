# Rock-Paper-Scissors Tournament Analysis

This repository contains the full analysis for the MT220-01 Group Project (Spring 2025), where we explore strategic behavior in a Rock-Paper-Scissors tournament dataset collected during class.

## 🧠 Team Members
- Alejandro De La Torre
- Isabel Nold
- Brian Tobin
- Matt Schwartz
- Meredith Kendall

## 📁 Directory Structure

```
├── data/                    # Original and reformatted datasets
│   ├── competition_last.xlsx
│   ├── competition_last_long.xlsx
│   └── competition_last_long_lagged.xlsx
├── report/
│   ├── tables/              # CSV tables and markdown reports
│   └── figures/             # Visualizations and plots
├── scripts/                 # R scripts for parts 1–4
│   ├── part2_player_summary.R
│   ├── part3_standings_strategy.R
│   └── part4_independent_analysis.R
├── README.md
```

## 📜 Project Components

### Part 2 – Player Behavior Summary
- Computes hand counts and win/loss/draw outcomes by player.
- Separates behavior by early vs. later match rounds.
- Tests for statistically significant strategy shifts.

### Part 3 – Final Standings & Strategy Analysis
- Game and match-level ranking of players.
- Strategy summaries for each team member.
- Comparison of top 5 and bottom 5 players.
- Early round outcome comparison across rankings.

### Part 4 – Independent Modeling & Exploration
- Reformats dataset into long panel structure.
- Adds lagged variables (previous throw/result).
- Builds multinomial logistic regression model.
- Visualizes predicted probabilities for each throw.

## 🔧 How to Run the Code

Each script is self-contained and reproducible. To run the project:

1. Clone this repository.
2. Open in RStudio or VSCode with R support.
3. Run the scripts in this order:
   - `scripts/part2_player_summary.R`
   - `scripts/part3_standings_strategy.R`
   - `scripts/part4_independent_analysis.R`

All outputs will be saved in `report/tables/` and `report/figures/`.

## 📦 Dependencies

Make sure the following packages are installed:
- `readxl`
- `dplyr`
- `tidyr`
- `ggplot2`
- `writexl`
- `nnet`
- `rpart`
- `rpart.plot`

## 🔄 Git Workflow Tips

- Always `Pull` before making changes.
- `Commit` with clear, descriptive messages.
- `Push` regularly to update your team.

## 🏁 Final Notes

This project exemplifies applied statistics and collaborative research. For additional details, see generated reports and figures under the `report/` directory.