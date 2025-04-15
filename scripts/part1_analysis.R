# part1_analysis.R
# 
# Description: 
#   Script for Part 1 of the MT220-01 Group Project. This script performs 
#   theoretical probability calculations and empirical analysis of 
#   Rock-Paper-Scissors game data.
#   It includes:
#     - Theoretical matchup probabilities
#     - Observed frequencies of each hand
#     - Empirical matchup matrix
#     - Comparison between theoretical and empirical results
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
# Last Updated: April 12, 2025
#
# Dependencies: 
#   Requires the `readxl`, `dplyr`, and `tidyr` packages.
#
# Inputs:
#   - data/competition_last.xlsx
#
# Outputs:
#   - Tables and matrices for report inclusion (export optional)
#   - Console output for verification
#
# Usage:
#   Source this script in RStudio or run sections interactively.

library(readxl)
library(dplyr)
library(tidyr)

# Load data
df <- read_excel("data/competition_last.xlsx")

# View the first few rows to confirm structure
head(df)