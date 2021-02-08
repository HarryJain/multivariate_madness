# Load Libraries
library(psych)
library(rela)
library(corrplot)
library(PerformanceAnalytics)
library(plot3D)
library(scatterplot3d)
library(pander)
library(PerformanceAnalytics)
library(vegan)
library(vegan3d)
library(mgcv)
library(MASS)
library(rgl)
library(reshape2)
library(data.table)
library(aplpack)
library(fpc)
library(cluster)
library(ape)
library(plyr)

# Read in my data from the local csv file I created
TD <- read.csv("allteams_augmented_newest.csv", header = T, stringsAsFactors = FALSE)

# Make a vector of all variables from the data
allallvars <- c("id", "name", "year", "conference", "conference_filtered", 
                "conference_type","conference_type_num", "tourn_bid", "tourn_bid_num", 
                "seed", "seed_type", "seed_type_num", "fast_break_pts", 
                "points_off_turnovers", "second_chance_pts", "minutes", "points", 
                "off_rebounds", "def_rebounds", "rebounds", "assists", "steals", "blocks", 
                "turnovers", "personal_fouls", "flagrant_fouls", "blocked_att", 
                "field_goals_made", "field_goal_pct", "field_goals_att", "three_points_made",
                "three_points_pct", "three_points_att", "free_throws_made",
                "free_throws_pct", "free_throws_att", "two_points_made", "two_points_pct",
                "two_points_att", "points_in_paint", "efficiency", "true_shooting_att",
                "true_shooting_pct", "opp_fast_break_pts", "opp_points_off_turnovers",
                "opp_second_chance_pts", "opp_minutes", "opp_points", "opp_off_rebounds", 
                "opp_def_rebounds", "opp_rebounds", "opp_assists", "opp_steals", 
                "opp_blocks", "opp_turnovers", "opp_personal_fouls", 
                "opp_flagrant_fouls", "opp_blocked_att", "opp_field_goals_made", 
                "opp_field_goal_pct", "opp_field_goals_att", "opp_three_points_made", 
                "opp_three_points_pct", "opp_three_points_att", "opp_free_throws_made", 
                "opp_free_throws_pct", "opp_free_throws_att", "opp_two_points_made", 
                "opp_two_points_pct", "opp_two_points_att", "opp_points_in_paint", 
                "opp_efficiency", "opp_true_shooting_att", "opp_true_shooting_pct", 
                "tourn_games_played", "tourn_games_played_num", "log_tourn_games_played", 
                "log2_tourn_games_played", "tourn_fast_break_pts", 
                "tourn_points_off_turnovers", "tourn_second_chance_pts", "tourn_minutes", 
                "tourn_points", "tourn_off_rebounds", "tourn_def_rebounds", 
                "tourn_rebounds", "tourn_assists", "tourn_steals", "tourn_blocks", 
                "tourn_turnovers", "tourn_personal_fouls", "tourn_flagrant_fouls", 
                "tourn_blocked_att", "tourn_field_goals_made", "tourn_field_goal_pct", 
                "tourn_field_goals_att", "tourn_three_points_made", 
                "tourn_three_points_pct", "tourn_three_points_att", 
                "tourn_free_throws_made", "tourn_free_throws_pct", 
                "tourn_free_throws_att", "tourn_two_points_made", "tourn_two_points_pct", 
                "tourn_two_points_att", "tourn_points_in_paint", "tourn_efficiency",
                "tourn_true_shooting_att", "tourn_opp_fast_break_pts", 
                "tourn_opp_points_off_turnovers", "tourn_opp_second_chance_pts", 
                "tourn_opp_minutes", "tourn_opp_points", "tourn_opp_off_rebounds", 
                "tourn_opp_def_rebounds", "tourn_opp_rebounds", "tourn_opp_assists", 
                "tourn_opp_steals", "tourn_opp_blocks", "tourn_opp_turnovers", 
                "tourn_opp_personal_fouls", "tourn_opp_flagrant_fouls", 
                "tourn_opp_blocked_att", "tourn_opp_field_goals_made", 
                "tourn_opp_field_goal_pct", "tourn_opp_field_goals_att", 
                "tourn_opp_three_points_made", "tourn_opp_three_points_pct", 
                "tourn_opp_three_points_att", "tourn_opp_free_throws_made", 
                "tourn_opp_free_throws_pct", "tourn_opp_free_throws_att", 
                "tourn_opp_two_points_made", "tourn_opp_two_points_pct", 
                "tourn_opp_two_points_att", "tourn_opp_points_in_paint", 
                "tourn_opp_efficiency", "tourn_opp_true_shooting_att", 
                "tourn_point_diff", "wins", "win_pct", "losses", "home_wins", 
                "home_win_pct", "home_losses", "away_wins", "away_wins_pct", 
                "away_losses", "neut_wins", "neut_win_pct", "neut_losses", 
                "owp", "oowp", "awp", "sos", "rpi")

# Create a category column based on the number of games played,
# makes ordination plots easier to read than with numbers only
# Teams that only played 1 game
TD$tourncat[(TD$tourn_games_played == 1)] = "First Round"
# Teams that played 2 games
TD$tourncat[(TD$tourn_games_played == 2)] = "Second Round"
# Teams that played 3 games
TD$tourncat[(TD$tourn_games_played == 3)] = "Sweet Sixteen"
# Teams that played 4 games
TD$tourncat[(TD$tourn_games_played == 4)] = "Elite Eight"
# Teams that played 5 games
TD$tourncat[(TD$tourn_games_played == 5)] = "Final Four"
# Teams that played 6 games
TD$tourncat[(TD$tourn_games_played == 6)] = "Championship"

# Make a vector of all variables used in multivariate analysis and the interpretations
#   (visually divided into the following categories: regular season team statistics, 
#   regular season opponent statistics, regular season composite statistics,
#   general tournament statistics, tournament team statistics, 
#   tournament opponent statistics)
vars <- c("name", "conference", "conference_filtered", "conference_type", "conference_type_num",
          
          "points", "rebounds", "assists", "steals", "blocks", "turnovers",
          "blocked_att", "personal_fouls", "flagrant_fouls",
          "efficiency", "true_shooting_pct","fast_break_pts", "points_off_turnovers", 
          "points_in_paint", "second_chance_pts", 
          
          "opp_points", "opp_rebounds", "opp_assists", "opp_turnovers",
          "opp_blocked_att", "opp_personal_fouls", "opp_flagrant_fouls",
          "opp_true_shooting_pct", "opp_fast_break_pts", "opp_points_off_turnovers",
          "opp_points_in_paint", "opp_second_chance_pts",
          
          "awp", "owp", "sos", "rpi",
          
          "seed", "seed_type", "tourn_bid", "tourn_bid_num", "tourncat",
          "tourn_games_played", "tourn_games_played_num", "log_tourn_games_played",
          "tourn_point_diff",
          
          "tourn_points", "tourn_assists",  
          "tourn_fast_break_pts", "tourn_second_chance_pts",
          "tourn_personal_fouls", "tourn_flagrant_fouls", "tourn_blocked_att",
          
          "tourn_opp_efficiency", "tourn_opp_field_goal_pct", "tourn_opp_points", 
          "tourn_opp_assists", "tourn_opp_points_in_paint", 
          "tourn_opp_blocked_att", "tourn_opp_personal_fouls", "tourn_opp_flagrant_fouls")

# Get a subset of the data based on the chosen variables
TD.all <- TD[, vars]

# Remove any teams with incomplete data (some of the older tournament teams lack 
#   more composite statistics)
TD.all <- TD.all[complete.cases(TD.all), ]

# Output the number of rows and columns
dim(TD.all)

# List the variables chosen for factor analysis
names(TD.all)

# Make a vector of all numerical variables used in multivariate analysis and the interpretations
numvars <- c("points", "rebounds", "assists", "steals", "blocks", "turnovers",
             "blocked_att", "personal_fouls", "flagrant_fouls",
             "efficiency", "true_shooting_pct","fast_break_pts", "points_off_turnovers", 
             "points_in_paint", "second_chance_pts", 
             
             "opp_points", "opp_rebounds", "opp_assists", "opp_turnovers",
             "opp_blocked_att", "opp_personal_fouls", "opp_flagrant_fouls",
             "opp_true_shooting_pct", "opp_fast_break_pts", "opp_points_off_turnovers",
             "opp_points_in_paint", "opp_second_chance_pts",
             
             "awp", "owp", "sos", "rpi",
             
             "log_tourn_games_played", "tourn_point_diff",
             
             "tourn_points", "tourn_assists",  
             "tourn_fast_break_pts", "tourn_second_chance_pts",
             "tourn_personal_fouls", "tourn_flagrant_fouls", "tourn_blocked_att",
             
             "tourn_opp_efficiency", "tourn_opp_field_goal_pct", "tourn_opp_points", 
             "tourn_opp_assists", "tourn_opp_points_in_paint", 
             "tourn_opp_blocked_att", "tourn_opp_personal_fouls", "tourn_opp_flagrant_fouls")