# Required libraries
library(reactable)
library(reactablefmtr)
library(tidyverse)
library(scales)

# Data
wc_matches <- read.csv("wcmatches.csv")
wc22_teams <- read.csv("wc22teams.csv")
country_flags <- read.csv("country_flags_dataset.csv")

####----Data Wrangling----####

# Modify dataframe into long format to see all participating teams in one column
wc_matches_v2 <- wc_matches%>%
  pivot_longer(cols = c(home_team, away_team),
               names_to = "Home_away",values_to = "Team")

# Some countries require renaming
wc_matches_v2[(wc_matches_v2$Team == "West Germany"), c("Team")] <- "Germany"
wc_matches_v2[(!is.na(wc_matches_v2$winning_team) & wc_matches_v2$winning_team == "West Germany"), c("winning_team")] <- "Germany"

# Summarising counts for each country's appearance in different stages of the competition

# Round of 16s
df_round16 <- wc_matches_v2|>
  filter(stage == "Round of 16")|>
  group_by(Team)|>
  count() |>
  rename("Round_16" = "n")
# Quarter-finals
df_quarter <- wc_matches_v2|>
  filter(stage == "Quarterfinals")|>
  group_by(Team)|>
  count()|>
  rename("Quarter_finals" = "n")
# Semi-finals
df_semi <- wc_matches_v2|>
  filter(stage == "Semifinals")|>
  group_by(Team)|>
  count()|>
  rename("Semi_finals" = "n")
# Finals
df_final <- wc_matches_v2|>
  filter(stage == "Final")|>
  group_by(Team)|>
  count()|>
  rename("Finals" = "n")
# World cup wins
df_wc_wins <- wc_matches_v2|>
  filter(stage == "Final")|>
  group_by(winning_team)|>
  count()|>
  mutate(n = n/2)

# The finals of 1950 competition was played in a round robin format instead and Uruguay was the winner thus adding one count for Uruguay
df_wc_wins_v2<-df_wc_wins|>
  mutate(n = ifelse(winning_team == "Uruguay", n+1, n))|>
  rename("Wins" = "n",
         "Team" = "winning_team")

# Join all dataframes
merged_results_df<-df_round16|>
  left_join(df_quarter)|>
  left_join(df_semi)|>
  left_join(df_final)|>
  left_join(df_wc_wins_v2)|>
  replace_na(list("Quarter_finals" = 0,
                  "Semi_finals" = 0,
                  "Finals" = 0,
                  "Wins" = 0))

# Processing of flag data for joining
country_flags_v2 <- country_flags|>
  rename("Team" = "Country")

# Join the data for teams participating in the Fifa world cup 22
# First filter the teams that are participating in the Fifa world cup 22
filtered_df<-merged_results_df|>
  semi_join(wc22_teams, by = "Team")
# Finally, use left join to merge with the world cup 22 team dataset
working_data <- wc22_teams|>
  left_join(filtered_df)|>
  left_join(country_flags_v2)|>
  replace_na(list("Round_16" = 0,
                  "Quarter_finals" = 0,
                  "Semi_finals" = 0,
                  "Finals" = 0,
                  "Wins" = 0))|>
  select("ImageURL", "Team", "Fifa_rank", "Ranking_points", 
         "Round_16","Quarter_finals","Semi_finals", 
         "Finals", "Wins")|>
  mutate(Ranking_points = round(Ranking_points, 2))

####----Add colour palette----####

# Colour scale function by Greg Lin
# Higher bias values generate more widely spaced colours at the high end
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

# Selected colour palette. You may customise your own palette by specifying the colours.
good_color <- make_color_pal(c("#ffffff", "#cfe3f3", "#87bbe1", "#579fd5", "#1077c3"))

# display the colours
# Generating a sequence of numbers between 0 and 1 to create the colour sequence
# You may specify the number of sequences you need
seq(0.1, 0.9, length.out = 6) |> 
  good_color() |> 
  show_col()

####----Create Table----####

table_v1<-reactable(working_data,
          # Sorting our table based on the FIFA ranking by default
          defaultSorted = "Fifa_rank",
          # Defining vertical and horizontal alignment of each column cell to be center by default 
          defaultColDef = colDef(
            vAlign = "center",
            align = "center"
          ),
          columns = list(
            # Embedding country flag images into the first column. You may specify the height and width of the cell.
            ImageURL = colDef(cell = embed_img(working_data,width = 45, height = 40,
                                               horizontal_align = "center"), name = ""),
            Team = colDef(name = "Team",
                          width = 105),
            Fifa_rank = colDef(name = "FIFA Ranking"),
            # Visualising ranking points using bar charts.
            Ranking_points = colDef(name = "FIFA Ranking Points",
                                    defaultSortOrder = "desc",
                                    align = "left",
                                    width = 180,
                                    cell = data_bars(working_data,
                                                     fill_color = "#FEC310",
                                                     bar_height = 10,
                                                     text_position = "outside-base",
                                                     text_size = 15,
                                                     background = "#e1e1e1")),
            Round_16 = colDef(name = "Round of 16",
                              style = function(value){
                                value
                                normalised <- (value-min(working_data$Round_16))/(max(working_data$Round_16)--min(working_data$Round_16))
                                color <- good_color(normalised)
                                list(background = color)}),
            Quarter_finals = colDef(
              name = "Quarter-Final",
              style = function(value){
                value
                normalised <- (value-min(working_data$Quarter_finals))/(max(working_data$Quarter_finals)--min(working_data$Quarter_finals))
                color <- good_color(normalised)
                list(background = color)}),
            Semi_finals = colDef(
              name = "Semi-Final",
              style = function(value){
                value
                normalised <- (value-min(working_data$Semi_finals))/(max(working_data$Semi_finals)--min(working_data$Semi_finals))
                color <- good_color(normalised)
                list(background = color)}),
            Finals = colDef(
              name = "Final",
              style = function(value){
                value
                normalised <- (value-min(working_data$Finals))/(max(working_data$Finals)--min(working_data$Finals))
                color <- good_color(normalised)
                list(background = color)}),
            Wins = colDef(
              name = "Champion",
              style = function(value){
                value
                normalised <- (value-min(working_data$Wins))/(max(working_data$Wins)--min(working_data$Wins))
                color <- good_color(normalised)
                list(background = color)})
          ))

# Add Title, subtitle and caption

table_v2 <- table_v1|>
  # Add title of table
  add_title("FIFA World Cup 2022",
            font_color = "#1077C3")|>
  # Add subtitle of table
  add_subtitle("History of participating teams in the knockout stages of the World Cup competition",
               font_weight = "normal",
               margin = c(5,0,10,0))|>
  # Add caption of table
  add_source("Note: Data of knockout stages excluded year 1950 competition as the finals were played in a round robin format instead",
             font_style = "italic",
             font_size = 14)|>
  add_source("DATA: KAGGLE| TABLE: TOU NIEN XIANG | NIENXIANGTOU.COM",
             font_color = "#C8C8C8",
             margin = c(5,0,0,0))

# Export table as html file
save_reactable_test(table_v2, "table_FIFA22.html")
