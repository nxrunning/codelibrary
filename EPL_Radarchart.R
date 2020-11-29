# Import libraries
library(understatr)
library(ggradar)
library(scales)
library(tidyverse)
library(showtext)

# Extract english premier league 19/20 season data
epl_data <- get_league_teams_stats("EPL", year = 2019)


# Compute measure of pressing intensity
epl_data <- epl_data %>%
  mutate(ppda = ppda.att/ppda.def)

# Summarising average data for each team over entire season
summary_data <- epl_data %>%
  select(npxG, deep, ppda, team_name) %>%
  group_by(team_name) %>%
  summarise(npxG = mean(npxG),
            deep = mean(deep),
            ppda = mean(ppda))%>%
  rename(group = team_name) %>%
  mutate_at(vars(-group),
            rescale)

# Renaming teams to make names shorter
summary_data$group <- recode(summary_data$group, 'Wolverhampton Wanderers' = "Wolverhampton")
summary_data$group <- recode(summary_data$group, 'Manchester City' = "Man. City")
summary_data$group <- recode(summary_data$group, 'Manchester United' = "Man. United")
summary_data$group <- recode(summary_data$group, 'Sheffield United' = "Sheffield")
summary_data$group <- recode(summary_data$group, 'Newcastle United' = "Newcastle")

# Reorder the factor levels according to EPL table
summary_data$group <- factor(summary_data$group, 
                          levels = c("Liverpool", "Man. City", "Chelsea","Leicester","Man. United",
                                     "Wolverhampton","Arsenal","Tottenham","Sheffield", "Burnley",
                                     "Everton","Crystal Palace", "Newcastle","Watford","West Ham",
                                     "Southampton","Aston Villa","Brighton","Bournemouth","Norwich"))

# Choose nicer looking fonts
font_add_google("Alatsi", "Alatsi")
font_add_google("Open Sans", "Sans")
showtext_auto()

#### Radar Chart Visualisation ####

# Compare between Liverpool vs Newcastle
comparison_livnew <- summary_data %>%
  filter(group %in% c("Liverpool", "Newcastle"))%>%
  select(group, deep, ppda, npxG)%>%
  mutate(group = as.character(group))%>%
  ggradar(grid.label.size = 10,
          axis.label.size = 10, 
          group.point.size = 2,
          group.line.width = 0.5,
          background.circle.colour = "gray70",
          gridline.max.colour = "black",
          gridline.mid.colour = "red",
          gridline.min.colour = "darkgoldenrod1",
          gridline.label.offset = 0.35,
          axis.label.offset = 1.2,
          group.colours = c("#FC766AFF", "darkgreen"))+
  labs(title = "A tale of two teams",
       subtitle = "Comparison of match statistics between Liverpool and Newcastle United",
       caption = "Visualisation: @nxrunning")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 35, family = "Alatsi", face = "bold"),
        plot.subtitle = element_text(color="grey20", family = "Sans", size=20),
        plot.caption = element_text(size = 20, hjust = 1),
        legend.text = element_text(size = 30),
        legend.box.margin = margin(-1.5, 0, 0, 0, "cm"))

# Compare between Liverpool vs Manchester City
comparison_livmanc<-summary_data %>%
  filter(group %in% c("Liverpool", "Man. City"))%>%
  select(group, deep, ppda, npxG)%>%
  mutate(group = as.character(group))%>%
  ggradar( 
    grid.label.size = 10,
    axis.label.size = 10, 
    group.point.size = 2,
    group.line.width = 0.5,
    background.circle.colour = "gray70",
    gridline.max.colour = "black",
    gridline.mid.colour = "red",
    gridline.min.colour = "darkgoldenrod1",
    gridline.label.offset = 0.35,
    axis.label.offset = 1.2,
    group.colours = c("#FC766AFF", "#5B84B1FF"))+
  labs(title = "Similar yet different",
       subtitle = "Comparison of match statistics between Liverpool and Manchester City",
       caption = "Visualisation: @nxrunning")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 35, family = "Alatsi", face = "bold"),
        plot.subtitle = element_text(color="grey20", family = "Sans", size=20),
        plot.caption = element_text(size = 20, hjust = 1),
        legend.text = element_text(size = 30),
        legend.box.margin = margin(-1.5, 0, 0, 0, "cm"))

# Radar chart for every team in EPL
epl_radar<-ggradar(summary_data, 
        grid.label.size = 1,
        axis.label.size = 5, 
        group.point.size = 1,
        group.line.width = 0.5,
        background.circle.colour = "gray70",
        gridline.max.colour = "black",
        gridline.mid.colour = "red",
        gridline.min.colour = "darkgoldenrod1",
        group.colours = "darkblue")+
  facet_wrap(~group, ncol = 5)+
  labs(title = "EPL teams' playing styles",
       subtitle = "Defined using three match statistics: PPDA, npxG, and deep",
       caption = "Visualisation: @nxrunning")+
  theme(plot.background = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill="darkblue", colour="black", size=0.5),
        strip.text.x = element_text(size= 22, colour="white", margin = margin(0.1, 0, 0, 0, "cm")),
        plot.title = element_text(color="grey20", family = "Alatsi", face = "bold", size=35),
        plot.subtitle = element_text(color="grey20", family = "Sans", size=20),
        plot.caption = element_text(color = "darkblue", size = 20),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.1,0,0,0), "cm"))
