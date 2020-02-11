#Importing Libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(forcats)

#Importing data
raw_data <- read.csv("Epl1920.csv")


#Ridgeline plot
raw_data %>%
  mutate(Team = fct_reorder(Team, Possession, .fun ='mean')) %>%
  ggplot( 
       aes(x = Possession, 
           y = reorder(Team, Possession), 
           fill = Team)) +
  geom_density_ridges(alpha = 0.5) + 
  theme_ridges() +
  labs(title = "EPL 19/20 Possession Distribution",
       y = "Teams",
       caption = "Data retrieved from premierleague.com") +
  theme_minimal()+
  theme(legend.position = "none")+
  theme(plot.title=element_text(face="bold"))
  ggsave(dpi = 300, filename = "ridgeline.jpeg")
