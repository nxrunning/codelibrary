#Clear environment
rm(list = ls())

#Set working directory
setwd("~/Machine Learning/Personal Projects/Covid19 Animation Line Graph")

#Import libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyr)
library(png)
library(grid)
library(ggimage)

#Import data
raw_data <- read.csv("Covid19.csv", header = TRUE)

#Tidying column name
names(raw_data)[1] <- "Country"

#Converting column names to date format
for (i in 3:length(names(raw_data))) {
  names(raw_data)[i] <- gsub("X", "", names(raw_data)[i])
  names(raw_data)[i] <- gsub("\\.", "-", names(raw_data)[i])
}

#Coverting from wide to long format
covid19 <- gather(raw_data, 
                  key = "Date",
                  value = "Cases",
                  "2020-02-01":"2020-03-20")

#Converting date column to date format
covid19$Date <- as.Date(covid19$Date)


#Preparing image to add personal twitter handle
img <- readPNG("twitter.png")
g <- rasterGrob(img, interpolate=TRUE)


#### Southeast Asian Graph ####

# Subsetting dataset
covid19_sea <- covid19[covid19$Region == "Southeast Asia", ]

# Graph
SEA_plot <- ggplot(data = covid19_sea, aes(x = Date, y = Cases, colour = Country))+
  geom_line()+
  geom_point()+
  scale_color_brewer(palette = "Set1")+
  labs(title = "COVID-19 across Southeast Asia",
       subtitle = "1st February 2020 - 20th March 2020",
       y = "Number of\nconfirmed cases",
       caption = "Data retrieved from World Health Organization")+
  theme_minimal()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic"))+
  annotation_custom(grob = g, xmin = as.Date("2020-01-11"), xmax = as.Date("2020-02-20"),
                    ymin = max(covid19_sea$Cases) -50, ymax = max(covid19_sea$Cases))+
  annotate("text", x = c(as.Date("2020-02-07")), y = c(max(covid19_sea$Cases)-25), 
           label = c("@nxrunning"))

#Animate the graph 
SEA_animate <- SEA_plot + transition_reveal(Date)
animate(SEA_animate, nframes = 120, duration = 20, renderer = gifski_renderer("covid19_sea.gif"))
  
#### European countries ####

# Subsetting dataset
covid19_europe <- covid19[covid19$Region == "Europe", ]

# Graph
Europe_plot <- ggplot(data = covid19_europe, aes(x = Date, y = Cases, colour = Country))+
  geom_line()+
  geom_point()+
  scale_color_brewer(palette = "Dark2")+
  labs(title = "COVID-19 across Europe",
       subtitle = "1st February 2020 - 20th March 2020",
       y = "Number of\nconfirmed cases",
       caption = "Data retrieved from World Health Organization")+
  theme_minimal()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic"))+
  annotation_custom(grob = g, xmin = as.Date("2020-01-11"), xmax = as.Date("2020-02-20"),
                    ymin = max(covid19_europe$Cases)-4000, ymax = max(covid19_europe$Cases))+
  annotate("text", x = c(as.Date("2020-02-08")), y = c(max(covid19_europe$Cases)-2000), 
           label = c("@nxrunning"))


#Preview the graph
Europe_plot

#Animate the graph 
Europe_animate <- Europe_plot + transition_reveal(Date)
animate(Europe_animate, nframes = 120, duration = 20, renderer = gifski_renderer("covid19_europe.gif"))





graph <- ggplot(data = covid19_sea, aes(x = Date, y = Cases, colour = Country))+
  geom_line()+
  geom_point()+
  scale_color_brewer(palette = "Set1")+
  labs(title = "COVID-19 across Southeast Asia",
       subtitle = "1st February 2020 - 18th March 2020",
       y = "Number of\nconfirmed cases",
       caption = "Data retrieved from World Health Organization")+
  theme_minimal()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic"))+
  xlim(as.Date("2020-02-01"), as.Date("2020-03-20"))+
  annotation_custom(grob = g, xmin = as.Date("2020-01-11"), xmax = as.Date("2020-02-20"),
                    ymin = 500, ymax = 550)+
  annotate("text", x = c(as.Date("2020-02-07")), y = c(525), label = c("@nxrunning"))
  
  
# Preview graph
graph

#Animate the graph 
graph_animate <- graph + transition_reveal(Date)
animate(graph_animate, nframes = 120, duration = 20, renderer = gifski_renderer("covid19.gif"))




annotation_custom(grob = g, -Inf, Inf, -Inf, Inf)

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
  ) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
gg_animate(p, interval = 0.5)


anim<- p + transition_time(year) +
  labs(title = "Year: {frame_time}")

animate(anim, nframes = 120, length = 20, renderer = gifski_renderer("test.gif"))

anim_save("test.png", animation = last_animation())