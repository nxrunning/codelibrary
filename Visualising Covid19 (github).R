#Import libraries
library(ggplot2) #For plotting static graph
library(gganimate) #For animating ggplot
library(gifski) #For rendering the animation

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
                  "2020-02-01":"2020-03-23")

#Converting date column to date format
covid19$Date <- as.Date(covid19$Date)

#### Visualising covid-19 in China ####

# Subsetting dataset
covid19_china <- covid19[covid19$Country == "China", ]

#Graph
China_plot <- ggplot(data = covid19_china, aes(x = Date, y = Cases))+
  geom_line(colour = "red3")+
  geom_point()+
  labs(title = "COVID-19 across China",
       y = "Number of\nconfirmed cases",
       caption = "Data retrieved from World Health Organization")+
  theme_minimal()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic"))+
  geom_text(aes(label = round(Cases, 0), vjust = -1))


#Preview the graph
China_plot

#Animate the graph 
China_animate <- China_plot + transition_reveal(Date)+
  labs(subtitle = "Date: {frame_along}")+
  theme(plot.subtitle = element_text(size=10, colour = "navy"))

#Save gif
animate(China_animate, nframes = 120, fps = 5, duration = 20, 
        renderer = gifski_renderer("covid19_china.gif"))

#### Southeast Asian Graph ####

# Subsetting dataset
covid19_sea <- covid19[covid19$Region == "Southeast Asia", ]

# Graph
SEA_plot <- ggplot(data = covid19_sea, aes(x = Date, y = Cases, colour = Country))+
  geom_line()+
  geom_point()+
  scale_color_brewer(palette = "Set1")+
  labs(title = "COVID-19 across Southeast Asia",
       y = "Number of\nconfirmed cases",
       caption = "Data retrieved from World Health Organization")+
  theme_minimal()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic"))

#Preview the graph
SEA_plot

#Animate the graph 
SEA_animate <- SEA_plot + transition_reveal(Date)+
  labs(subtitle = "Date: {frame_along}")+
  theme(plot.subtitle = element_text(size=10, colour = "navy"))

#Save the gif
animate(SEA_animate, nframes = 120, duration = 20, fps = 5, 
        renderer = gifski_renderer("covid19_sea.gif"))

#### Other notable countries ####

# Subsetting dataset
covid19_others <- covid19[covid19$Country %in% c("Japan", "South Korea", "France", "Germany", "Spain",
                                                 "Italy", "United Kingdom", "United States", 
                                                 "Switzerland"), ]


# Graph
Others_plot <- ggplot(data = covid19_others, aes(x = Date, y = Cases, colour = Country))+
  geom_line()+
  geom_point()+
  scale_color_brewer(palette = "Dark2")+
  labs(title = "COVID-19 across the World",
       y = "Number of\nconfirmed cases",
       caption = "Data retrieved from World Health Organization")+
  theme_minimal()+
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic"))


#Preview the graph
Others_plot

#Animate the graph 
Others_animate <- Others_plot + transition_reveal(Date)+
  labs(subtitle = "Date: {frame_along}")+
  theme(plot.subtitle = element_text(size=10, colour = "navy"))

#Save the gif
animate(Others_animate, nframes = 120, duration = 20, fps =5,
        renderer = gifski_renderer("covid19_others.gif"))