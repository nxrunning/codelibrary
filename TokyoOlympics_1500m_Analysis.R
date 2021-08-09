# Import required libraries
library(tidyverse) # for data processing and visualisation
library(gt) # for computing tables
library(patchwork) # for merging different visualisations together

# Import data of personal records for calculation of critical speed and work capacity
raw_data <- read.csv("Tokyo_Oympics_1500mfinalists_personalrecords.csv")

# Process data to compute performance time in seconds and calculate the average speed
working_data<-raw_data%>%
  rename("Name" = "ï..Name")%>%
  separate(Time, into = c("Minute", "Seconds"), sep = ":")%>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "/")%>%
  mutate(Minute = as.numeric(Minute),
         Seconds = as.numeric(Seconds),
         Year = as.numeric(Year))%>%
  mutate(Time = (Minute*60)+Seconds)%>%
  mutate(Speed = Distance/Time)

#### Calculation of critical speed and work capacity of all athletes ####

# Create for loop to calculate D' and CS for each athlete
working_data<-working_data%>%
  filter(Year >= 2014)

athlete_list = as.vector(unique(working_data$Name))
CS_list = c()
workcapacity_list = c()

for (i in 1:length(athlete_list)){
  res <- lm(Distance~Time, data = working_data[working_data$Name == athlete_list[i], ])
  #y-intercept is the work capacity
  workcapacity_list[i] <- res$coefficients[1]
  #slope is the critical speed
  CS_list[i] <- res$coefficients[2]
}


# Put the linear regression results into a dataframe
critical_speed_results_df <- data.frame(
  Name = athlete_list,
  Critical_speed = CS_list,
  Work_capacity = workcapacity_list
)

#### Finals results data ####

# Read results data
raw_results_data <- read.csv("Tokyo_Olympics_1500mfinalists_results.csv")

# Processing of results data
results_df<-raw_results_data%>%
  rename("Name" = "ï..Name")%>%
  separate(Time_400, into = c("Minute_400", "Sec_400"), sep = ":")%>%
  separate(Time_800, into = c("Minute_800", "Sec_800"), sep = ":")%>%
  separate(Time_1200, into = c("Minute_1200", "Sec_1200"), sep = ":")%>%
  separate(Time_1400, into = c("Minute_1400", "Sec_1400"), sep = ":")%>%
  separate(Time_Finish, into = c("Minute_1500", "Sec_1500"), sep = ":")%>%
  mutate(Minute_400 = as.numeric(Minute_400),
         Sec_400 = as.numeric(Sec_400),
         Minute_800 = as.numeric(Minute_800),
         Sec_800 = as.numeric(Sec_800),
         Minute_1200 = as.numeric(Minute_1200),
         Sec_1200 = as.numeric(Sec_1200),
         Minute_1400 = as.numeric(Minute_1400),
         Sec_1400 = as.numeric(Sec_1400),
         Minute_1500 = as.numeric(Minute_1500),
         Sec_1500 = as.numeric(Sec_1500))%>%
  mutate(Time_sec_400 = (Minute_400*60)+Sec_400,
         Time_sec_800 = (Minute_800*60)+Sec_800,
         Time_sec_1200 = (Minute_1200*60)+Sec_1200,
         Time_sec_1400 = (Minute_1400*60)+Sec_1400,
         Time_sec_1500 = (Minute_1500*60)+Sec_1500)%>%
  mutate(Split_400 = Time_sec_400,
         Split_800 = Time_sec_800 - Time_sec_400,
         Split_1200 = Time_sec_1200 - Time_sec_800,
         Split_1400 = Time_sec_1400 - Time_sec_1200,
         Split_1500 = Time_sec_1500 - Time_sec_1400)%>%
  mutate(Speed_400 = 400/Split_400,
         Speed_800 = 400/Split_800,
         Speed_1200 = 400/Split_1200,
         Speed_1400 = 200/Split_1400,
         Speed_1500 = 100/Split_1500)

#### Compute descriptive table ####
descriptive_results <- results_df%>%
  select(Name, Time_sec_1500)

descriptive_table_df <- raw_results_data%>%
  rename("Name" = "ï..Name")%>%
  select(Name, Time_Finish, Gender)%>%
  right_join(critical_speed_results_df, by = "Name")%>%
  right_join(descriptive_results, by = "Name")%>%
  mutate(Average_Speed = round((1500/Time_sec_1500),2))%>%
  mutate(Percentage_CS = round((Average_Speed/Critical_speed)*100,2))%>%
  mutate(Name = as.character(Name))%>%
  select(-Time_sec_1500)

# Create Table
descriptive_table<-descriptive_table_df%>%
  select(Name, Gender, Critical_speed, Work_capacity, Time_Finish, Average_Speed, Percentage_CS)%>%
  mutate(Critical_speed = round(Critical_speed,2),
         Work_capacity = round(Work_capacity,2))%>%
  arrange(-Average_Speed)%>%
  rename("Result" = "Time_Finish",
         "Critical Speed" = "Critical_speed",
         "Work Capacity" = "Work_capacity",
         "Average Speed" = "Average_Speed",
         "Percentage of Critical Speed" = "Percentage_CS")%>%
  gt(rowname_col = "Name", groupname_col = "Gender")%>%
  tab_footnote(footnote = "CS and D' values are likely inaccurate due to only two personal best records available",
               locations = cells_stub(rows = c(17)))%>%
  tab_header(title = md("**Tokyo Olympics 1500m Finalists**"))%>%
  tab_source_note("Table: @nxrunning | Data: Tokyo Olympics 2020 & IAAF Database")


# Customising Table
descriptive_table<-descriptive_table%>%
  cols_label("Critical Speed" = html("Critical Speed<br>(m/s)"),
             "Work Capacity" = html("Work Capacity<br>(m)"),
             "Result" = html("Result"),
             "Average Speed" = html("Average Speed<br>(m/s)"),
             "Percentage of Critical Speed" = html("Percentage of<br>Critical Speed<br>(%)"))%>%
  cols_align(align = "center")%>%
  tab_style(style = list(
    cell_borders(sides = c("top","bottom"), color = "black", weight = px(3))),
    locations = list(cells_row_groups()))

# Save Table
descriptive_table%>%
  gtsave("Descriptivetable.html", inline_css = TRUE)

#### Data preparation for Visualisation of pacing ####

# Compute remaining D' after every lap
remaining_work_capacity_splits <- results_df%>%
  select(Name, Gender, Split_400, Split_800, Split_1200, Split_1400, Split_1500,
         Speed_400, Speed_800, Speed_1200, Speed_1400, Speed_1500)%>%
  left_join(critical_speed_results_df, by = "Name")%>%
  mutate(D_400 = Work_capacity - ((Speed_400 - Critical_speed)*Split_400))%>%
  mutate(D_800 = D_400 - ((Speed_800 - Critical_speed)*Split_800))%>%
  mutate(D_1200 = D_800 - ((Speed_1200 - Critical_speed)*Split_1200))%>%
  mutate(D_1400 = D_1200 - ((Speed_1400 - Critical_speed)*Split_1400))%>%
  mutate(D_1500 = D_1400 - ((Speed_1500 - Critical_speed)*Split_1500))%>%
  mutate(D_400_perc = round((D_400/Work_capacity*100),2),
         D_800_perc = round((D_800/Work_capacity*100),2),
         D_1200_perc = round((D_1200/Work_capacity*100),2),
         D_1400_perc = round((D_1400/Work_capacity*100),2),
         D_1500_perc = round((D_1500/Work_capacity*100),2))

work_capacity_plot_df <- remaining_work_capacity_splits%>%
  select(Name, Gender, Work_capacity, D_400, D_800, D_1200, D_1400, D_1500)%>%
  pivot_longer(cols = starts_with("D_"), names_to = "Splits", values_to = "D_remaining")%>%
  mutate(D_perc = round((D_remaining/Work_capacity)*100,2))


work_capacity_plot_df$Splits = factor(work_capacity_plot_df$Splits,
                                      levels = c("D_400", "D_800", "D_1200", 
                                                 "D_1400","D_1500"))

# Compute Lap times in percentages of Critical Speed
CS_plot_df<-results_df%>%
  select(Name, Gender, Speed_400, Speed_800, Speed_1200, Speed_1400, Speed_1500)%>%
  left_join(critical_speed_results_df, by = "Name")%>%
  pivot_longer(cols = starts_with("Speed"), names_to = "Splits", values_to = "Lap_Speed")%>%
  mutate(Speed_diff = Lap_Speed - Critical_speed)%>%
  mutate(Percentage_CS = round(((Speed_diff/Critical_speed)*100),2),
         Rate_of_capacity_diminishing = round((Speed_diff/Work_capacity)*100,2))

CS_plot_df$Splits = factor(CS_plot_df$Splits,
                           levels = c("Speed_400", "Speed_800", "Speed_1200", 
                                      "Speed_1400","Speed_1500"))

#### Visualisation of pacing ####

#### Women's race ####

# Visualising women lap times
women_lap_plot<-CS_plot_df%>%
  filter(Name %in% c("Faith Kipyegon", "Sifan Hassan", "Laura Muir", "Freweyni Gebreezibeher",
                     "Gabriela Debues-Stafford", "Linden Hall"))%>%
  ggplot(aes(x = Splits, y = Lap_Speed, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Lap Speed (m/s)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))


# Visualising women lap times in respect to critical speed
women_CS_plot<-CS_plot_df%>%
  filter(Name %in% c("Faith Kipyegon", "Sifan Hassan", "Laura Muir", "Freweyni Gebreezibeher",
                     "Gabriela Debues-Stafford", "Linden Hall"))%>%
  ggplot(aes(x = Splits, y = Percentage_CS, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Percentage of Speed\nabove Critical Speed(%)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))

# Visualising women lap times in respect to work capacity
women_D_plot<-work_capacity_plot_df%>%
  filter(Name %in% c("Faith Kipyegon", "Sifan Hassan", "Laura Muir", "Freweyni Gebreezibeher",
                     "Gabriela Debues-Stafford", "Linden Hall"))%>%
  ggplot(aes(x = Splits, y = D_remaining, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey")+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Remaining Work Capacity (m)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))

# Visualising women lap times in respect to work capacity
women_D_plot_perc <-work_capacity_plot_df%>%
  filter(Name %in% c("Faith Kipyegon", "Sifan Hassan", "Laura Muir", "Freweyni Gebreezibeher",
                     "Gabriela Debues-Stafford", "Linden Hall"))%>%
  ggplot(aes(x = Splits, y = D_perc, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey")+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Percentage of Work\nCapcity Remaining (%)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))

# Combine both plots
p1 <- (women_D_plot + women_D_plot_perc)/(women_lap_plot + women_CS_plot) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "Analysis of Tokyo Olympics Women's 1500m Finals",
                  caption = "Visualisation: @nxrunning") & 
  theme(legend.position = "bottom",
        plot.title = element_text(colour = "Navy", face = "bold", size = 20),
        plot.caption = element_text(size = 15),
        legend.text = element_text(size = 15))

# Save the plot 
ggsave(p1, dpi = 800, height = 250, width = 200, units = "mm", filename = "women_1500m_pacing.jpeg")

#### Men ####

# Visualising men lap times
men_lap_plot<-CS_plot_df%>%
  filter(Name %in% c("Jakob Ingebrigtsen", "Timothy Cheruiyot", "Josh Kerr", "Abel Kipsang",
                     "Adel Mechaal", "Cole Hocker"))%>%
  ggplot(aes(x = Splits, y = Lap_Speed, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Lap Speed (m/s)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))


# Visualising men lap times in respect to critical speed
men_CS_plot<-CS_plot_df%>%
  filter(Name %in% c("Jakob Ingebrigtsen", "Timothy Cheruiyot", "Josh Kerr", "Abel Kipsang",
                     "Adel Mechaal", "Cole Hocker"))%>%
  ggplot(aes(x = Splits, y = Percentage_CS, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Percentage of Speed\nabove Critical Speed(%)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))

# Visualising men lap times in respect to work capacity
men_D_plot<-work_capacity_plot_df%>%
  filter(Name %in% c("Jakob Ingebrigtsen", "Timothy Cheruiyot", "Josh Kerr", "Abel Kipsang",
                     "Adel Mechaal", "Cole Hocker"))%>%
  ggplot(aes(x = Splits, y = D_remaining, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey")+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Remaining Work Capacity (m)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))

# Visualising men lap times in respect to work capacity percentage
men_D_plot_perc <-work_capacity_plot_df%>%
  filter(Name %in% c("Jakob Ingebrigtsen", "Timothy Cheruiyot", "Josh Kerr", "Abel Kipsang",
                     "Adel Mechaal", "Cole Hocker"))%>%
  ggplot(aes(x = Splits, y = D_perc, colour = Name, group = Name))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey")+
  scale_color_brewer(palette = "Dark2", name = "")+
  scale_x_discrete(labels = c("400 m", "800 m", "1200 m", "1400 m", "Finish"))+
  labs(y = "Percentage of Work\nCapcity Remaining (%)",
       x = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 15))

# Combine both plots
p2 <- (men_D_plot + men_D_plot_perc)/(men_lap_plot + men_CS_plot) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "Analysis of Tokyo Olympics Men's 1500m Finals",
                  caption = "Visualisation: @nxrunning") & 
  theme(legend.position = "bottom",
        plot.title = element_text(colour = "Navy", face = "bold", size = 20),
        plot.caption = element_text(size = 15),
        legend.text = element_text(size = 15))

ggsave(p2, dpi = 800, height = 250, width = 200, units = "mm", filename = "men_1500m_pacing.jpeg")
