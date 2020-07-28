library(tidyverse)
library(ggtext)

# Import data
raw_female <- read.csv("complete sgp female runners splits.csv")
raw_male <- read.csv("complete sgp male runners splits.csv")

# Assign Gender values to respective dataframes
raw_female <- mutate(raw_female, Gender = "Female")
raw_male <- mutate(raw_male, Gender = "Male")

# Creating index column for purpose of dividing the participants to the respective groups later
raw_female$Index <- seq(1, nrow(raw_female))
raw_male$Index <- seq(1, nrow(raw_male))

# Combine dataframes into one
raw_combined <- rbind(raw_female, raw_male)

# Compute 40km Net Time and adds to dataframe
raw_combined <- mutate(raw_combined,
                       km40_net_time = (Speed_5km*5) + (Speed_10km*5) + (Speed_15km*5) + (Speed_20km*5) +
                         (Speed_25km*5) + (Speed_30km*5) + (Speed_35km*5) + (Speed_40km*5))

# Converting Net time column to numeric
raw_combined$Net.Time <- as.character(raw_combined$Net.Time)
raw_combined$Net.Time <- gsub(":", "", raw_combined$Net.Time)

# Convert Net time to seconds
raw_combined$Conv_net_time <- NA
for (i in 1:nrow(raw_combined)){
  temp_list <- unlist(strsplit(raw_combined[i, "Net.Time"], ""))
  hour <- as.integer(temp_list[2])
  minutes <- as.integer(paste(temp_list[3], temp_list[4], sep =""))
  seconds <- as.integer(paste(temp_list[5], temp_list[6], sep =""))
  total_time <- hour*3600 + minutes*60 + seconds
  raw_combined$Conv_net_time[i] <- total_time
}

# Compute average speed for last section
raw_combined<-mutate(raw_combined,
                     Speed_end = (Conv_net_time - km40_net_time)/2.195)

# Compute change in speed between 40km and last section
# Positive value indicates speeding up during the last section
raw_combined<-mutate(raw_combined,
                     Speed_change = round(Speed_40km - Speed_end, 2))

# Remove missing data and compute speeds in km/hr
plot_data <- select(raw_combined, Speed_5km, Speed_10km, Speed_15km, Speed_20km, Speed_25km,
                    Speed_30km, Speed_35km, Speed_40km, Speed_end, Speed_change, Gender, Index) %>%
  na.omit(raw_combined) %>%
  mutate(Speed_5km_kmhr = 3600/Speed_5km,
         Speed_10km_kmhr = 3600/Speed_10km,
         Speed_15km_kmhr = 3600/Speed_15km,
         Speed_20km_kmhr = 3600/Speed_20km,
         Speed_25km_kmhr = 3600/Speed_25km,
         Speed_30km_kmhr = 3600/Speed_30km,
         Speed_35km_kmhr = 3600/Speed_35km,
         Speed_40km_kmhr = 3600/Speed_40km,
         Speed_end_kmhr = 3600/Speed_end,
         Speed_change_kmhr = Speed_end_kmhr - Speed_40km_kmhr)

# Making Gender column a factor
plot_data$Gender <- factor(plot_data$Gender)

#### Boxplot visualisation of change in speed by gender groups ####
ggplot(plot_data, aes(x = Gender, y = Speed_change_kmhr))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = quantile(plot_data$Speed_change_kmhr, c(0.01, 0.98)), 
                     breaks =c(seq(-2, 3, 1)))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue4")+
  labs(title = "<b><span style = 'color:white;'>Do runners speed up during the end of a marathon?</b><br>
       <span style = 'color:white; font-size:8pt'><span style = 'color:yellow;'>88%</span> of female runners and <span style = 'color:yellow;'>86%</span> of male runners increased their speed 
       during the last 2.195 km<br>as compared to their 40-km time split during a marathon</span>",
       y = "Change in average speed (km/h)",
       x = "",
       caption = "Data from Standard Chartered Singapore Marathon 2019")+
  theme_minimal()+
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 15,
                                            lineheight = 1,
                                            padding = margin(5.5, 5.5, 5.5, 5.5),
                                            margin = margin(0, 0, 5.5, 0),
                                            fill = "midnightblue"))

# Computing range of boxplot whiskers for respective gender groups
boxplot_range<-plot_data %>%
  group_by(Gender) %>%
  summarise(lower_whisker=quantile(Speed_change_kmhr, 0.25)-IQR(Speed_change_kmhr)*1.5,
            upper_whisker=quantile(Speed_change_kmhr, 0.75)+IQR(Speed_change_kmhr)*1.5)


#### Eliminating outlier data ####

# Compute respective female and male data excluding outliers
non_outlier_female <- plot_data %>%
  filter(Gender == "Female") %>%
  filter(between(Speed_change_kmhr,boxplot_range$lower_whisker[1], boxplot_range$upper_whisker[1]))

non_outlier_male <- plot_data %>%
  filter(Gender == "Male") %>%
  filter(between(Speed_change_kmhr,boxplot_range$lower_whisker[2], boxplot_range$upper_whisker[2]))

# How many females sped up?
non_outlier_female %>%
  select(Speed_change_kmhr) %>%
  filter(Speed_change_kmhr >0) %>%
  summarise(count = n(),
            pct = (count/nrow(non_outlier_female)*100))

# How many males sped up?
non_outlier_male %>%
  select(Speed_change_kmhr) %>%
  filter(Speed_change_kmhr >0) %>%
  summarise(count = n(),
            pct = (count/nrow(non_outlier_male)*100))

#### Dividing gender groups based on percentiles ####

# Male
total_male_cases <- nrow(raw_male)
percentile_20th_male_cases <- floor(0.2*total_male_cases)
percentile_40th_male_cases <- ceiling(0.4*total_male_cases)
percentile_60th_male_cases <- ceiling(0.6*total_male_cases)
percentile_80th_male_cases <- ceiling(0.8*total_male_cases)

# Female
total_female_cases <- nrow(raw_female)
percentile_20th_female_cases <- floor(0.2*total_female_cases)
percentile_40th_female_cases <- ceiling(0.4*total_female_cases)
percentile_60th_female_cases <- ceiling(0.6*total_female_cases)
percentile_80th_female_cases <- ceiling(0.8*total_female_cases)

# Assigning groups based on percentiles 
non_outlier_female<-mutate(non_outlier_female,
                           Group = ifelse(Index<= percentile_20th_female_cases, 1,
                                          ifelse(Index >percentile_40th_female_cases & Index < percentile_60th_female_cases, 2,
                                                 ifelse(Index > percentile_80th_female_cases, 3, 0))))

# Assigning groups based on percentiles 
non_outlier_male<-mutate(non_outlier_male,
                         Group = ifelse(Index<= percentile_20th_male_cases, 1,
                                        ifelse(Index >percentile_40th_male_cases & Index < percentile_60th_male_cases, 2,
                                               ifelse(Index > percentile_80th_male_cases, 3, 0))))
# Consolidate the working dataframe for analysis
combined_non_outlier <- rbind(non_outlier_female, non_outlier_male)
working_data <- combined_non_outlier %>%
  select(Index, Gender, Group, Speed_5km_kmhr, Speed_10km_kmhr, Speed_15km_kmhr, Speed_20km_kmhr,
         Speed_25km_kmhr, Speed_30km_kmhr, Speed_35km_kmhr, Speed_40km_kmhr, Speed_end_kmhr, 
         Speed_change_kmhr) %>%
  filter(Group != 0)

# Making Group column a factor
working_data$Group <- factor(working_data$Group, levels = c(1,2,3), labels = c("Fast", "Mid-Pack", "Slow"))

#### Visualising pacing profiles ####

long_data <-working_data %>%
  pivot_longer(c(Speed_5km_kmhr, Speed_10km_kmhr, Speed_15km_kmhr, Speed_20km_kmhr,
                 Speed_25km_kmhr, Speed_30km_kmhr, Speed_35km_kmhr, Speed_40km_kmhr, Speed_end_kmhr),
               names_to = "Distance", values_to = "Speed")%>%
  filter(between(Speed, 0, 20))


long_data$Distance<-factor(long_data$Distance, levels = c("Speed_5km_kmhr", "Speed_10km_kmhr", 
                                                          "Speed_15km_kmhr", "Speed_20km_kmhr",
                                                          "Speed_25km_kmhr", "Speed_30km_kmhr", 
                                                          "Speed_35km_kmhr", "Speed_40km_kmhr", 
                                                          "Speed_end_kmhr"))
# Mean Plots
long_data %>%
  group_by(Gender, Group, Distance) %>%
  summarise(n = n(),
            mean = mean(Speed),
            sd = sd(Speed),
            se = sd/sqrt(n),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n)) %>%
  ggplot(aes(x = Distance, y = mean, group = Group, color = Group))+
  geom_point(size = 3)+
  geom_line(linetype = "solid")+
  scale_x_discrete(labels = c("5\nkm","10\nkm", "15\nkm", "20\nkm", "25\nkm", "30\nkm",
                              "35\nkm","40\nkm", "End"))+
  labs(title = "<b>Pacing profiles of marathon runners</b><br>
       <span style = 'font-size:10pt;'>Time splits of different groups of runners, segregated by gender groups</span>",
       y = "Speed (km/h)",
       x = "",
       caption = "Data from Standard Chartered Singapore Marathon 2019")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()+
  facet_wrap(~Gender)+
  theme(
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = "blue4", box.color = "black",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    ))+
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 13,
                                            lineheight = 1,
                                            padding = margin(5.5, 5.5, 5.5, 5.5),
                                            margin = margin(0, 0, 5.5, 0),
                                            fill = "springgreen2"))

#### Visualising the end spurt across groups ####

# Compute change in speed in percentages
working_data <- working_data %>%
  mutate(Speed_change_pct = (Speed_change_kmhr / Speed_40km_kmhr)*100)

working_data %>%
  group_by(Group, Gender) %>%
  summarize(n= n(),
            mean = mean(Speed_change_pct),
            sd = sd(Speed_change_pct),
            se = sd/sqrt(n),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n)) %>%
  ggplot(aes(x = Group, y = mean, group = 1))+
  geom_point(size = 3, color = "Midnight blue")+
  geom_line(color = "black", linetype = "dashed")+
  geom_errorbar(aes(ymin = mean-ci,
                    ymax = mean+ci), width = 0.1)+
  geom_text(aes(label = round(mean, 2), hjust = -0.4, vjust = 0))+
  labs(title = "<b>Does the end spurt differ among runners?</b><br>
       <span style = 'font-size:10pt;'>In both gender groups, the slow group's increase in speed at the end of a marathon was significantly less as compared to the fast and mid-pack groups</span>",
       y = "Change in speed (%)",
       x = "",
       caption = "Data from Standard Chartered Singapore Marathon 2019")+
  theme_classic()+
  scale_color_brewer(palette = "Set1", name = "Gender")+
  facet_wrap(~Gender)+
  theme(
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = "blue4", box.color = "black",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    ))+
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 13,
                                            lineheight = 1,
                                            padding = margin(5.5, 5.5, 5.5, 5.5),
                                            margin = margin(0, 0, 5.5, 0),
                                            fill = "gold"))

#### Data analysis: One-way ANOVA ####
library(car)

# Separate dataframes based on gender groups for analysis
working_female <- working_data %>%
  filter(Gender == "Female")
working_male <- working_data %>%
  filter(Gender == "Male")


# Checking homogeneity of variance
# Both violated the assumption
leveneTest(working_female$Speed_change_pct, working_female$Group, center = mean)
leveneTest(working_male$Speed_change_pct, working_male$Group, center = mean)

# Since variance is not equal, we will use Welch's F instead
# Sig diff among groups in both genders
oneway.test(data = working_female, Speed_change_pct~Group)
oneway.test(data = working_male, Speed_change_pct~Group)

# Post-hoc pairwise comparison
# Only the slow group found to differ from the other two groups in both genders
pairwise.t.test(working_female$Speed_change_pct, working_female$Group,
                p.adjust.method = "bonferroni")
pairwise.t.test(working_male$Speed_change_pct, working_male$Group,
                p.adjust.method = "bonferroni")