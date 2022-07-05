library(tidyverse)
library(gganimate)
library(png)
library(gifski)

# You may have to include the following lines if you have not initialized custom fonts in r
## library(extrafont) 
## font_import()
## loadfonts(device = "win")


####### Data Wrangling #################

# Reading in the Data
ph_temp <- read_csv("PH Weather Data.csv", skip = 1)

## data source: https://climateknowledgeportal.worldbank.org/download-data

# Changing column names
temp_cols <- c("year", "philippines", "reg_car", "reg_ncr", "reg_1", "reg_2", "reg_5",  "reg_6",  "reg_7",  "reg_8",
               "reg_13",  "reg_armm",  "reg_9",  "reg_10",  "reg_11",  "reg_12",  "reg_3",  "reg_4a",  "reg_4")

colnames(ph_temp) <- temp_cols

# Transforming regional data to tidyformat (columns to: region, year, temperature)
ph_temp <- ph_temp %>%
  pivot_longer(cols = 3:19, 
               names_to = "region", values_to = "temp",  names_prefix = "reg_") %>%
  mutate(
    island = factor(case_when(
    region %in% c("1", "2", "3", "4", "4a", "ncr", "car", "5") ~ "Luzon",
    region %in% c("6", "7", "8") ~ "Visayas",
    TRUE ~ "Mindanao")), # Classifying regions to whether they belong to Luzon, Visayas, or Mindanao
    region = factor(region, levels = c("ncr", "car", "1", "2", "3", "4", "4a", "5",
                                       "6", "7", "8", "9","10", "11", "12", "13", "armm")) #Ordering the regions
    )

# Data aggregation
## first_temp - gives the mean temperature of that region in 1910
## diff - gives the difference in the mean temperature of a given year and first_temp


ph_temp <- ph_temp %>%
  filter(year == min(year)) %>%
  mutate(first_temp = temp) %>%
  select(region, first_temp) %>%
  right_join(., ph_temp, by = "region") %>%
  rowwise() %>%
  mutate(diff = temp - first_temp,
         year = year)

# Setting the color palette 
red_blue <- c("#1D1CC9", "#ffffff", "#B00B69")

##### Graphing ###########

# Making the Circular Bar Graph

circle_static <- ggplot(ph_temp, aes(x = region, y=diff)) +   
  geom_bar(stat="identity", aes(fill = diff)) +
  ylim(-2,2.5) +
  # Cleaning up the graph
  theme_minimal() + #White background
  theme(
    axis.text.x = element_blank(), #No x-axis label
    axis.text.y = element_blank(), #No y-axis label
    axis.title = element_blank(),  #No axes titles
    panel.grid = element_blank(), #No grid lines
    plot.title = element_text(family = "Open Sans", face = "bold", size = 20, hjust = 0.7), #custom font and font sizes
    legend.title = element_text(family = "Arial", size = 10), #custom font and font sizes
    plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm") #plot margin sizes
  ) +
  geom_text(data = ph_temp, aes(y = 1.9, label = region), family = "Open Sans", size = 5) + #region labels
  scale_fill_gradientn(colors = red_blue) + #bar colors using color palette
  coord_polar(start = 0) + #make the bar graph into a circle graph
  geom_text(aes(x = 0, y = -2, label = year), family = "Hussar Nova", size = 5) +  #year in center of circle
  labs(title = "How has the average temperature in the Philippines \nchanged in the last 100 years?",
       fill = "Difference with \nAve. Temp. in 1910") #plot and legend titles

## If at this point, your static (non-animated) plot does not look like the final plot, don't worry.
## This is because all frames are stacked or merged into one.
## Animating it will change its form.


# Animating the Circular Bar Graph

circle_anim <- circle_static + transition_states(year) #animate the plot over years

#save the animated plot
circle_temp <- animate(circle_anim, fps = 10, duration = 32, 
                       end_pause = 50, #put a pause before gif looping
                       height = 800, width = 800, #set plot size
                       renderer = gifski_renderer("PH Temp by Region.gif")) #export plot


# Creating the Line Graph

ph_ave_years <- ph_temp %>% 
  ungroup() %>% #since data aggregation made our data into rowwise format
  head(., 120) %>% #pivot_longer duplicated country-level data, so this removes the duplicates
  ggplot(., aes(x = year, y = philippines)) + 
  geom_path() + #line graph 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), #remove grid background
    plot.title = element_text(family = "Open Sans", face = "bold"),
    text = element_text(family = "Open Sans") #use custom font
  ) +
  labs(x = "Year", y = "Temperature (C)", title = 
         "Overall Average Temperature in the Philippines") + #plot labels and titles
  scale_x_continuous(breaks = seq(1910, 2020, by = 20)) #change x axis breaks


# Animating the Line Graph

animate_years <- ph_ave_years + transition_reveal(year) #animate the line graph

time_temp <- animate(animate_years, #save and export the line graph
        width = 800, height = 200, 
        fps = 10, duration = 32,
        end_pause = 50, 
        renderer = gifski_renderer("PH Ave Over Time.gif"))


