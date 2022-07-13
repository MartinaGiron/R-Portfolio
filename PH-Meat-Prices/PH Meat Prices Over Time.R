library(tidyverse) 
library(gganimate) #animated graphs
library(extrafont) #custom fonts
library(Cairo) #anti-alias on graphs
library(gridExtra) 

loadfonts(device = "win")


# data from https://data.humdata.org/dataset/wfp-food-prices-for-philippines/resource/9a842d72-0d7d-4922-ad0e-eb8106c1ab0e
# code reference: https://towardsdatascience.com/how-to-combine-animated-plots-in-r-734c6c952315

###### Data Wrangling ###############

food <- read_csv("PH Food Prices.csv",
                 col_types = "Dfffnnffffffnn",
                 skip = 2,
                 col_names = c("date", "region", "city", "market",
                               "lat", "lon", "category", "commodity", "unit", "price_flag",
                               "price_type", "currency", "price", "price_usd"))

# Select only food items included in our graphic

food_combined <- food %>%
  filter(commodity %in% c("Meat (pork)", "Meat (chicken, whole)", "Meat (beef, chops with bones)", "Fish (fresh)") |
          str_detect(commodity, "Fish"),
         price_type == "Retail",
         region == "National Capital region") %>%
  select(date, commodity, price) 

# In earlier years, all fish was encoded as "Fish (fresh)"
# But some time in 2019, the category was replaced with specific types of fish
# So we get the average of these for simplicity

food_fish_avg <- food_combined %>% # Getting the average price per date of all specific fish food items
  filter(str_detect(commodity, "Fish")) %>%
  filter(commodity != "Fish (fresh)") %>%
  group_by(date) %>%
  summarize(price = mean(price)) %>%
  mutate(commodity = "Fish (fresh)")


food_fish_fresh <- food_combined %>% # Getting all other meats and "Fish (fresh)"
  filter(commodity %in% c("Meat (pork)", "Meat (chicken, whole)", "Meat (beef, chops with bones)", "Fish (fresh)"))

graph_food <- rbind(food_fish_fresh, food_fish_avg) # Combining the two as our final dataset





graph_food <- graph_food %>%
  filter(date >= "2008-01-15") %>% # Filter these dates because before it, only data on pork prices were available
  mutate(commodity = case_when( # Renaming the food items for simplicity
           commodity == "Meat (pork)" ~ "Pork",
           commodity == "Meat (chicken, whole)" ~ "Chicken",
           commodity == "Meat (beef, chops with bones)"~ "Beef",
           commodity == "Fish (fresh)" ~ "Fish" 
         ))

graph_food <- graph_food %>%
  filter(date == as.Date("2008-01-15")) %>% 
  mutate(first_price = price) %>%
  select(commodity, first_price) %>%
  full_join(graph_food, ., by = "commodity") %>%
  mutate(perc_diff = (price - first_price)/first_price*100) # Calculating the percent increase in price from their first entry in 2008-01-15

###### Graphing ###############

# First we specify a static graph with our desired aesthetics
# If after running this code, food_static doesn't look quite right, that's okay. It'll come together once animated
food_static <- ggplot(graph_food, aes(x = date, y = price, color = commodity)) +
  geom_line(show.legend = FALSE, aes(size = 1)) + # Lines for price each food item
  geom_segment(aes(xend = as.Date("2022-4-15"), yend = price, group = commodity), linetype = 2, colour = 'grey', show.legend = FALSE) + # Vertical line connecting current price of a food item to its label
  geom_point(size = 2, show.legend = FALSE) + # Add a point at the end of the geom_line
  geom_point(show.legend = FALSE) +
  geom_vline(xintercept = as.Date("2020-3-15"), linetype = 2)+ # Vertical line to mark start of COVID-19 Outbreak in the Philippines
  annotate("text", label = "COVID-19 Outbreak", x = as.Date("2020-5-15"), y = 250, angle = 90, size = 7) + #Label of start of COVID-19 Outbreak in the Philippines
  geom_text(aes(x = as.Date("2022-4-15"), 
                label = paste(commodity, "(", round(perc_diff,2), "%)") ,
                group = commodity), color = "#000000", hjust = 0, show.legend = FALSE,
                family = "Open Sans", size = 7) + # Label indicating food item and percent increase in price
  scale_x_date(limits = c(as.Date("2008-01-15"), as.Date("2024-10-15")), #Increase x-limits to fit label
               breaks = scales::pretty_breaks(n = 10)) + #Make breaks yearly
  theme_minimal() +
  theme(
    text = element_text(family = "Open Sans", size = 30),
    panel.grid = element_blank(), # Remove grid
    plot.margin = unit(c(0.8,0.8,0.8,0.8), "cm"),
    plot.title = element_text(family = "Open Sans", face = "bold", size = 40, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 40, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 40, b = 0, l = 0))
  ) +
  labs(x = "Year", y = "Price (PHP)", title = "Cost of 1 Kilo of Meat in the Philippines")


# Animate the graph over time
food_temp <- food_static +  transition_reveal(date)

# Specify details for animation
time_temp <- animate(food_temp, #save and export the line graph
                     width = 1200, height = 600, 
                     fps = 10, duration = 20,
                     end_pause = 60, 
                     renderer = av_renderer(),
                     type = "cairo")

# Save final animation
anim_save("PH Meat Prices Over Time.mp4", time_temp)


