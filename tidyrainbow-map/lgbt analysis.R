########## Loading Libraries ######################

library(tidyverse)
library(sf)   #for geospatial analysis
library(tmap) #for mapping
library(ragg) #to use custom fonts on your pc

########## Reading the Data  ######################

eu_data <- read_csv(url("https://raw.githubusercontent.com/r-lgbtq/tidyrainbow/main/data/2022/2022-08-15/EU-LGBT-Survey/LGBT_Survey_Discrimination.csv"))

dis_perc_lesbian[5,1] <- "Czechia" #changing the name Czech Republic in the dataset to match the Czechia in the shp file


world <- read_sf("ne_10m_admin_0_countries.shp")

########## Data Wrangling  ######################

# First, let's filter the data so we have only the variables we plan to map
# question_code c2_b: 
# "In the last 12 months, in the country where you live, 
# have you personally felt discriminated against or harassed 
# on the grounds of gender?"

dis_perc_lesbian <- eu_data %>% 
  dplyr::filter(question_code == "c2_b", 
                answer == "Yes", 
                subset == "Lesbian")

dis_perc_lesbian <- dis_perc_lesbian[1:28,c(1,6)] 

dis_perc_lesbian <- dis_perc_lesbian %>%
  mutate(percentage = as.numeric(percentage)) #converting the type to numeric

# Then, we'll merge our data with the shape file
a <- merge(world, dis_perc_lesbian, by.x = "SOVEREIGNT",  by.y = "CountryCode") 


########## Data Wrangling  ######################

# Specifying the palette (based on the pink of the lesbian flag)

les_pal <- c("#d6d6d6", "#d462a6", "#a40062")

# A bouding box so our map displays only countries in the dataset
# We don't have to add all countries, only some countries that will define the "edges" of our map
eu_bb <- st_bbox(world %>% filter(SOVEREIGNT %in% c("Greece", "Italy", "Spain", "Finland", "Denmark")))


a <- a %>% st_transform(54032) #changing CRS to World Azimuthal Equidistant, which tends to be better for continent-wide analyses

########## Creating our Map  ######################

tm_shape(a, bbox = eu_bb) +
  tm_fill(col = "percentage", palette = les_pal, breaks = seq(15, 50, by = 5),
          title = "Percent Experienced") + 
  tm_borders(col = "grey20", lwd = 0.5) +
  tm_legend(position = c("left", "top"), legend.outside=TRUE) +
  tm_credits("Source: Fundamental Rights Agency \nvia Kaggle", 
             position = c("center", "bottom"),
             col = "white") +
  tm_layout(main.title = "% of Lesbians who Personally Felt Discriminated Against \nor Harassed on the Grounds of Gender in Europe",
            main.title.position = "center", main.title.size = 1, main.title.color = "white",
            fontface = "bold", fontfamily = "Montserrat",
            frame = FALSE, bg.color = "grey20",
            legend.text.fontface = "plain", 
            legend.title.fontface = "plain",
            legend.text.color = "white", 
            legend.title.color = "white",
            legend.text.size = 0.7)
  
########## Creating our Bar Chart  ######################

# Rearranging countries by percentage, descending
dis_perc_lesbian_ordered <- dis_perc_lesbian %>% 
  arrange(desc(percentage)) %>%
  mutate(CountryCode = fct_reorder(CountryCode, percentage))

# Assigning colors to the bar chart
flag_pal <- rep(les_pal, each = 10)
flag_pal <- flag_pal[-2]

# Creating the bar chart 

dis_perc_lesbian_ordered %>% 
  ggplot(., aes(y = CountryCode, x = percentage, fill = CountryCode, family = "Montserrat", fontface = "bold")) +
  geom_col(width = 0.9, position = position_dodge(width = 2)) +
  geom_text(aes(label = percentage), 
              hjust = -0.4, color = "white") +
  geom_text(aes(x = 8.8, label = CountryCode), 
            size = 3.5, color = "white") +
  scale_fill_manual(values = flag_pal) +
  theme(legend.position = "none", 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Montserrat"),
        axis.ticks = element_blank(),
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "grey20"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1.2,
        plot.margin = unit(c(0.5,0.5, 0.5,0.5), "cm"))+
  scale_x_continuous(breaks = seq(10, 50, by = 10)) +
  expand_limits(x = c(1, 50))



