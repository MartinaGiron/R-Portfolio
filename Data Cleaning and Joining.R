library(here)
library(tidyverse)
library(readxl)

here()
vital <- read_xlsx(here("ph psa regional datasets", "vital events.xlsx"),
                   sheet = "4B 2022 Vital Events by Sex", range = "A1:H140", 
                   col_names = c("category", "birth_both", "birth_male", "birth_female", "death_both", "death_male", "death_female", "marriage"),
                   col_types = c("text", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric"))
vital

healthstations <- read_xlsx(here("ph psa regional datasets", "percent brgy with health stations.xlsx"),
                            sheet = 1, range = "A4:S13", 
                            col_names = c("year", "philippines", "ncr", "car", "reg_1", "reg_2", "reg_3", "reg_4a", "reg_4b", "reg_5", 
                                          "reg_6", "reg_7", "reg_8", "reg_9", "reg_10", "reg_11", "reg_12", "caraga", "armm"),
                            col_types = "numeric")
healthstations

hcw_1 <- read_xlsx(here("ph psa regional datasets", "hcw region.xlsx"),
                   sheet = 1, range = "A4:T43", 
                   col_names = c("year", "hcw_type", "philippines", "ncr", "car", "reg_1", "reg_2", "reg_3", "reg_4a", "reg_4b", "reg_5", 
                                 "reg_6", "reg_7", "reg_8", "reg_9", "reg_10", "reg_11", "reg_12", "caraga", "armm"),
                   col_types =  c("numeric", "text", "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                  "numeric","numeric","numeric", "numeric", "numeric","numeric", "numeric","numeric"))
hcw_1

hcw_2 <- read_xlsx(here("ph psa regional datasets", "hcw more region.xlsx"),
                   sheet = 1, range = "A4:T43", 
                   col_names = c("year", "hcw_type", "philippines", "ncr", "car", "reg_1", "reg_2", "reg_3", "reg_4a", "reg_4b", "reg_5", 
                                 "reg_6", "reg_7", "reg_8", "reg_9", "reg_10", "reg_11", "reg_12", "caraga", "armm"),
                   col_types =  c("numeric", "text", "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric",
                                  "numeric","numeric","numeric", "numeric", "numeric","numeric", "numeric","numeric"))
hcw_2

immunized <- read_xlsx(here("ph psa regional datasets", "fully immunized children.xlsx"),
                       sheet = 1, range = "A4:L21", 
                       col_names = c("category", "year_2006", "year_2007", "year_2008", "year_2009", "year_2010", "year_2011",
                                     "year_2012", "year_2013", "year_2014", "year_2015", "year_2016"),
                       col_types =  c("text", "numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","numeric", "numeric", "numeric"))
immunized

##### Data Cleaning ##### 
vital$year <- rep(2021, nrow(vital))
hcw_1_years <- c(rep(2006, 4), rep(2007, 4), rep(2008, 4), rep(2009, 4), rep(2010, 4), rep(2011, 4), rep(2012, 4),
                 rep(2013, 4), rep(2014, 4), rep(2015, 4))
hcw_1$year <- hcw_1_years

hcw_2_years <- c(rep(2006, 6), rep(2007, 6), rep(2008, 6), rep(2009, 6), rep(2010, 6), rep(2011, 6), rep(2012, 4))
hcw_2$year <- hcw_2_years


immunized <- immunized %>% 
  pivot_longer(., cols = 2:12, 
               names_to = "year",
               values_to = "count_immunized",
               names_prefix = "year_")
healthstations <- healthstations %>%
  pivot_longer(., cols = 2:19,
               names_to = "category",
               values_to = "count_healthstations", 
               names_prefix = "reg_"
  )
hcw_1 <- hcw_1 %>%
  pivot_longer(., cols = 3:20,
               names_to = "category",
               values_to = "count_hcw",
               names_prefix = "reg_")
hcw_1 <- hcw_1 %>%
  pivot_wider(., names_from = hcw_type,
              values_from = count_hcw)

hcw_2 <- hcw_2 %>%
  pivot_longer(., cols = 3:20,
               names_to = "category",
               values_to = "count_hcw",
               names_prefix = "reg_")
hcw_2 <- hcw_2 %>%
  pivot_wider(., names_from = hcw_type,
              values_from = count_hcw)

hcw_full <- full_join(hcw_1, hcw_2, by = c("category", "year"))
health_assets <- full_join(hcw_full, healthstations)

vital_regions <- vital %>%
  filter(str_detect(category, "REGION") | category == "TOTAL")

regions <- c("philippines", "ncr", "car", "1", "2", "3", "4a", "4b", "5", 
             "6", "7", "8", "9", "10", "11", "12", "caraga", "armm")

vital_regions$category <- regions

immunized$category <- rep(regions, each = 11)
immunized$year <- as.numeric(immunized$year)

philippines_health <- full_join(vital_regions, health_assets, by = c("category", "year")) %>% full_join(., immunized)

writexl::write_xlsx(philippines_health, "Philippines Regional Health Data.xlsx")
