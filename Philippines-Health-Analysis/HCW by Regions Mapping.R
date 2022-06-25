library(tidyverse)
library(sf)
library(here)
library(tmap)
library(viridis)

ds_regions <- c("armm", "car", "ncr",  "1", "2", "3", "4a", "4b", "5",
                "6", "7", "8", "9", "10", "11", "12", "caraga")
here()
ph_regions <- read_sf(here("Philippines Spatial Admin 0-2", "phl_admbnda_adm1_psa_namria_20200529.shp"))
ph_health <- readxl::read_excel(here("Philippines Regional Health Data.xlsx"))



ph_regions <- ph_regions %>%
  mutate(region = ds_regions)
map_palette <- magma(5)

ph_health <- ph_health %>% dplyr::filter(year == 2015) %>% dplyr::select(category, Doctors)
ph_health <- ph_health[2:18,]


ph_doctors <- full_join(ph_regions, ph_health, by = c("region" = "category"))

tm_shape(ph_doctors) +
  tm_fill(col = "Doctors", style = "quantile", palette = map_palette) +
  tm_borders() +
  tm_legend(position = c("left", "top"), legend.outside=TRUE) +
  tm_credits("Source: PSA, DOH", position = c("left", "bottom")) +
  tm_layout(main.title = "Number of Government Doctors per Region (2015)",
            main.title.position = "center")
