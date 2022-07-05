# Description
 - `PH Temp Animation.R` - Code creating the .gif of Average Temperature Change Over Time in the Philippines
 - `output`
  - `PH Temp by Region.gif` - animation of circular bar graph
  - `PH Ave Over Time.gif` - animation of line graph
  - `Final Animation.mp4` - animation of circular bar graph stacked on top of the line graph

## R Contents
1. Data Wrangling
  - Reading in the data and sf file
2. Graphing
  - Circular Bar Graph
  - Line Graph


## Tools
 - Packages: tidyverse, sf, gganimate, png, gifski, extrafont
 - [flixier](https://editor.flixier.com/) for combining the .gif files and saving them as a video


# Data Sources
 - [World Bank Group Climate Change Knowledge Portal](https://climateknowledgeportal.worldbank.org/download-data)
   - Chooose the following options
     - `Tab` - Timeseries
     - `Collection` - CRU (Observed)
     - `Variable` - Mean-Temperature
     - `Aggregation` - Annual
     - `Area Type` - Country + Sub-national units
     - `Country` - Philippines
     - `Time Period` - Historical Reference Period, 1901 


# References
1. [Circular Barplot](https://r-graph-gallery.com/circular-barplot.html)
2. [Building an animation step-by-step with gganimate](https://www.alexcookson.com/post/2020-10-18-building-an-animation-step-by-step-with-gganimate/)

Project Inspired by [@anttilip](https://twitter.com/anttilip/status/1542192214016724996) on Twitter
