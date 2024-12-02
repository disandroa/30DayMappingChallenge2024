# 30-Day Mapping Challenge
# Day 26: Map Projections
# Mapping the globe on a Dymaxion projection

# Akira Di Sandro
# start date: 2024-12-02
# last updated: 2024-12-02


# set-up ----
{
  # load packages
  library(tidyverse)
  library(rnaturalearth)
  library(sf)
  library(ggplot2)
  
  # set CRS/projection
  proj <- "+proj=vandg4" # igh, moll, wintri, robin, vandg4
  
}

# load data ----
{
  # load world data
  world <- ne_countries(scale = "medium",
                        returnclass = "sf") %>% 
    st_transform(crs = proj)
  
}

# create map ----
{
  # create map
  proj_map <- ggplot() +
    
    # add base map
    geom_sf(data = world, fill = "tan", color = "antiquewhite") + 
    
    # add title and subtitle
    labs(title = "The World on the Van der Grinten Projection",
    caption = "
         30-Day Map Challenge\n
         Day 26: Map Projections\n
         Author: Akira Di Sandro\n
         Source: Rnaturalearth\n
         R Packages used: tidyverse, sf, ggplot2, rnaturalearth"
    ) +
    
    # define map theme
    theme_minimal() +
    theme(plot.title = element_text(face = "bold",
                                    size = 14,
                                    hjust = 0.5,
                                    margin = margin(0, 0, -5, 0),
                                    family = "AppleGothic"),
          plot.caption = element_text(face = "italic",
                                      size = 7,
                                      hjust = 1,
                                      margin = margin(-45, 5, 0, 0),
                                      lineheight = 0.5,
                                      family = "AppleGothic"),
          plot.background = element_rect(fill = "#96dbe3", color = "transparent")
    )
  
  # save map as png
  ggsave("outputs/26-Aki-MapProjections.png",
         plot = proj_map,
         width = 8, height = 5, units = "in")
  
}
