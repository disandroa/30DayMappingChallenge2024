# 30-Day Mapping Challenge
# Day 29: Overture Maps
# Using Overture Maps data to make a map of Firenze, Italy.

# Akira Di Sandro
# start date: 2024-11-29
# last updated: 2024-11-29


# set-up ----
{
  # load packages
  library(tidyverse)
  library(sf)
  library(overturemapsr)
  library(tigris)
  library(rdeck)
  library(htmlwidgets)
  library(arrow)
  
  # use mapbox token -- update the personal token when using this code
  # options(rdeck.mapbox_access_token = <insert personal token here>)
  
}

# load and clean data ----
{
  # load overture data
  buildings <- open_dataset('s3://overturemaps-us-west-2/release/2024-05-16-beta.0/theme=buildings')
  
  # define florence bbox
  firenze_bbox <- c(xmin = 11.151786621561394, 
                    xmax = 11.329452531215946,
                    ymin = 43.726914540750485,
                    ymax = 43.83359522522425)
  
  # load florence building data
  firenze_buildings <- buildings %>% 
    filter(bbox$xmin > firenze_bbox[1],
           bbox$xmax < firenze_bbox[2],
           bbox$ymin > firenze_bbox[3],
           bbox$ymax < firenze_bbox[4]) %>% 
    dplyr::select(id, geometry, height, class, min_height, roof_color) %>% 
    collect() %>% 
    mutate(height = ifelse(is.na(height), 0, height)) %>% 
    st_as_sf(crs = 4326) %>% 
    mutate(new_class = case_when(class %in% c("church", "chapel", "shrine", "monastery", "religious", "cathedral", "synagogue") ~ "Religious",
                                 class %in% c("semidetached_house", "residential", "shed", "apartments", "house", "boathouse", "hut", "hangar", "bunker", "allotment_house") ~ "Residential",
                                 class %in% c("office", "warehouse", "service", "commercial", "retail", "kiosk", "supermarket") ~ "Commercial",
                                 class == "government" ~ "Government",
                                 class %in% c("farm", "farm_auxiliary") ~ "Farm",
                                 class %in% c("school", "kindergarten", "university", "dormitory", "college") ~ "Education",
                                 class %in% c("garages", "garage", "parking") ~ "Parking",
                                 .default = "Other"),
           new_class = factor(new_class, levels = c("Farm", "Religious", "Parking", "Education", "Government", "Commercial", "Residential", "Other")))
  
}

# create map ----
{
  # mapping building classes
  color_palette <- c("Farm" = "#7FC97F", 
                     "Religious" = "#BEAED4", 
                     "Parking" = "#FDC086",
                     "Education" = "#FFFF99",
                     "Government" = "#386CB0",
                     "Commercial" ="#F0027F",
                     "Residential" = "#BF5B17",
                     "Other" = "#666666")
  
  map <- rdeck(map_style = mapbox_streets(), 
               initial_view_state = view_state(
                 center = c(11.246800771068502, 43.77943279356716),
                 zoom = 12.5,
                 bearing = -60,
                 pitch = 76
               )) |> 
    add_polygon_layer(
      data = firenze_buildings, 
      name = "Building Classifications in Firenze, Italia",
      get_polygon = geometry, 
      get_elevation = height, 
      get_fill_color = scale_color_category(
        col = new_class,
        palette = color_palette,
        legend = T
      ),
      extruded = TRUE, 
      opacity = 0.5) 
  
  
  # save map as html
  saveWidget(map, file = "outputs/29-Aki-Overture.html")
  
}
