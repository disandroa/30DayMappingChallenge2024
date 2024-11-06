# 30-Day Mapping Challenge
# Day 3: Polygons
# Plotting the Political Wards of Philadelphia County with a choropleth showing voter turnout from the 2020 General Election

# Akira Di Sandro
# start date: 2024-11-03
# last updated: 2024-11-04


# set-up ----
{
  # load packages
  library(tidyverse)
  library(sf)
  library(tidycensus)
  library(ggtext)
  library(glue)
  
  # set working directory
  # setwd("~/Documents/MUSA/Fall24/MUSA6310_Communication/30DayMappingChallenge2024")
  
  # set CRS
  my_crs <- 'EPSG:2272' # EPSG:2272 - NAD83 / Pennsylvania South (ftUS)
}

# load data ----
{
  # source: https://opendataphilly.org/datasets/political-wards/
  philly_pol_wards <- st_read("https://opendata.arcgis.com/datasets/d67c97376d18456d98e5fa6e00415ad4_0.geojson") %>% 
    st_transform(crs = my_crs)
  
  # source: https://opendataphilly.org/datasets/voter-turnout/
  voter_turnout <- read.csv("data/day03_data/election_turnout_ward.csv")
  
  # source: https://opendataphilly.org/datasets/hydrology/
  hydro <- st_read("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/Hydrographic_Features_Poly/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson") %>% 
    st_transform(crs = my_crs)
  
  # loading geometries of counties surrounding philly to create a pseudo-basemap
  {
    nj_tracts <- get_acs(geography = "county",
                         variables = "B01001_001",
                         year = 2022,
                         state = 34,
                         county = c(5, 7, 15),
                         geometry = T) %>% 
      st_transform(crs = my_crs)
    
    pa_tracts <- get_acs(geography = "county",
                         variables = "B01001_001",
                         year = 2022,
                         state = 42,
                         county = c(17, 45, 91),
                         geometry = T) %>% 
      st_transform(crs = my_crs)
    
  }
  
}

# data wrangling ----
{
  # filter voter_turnout to keep data from the 2020 General Election
  voter_turnout_20 <- voter_turnout %>% 
    filter(election_name == "2020 General")
  
  # join voter_turnout_20 to philly_pol_wards
  to_map <- left_join(philly_pol_wards %>% 
                        mutate(ward = as.numeric(WARD_NUM)), 
                      voter_turnout_20, 
                      by = "ward")
}

# create map ----
{
  # create map labels and define color palette
  {
    # define colors for the annotations
    lightgreen <- "#9ec799"
    darkgreen <- "#093d02"
    
    # annotations
    annotation_lowest <- glue("
    **Lowest Voter Turnout**:\n
    Ward 27, 39.9%")
    annotation_highest <- glue("
    **Highest Voter Turnout**:\n
    Ward 9, 80.1%")
    annotation_subtitle <- glue("Colored by **Voter Turnout** at the 2020 General Election\n
                         ranging from <span style='color:{lightgreen};'>**39.9%**</span> to <span style='color:{darkgreen};'>**80.1%**</span>.")
    
  }
  
  # Create map
  philly_ward_map <- ggplot() +
    # main data to plot
    geom_sf(data = to_map, aes(fill = turnout), color = "transparent") +
    
    # add surrounding counties
    geom_sf(data = nj_tracts, fill = "#555555", color = "lightgrey") +
    geom_sf(data = pa_tracts, fill = "#555555", color = "lightgrey") +
    
    # add hydrology
    geom_sf(data = hydro, fill = "#96dbe3", color = "transparent") +
    
    # add cora-colored border to emphasize the regions with highest and lowest voter turnout
    geom_sf(data = to_map %>% 
              filter(turnout == .399 | turnout == .801),
            fill = "transparent", color = "coral") +
    
    # define custom color palette
    scale_fill_gradient(low = "#9ec799",
                        high = "#093d02") +
    
    # map limits
    coord_sf(xlim = c(2630000, 2749276),
             ylim = c(208915, 330000)) +
    
    # add title and subtitle
    labs(title = "Philadelphia County Political Wards",
         caption = "
         30-Day Map Challenge\n
         Day 3: Polygons\n
         Author: Akira Di Sandro\n
         Source: OpenDataPhilly\n
         R Packages used: tidyverse, sf, tidycensus, ggtext, glue"
    ) +
    
    # add annotations + label
    geom_richtext(aes(x = 2641928, y = 233598, label = annotation_lowest),
                  size = 5/.pt, lineheight = 0.5, color = "#9ec799", hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    geom_richtext(aes(x = 2683674, y = 295230, label = annotation_highest),
                  size = 5/.pt, lineheight = 0.5, color = "#093d02", hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    geom_richtext(aes(x = 2633578, y = 308204, label = annotation_subtitle), 
                  size = 10/.pt, lineheight = 0.5, color = "black", hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    
    # add lines to annotations
    geom_segment(aes(x = 2662920, xend = 2680096, y = 233598, yend = 234598), color = "coral", size = 0.3) +
    geom_segment(aes(x = 2676518, xend = 2683078, y = 285198, yend = 295230), color = "coral", size = 0.3) +
    
    # define map theme
    theme_void() +
    theme(plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    margin = margin(10, 0, -50, 10), 
                                    hjust = 0,
                                    family = "AppleGothic"),
          legend.position = "none",
          plot.caption = element_text(face = "italic",
                                      size = 5,
                                      hjust = 1,
                                      margin = margin(-40, 20, 0, 0),
                                      lineheight = 0.5,
                                      family = "AppleGothic"),
          plot.background = element_rect(fill = "#555555"),
          panel.background = element_rect(fill = "#555555"),
          plot.margin = margin(0, 0, 0, 0)
    )
  
  # save map as png
  ggsave("outputs/03-Aki-Polygons.png",
         plot = philly_ward_map,
         width = 5, height = 5, units = "in")
  
}
