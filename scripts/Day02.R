# 30-Day Mapping Challenge
# Day 2: Lines
# Are the streets of Philadelphia bike friendly?

# Akira Di Sandro
# start date: 2024-11-30
# last updated: 2024-11-30


# set-up ----
{
  # load packages
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(ggplot2)
  library(ggtext)
  library(glue)
  
  # philly crs
  crs_philly <- 'EPSG:2272' # EPSG:2272 - NAD83 / Pennsylvania South (ftUS)
  
}

# load data ----
{
  bikes <- st_read("~/Downloads/PhiladelphiaBikeNetwork_SupportingDatasets201209/BikeNetwork_SupportingDatasets201209/PhiladelphiaBikeConnectorStreets201204.shp")
  
  # source: https://opendataphilly.org/datasets/hydrology/
  hydro <- st_read("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/Hydrographic_Features_Poly/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson") %>% 
    st_transform(crs = crs_philly)
  
  # loading geometries of counties surrounding philly for basemap
  nj_tracts <- get_acs(geography = "county",
                       variables = "B01001_001",
                       year = 2022,
                       state = 34,
                       county = c(5, 7, 15),
                       geometry = T) %>% 
    st_transform(crs = crs_philly)
  
  pa_tracts <- get_acs(geography = "county",
                       variables = "B01001_001",
                       year = 2022,
                       state = 42,
                       county = c(17, 45, 91, 101),
                       geometry = T) %>% 
    st_transform(crs = crs_philly)
  
}

# data wrangling ----
{
  # simplify the street categories
  bikes_simplified <- bikes %>% 
    mutate(friendly = case_when(RecFacil %in% c("Bike Friendly Street", "Climbing Lane", "Cycletrack", "Contraflow") ~ "Bike Friendly Street",
                                RecFacil == "Buffered Bike Lane" ~ "Buffered Bike Lane",
                                grepl("Shared", RecFacil) ~ "Shared Lane",
                                grepl("^Bike Lane", RecFacil) ~ "Bike Lane",
                                .default = NA)) %>% 
    filter(!is.na(friendly))
}

# create map ----
{
  # define color values for categories of streets
  street_colors <- c("Bike Friendly Street" = "#DCCA2CFF",
                     "Shared Lane" = "coral",
                     "Bike Lane" = "#0E84B4FF",
                     "Buffered Bike Lane" = "#58A449FF")
  bikefriendly <- "#DCCA2CFF"
  shared <- "coral"
  bikelane <- "#0E84B4FF"
  buffered <- "#58A449ff"
  
  # annotation for rivers
  text_schu <- "Schuylkill River"
  text_dela <- "Delaware River"
  text_description <- glue("Most streets are <span style='color:{bikefriendly};'>**bike friendly**</span> or <span style='color:{shared};'>**shared**</span> between autos\n
                           and bikes. There are also several streets with <span style='color:{bikelane};'>**bike lanes**</span>\n
                           and very few thaat have <span style='color:{buffered};'>**buffered bike lanes**</span>. As a\n
                           Philadelphian who frequently bikes, I can say that this dataset\n
                           needs to be updated!")
  
  # Create map
  bike_streets <- ggplot() +
    
    # add surrounding counties
    geom_sf(data = nj_tracts, fill = "darkgrey", color = "lightgrey") +
    geom_sf(data = pa_tracts, fill = "darkgrey", color = "lightgrey") +
    
    # map of philadelphia county
    geom_sf(data = pa_tracts %>% 
              filter(GEOID == "42101"), 
            fill = "#555555", color = "lightgrey") +
    
    # add hydrology
    geom_sf(data = hydro, fill = "#96dbe3", color = "transparent") +
    
    # add cora-colored border to emphasize the regions with highest and lowest voter turnout
    geom_sf(data = bikes_simplified, aes(color = friendly), show.legend = F) +
    
    # define custom color palette
    scale_color_manual(values = street_colors) +
    
    # map limits
    coord_sf(xlim = c(2660000, 2749276),
             ylim = c(208915, 310000)) +
    
    # add title and subtitle
    labs(title = "Which Philadelphia Streets are Bike Friendly?",
         caption = "
         30-Day Map Challenge\n
         Day 2: Lines\n
         Author: Akira Di Sandro\n
         Source: OpenDataPhilly (Sep. 2012)\n
         R Packages used: tidyverse, sf, tidycensus, ggtext, glue"
    ) +
    
    # add annotations + label
    geom_richtext(aes(x = 2705000, y = 228000, label = text_description),
                  size = 7/.pt, lineheight = 0.5, color = "black", hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    
    # define map theme
    theme_void() +
    theme(plot.title = element_text(face = "bold", 
                                    size = 18, 
                                    margin = margin(5, 0, -45, 10), 
                                    hjust = 0,
                                    family = "AppleGothic"),
          plot.caption = element_text(face = "italic",
                                      size = 5,
                                      hjust = 1,
                                      margin = margin(-40, 5, 0, 0),
                                      lineheight = 0.5,
                                      family = "AppleGothic")
    )
  
  # save map as png
  ggsave("outputs/02-Aki-Lines.png",
         plot = bike_streets,
         width = 6, height = 6, units = "in")
  
}