# 30-Day Mapping Challenge
# Day 21: Conflict

# Akira Di Sandro
# start date: 2024-11-30
# last updated: 2024-11-30


# set-up ----
{
  # load packages
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(ggtext)
  library(glue)
  
  # crs for palestine
  crs_palestine <- 'EPSG:28191'
  
}

# load data ----
{
  # source: https://gaza-unosat.hub.arcgis.com/pages/data?preview=true
  palestine <- st_read("data/day21_data/UNOSAT_GazaStrip_CDA_06September2024.gdb") %>% 
    st_as_sf() %>% 
    st_transform(crs = crs_palestine)
  
  # for base map
  pal_basemap <- ne_countries(scale = "large",
                              country = c("Palestine", "Israel", "Egypt", "Lebanon")) %>% 
    st_transform(crs = crs_palestine)
}

# data wrangling ----
{
  # only keep data from latest sensor
  pal_latest <- palestine %>% 
    dplyr::select(SensorDate_9:Shape)
}

# create map ----
{
  # annotations
  text_egy <- "Egypt"
  text_isr <- "Israel"
  text_pal <- "Gaza, Palestine"
  
  col_low <- "#400603"
  col_high <- "#fc1105"
  
  text_caption <- glue("Since October 7th, 2023, Israel has relentlessly\n
                       and indiscriminantly bombarded the Gaza strip.\n
                       This map shows the severity of damages to\n
                       buildings in Gaza as a result of the ongoing\n
                       genocide (<span style='color:{col_low};'>**low**</span> to <span style='color:{col_high};'>**high**</span> levels of damage).")
  
  # create map
  pal_map <- ggplot() +
    
    # add base map
    geom_sf(data = pal_basemap, fill = "#555555", color = "lightgrey") + 
    
    # thicker line around gaza
    geom_sf(data = pal_basemap %>% 
              filter(admin == "Palestine"), fill = "#555555", color = "lightgrey",
            lwd = 1) + 
    
    # add points of damaged buildings
    geom_sf(data = pal_latest, aes(color = Main_Damage_Site_Class_9),
            alpha = 0.25,
            show.legend = F) +
    
    # set color and size
    scale_color_gradient(low = "#400603",
                         high = "#fc1105") +
    
    # map limits
    coord_sf(xlim = c(65000, 110000),
             ylim = c(62000, 125000)) +
    
    # add title and subtitle
    labs(title = "Building Damages in the Gaza Strip,\nSep. 2024",
         caption = "
         30-Day Map Challenge\n
         Day 21: Conflict\n
         Author: Akira Di Sandro\n
         Source: UNOSTAT\n
         R Packages used: tidyverse, sf, ggplot2, rnaturalearth, ggtext, glue"
    ) +
    
    # add annotations + label
    geom_richtext(aes(x = 68000, y = 65000, label = text_egy),
                  size = 10/.pt, lineheight = 0.5, color = "lightgrey", hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    geom_richtext(aes(x = 82000, y = 90000, label = text_pal),
                  size = 10/.pt, lineheight = 0.5, color = "#555555", hjust = 1, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    geom_richtext(aes(x = 100000, y = 75000, label = text_isr),
                  size = 10/.pt, lineheight = 0.5, color = "lightgrey", hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    geom_richtext(aes(x = 65000, y = 110000, label = text_caption),
                  size = 13/.pt, lineheight = 0.5, color = "black", hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    
    # define map theme
    theme_void() +
    theme(plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    margin = margin(5, 0, -45, 5), 
                                    hjust = 0,
                                    family = "AppleGothic"),
          plot.caption = element_text(face = "italic",
                                      size = 7,
                                      hjust = 1,
                                      margin = margin(-45, 5, 0, 0),
                                      lineheight = 0.5,
                                      family = "AppleGothic"),
          panel.background = element_rect(fill = "#96dbe3"),
          plot.margin = margin(0, 0, 0, 0)
    )
  
  # save map as png
  ggsave("outputs/21-Aki-Conflict.png",
         plot = pal_map,
         width = 6, height = 8, units = "in")
  
}
