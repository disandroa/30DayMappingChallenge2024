# 30-Day Mapping Challenge
# Day 22: 2 colors
# Mapping where trans youth are not able to receive gender-affirming care

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
  library(ggtext)
  library(glue)
  library(cowplot)
  
  # set CRS
  crs_contUS <- 'ESRI:102004' # ESRI:102004 - USA_Contiguous_Lambert_Conformal_Conic
  crs_alaska <- 'EPSG:3338'
  crs_hawaii <- 'EPSG:4269'
  
}

# load data ----
{
  # list of states where gender-affirming care is not available for trans youth
  # source: https://www.kff.org/other/dashboard/gender-affirming-care-policy-tracker/
  banned_care <- c("Alabama", "Arkansas", "Arizona", "Florida", "Georgia", "Iowa", "Idaho", "Indiana", "Kentucky", "Louisiana", "Missouri", "Mississippi", "Montana", "North Carolina", "New Hampshire", "North Dakota", "Nebraska", "Ohio", "Oklahoma", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "West Virginia", "Wyoming")
  
  # US states
  USA <- ne_states(country = "united states of america") %>% 
    mutate(affirming_care = ifelse(name %in% banned_care, 0, 1),
           color = ifelse(affirming_care == 0, "red", "black"))
  
}

# data wrangling/analysis ----
{
  # contiguous states
  USA_contig <- USA %>% 
    filter(!grepl("Alaska|Hawaii", name)) %>% 
    st_transform(crs = crs_contUS)
  
  alaska <- USA %>% 
    filter(grepl("Alaska", name)) %>% 
    st_transform(crs = crs_alaska)
  
  hawaii <- USA %>% 
    filter(grepl("Hawaii", name)) %>% 
    st_transform(crs = crs_hawaii)
  
}

# create map ----
{
  # annotations
  color_red <- "red"
  text_title <- glue("<span style='color:{color_red};'>**Half of US States**</span> have Policies and Laws Limiting Access to Gender Affirming Care for Youth")
  
  # create map
  care_map1 <- ggplot() +
    
    # add base map
    geom_sf(data = USA_contig, aes(fill = color)) + 
    
    # set fill color
    scale_fill_identity() +
    
    # set map limits
    coord_sf(xlim = c(-2361356, 2256078),
             ylim = c(-1600266, 1721503)) +
    
    # add title and subtitle
    labs(caption = "
         30-Day Map Challenge\n
         Day 22: 2 Colors\n
         Author: Akira Di Sandro\n
         Source: KFF\n
         R Packages used: tidyverse, sf, ggplot2, rnaturalearth, ggtext, glue, cowplot"
    ) +
    
    # add annotations + label
    geom_richtext(aes(x = -52639, y = 1700000, label = text_title),
                  size = 12/.pt, color = "black", hjust = 0.5, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    
    # define map theme
    theme_void() +
    theme(plot.caption = element_text(face = "italic",
                                      size = 7,
                                      hjust = 1,
                                      margin = margin(-45, 5, 0, 0),
                                      lineheight = 0.5,
                                      family = "AppleGothic"),
          panel.background = element_rect(fill = "white", color = "transparent")
    )
  
  care_map2 <- ggplot() +
    
    # add base map for alaska
    geom_sf(data = alaska, aes(fill = color)) +
    
    # set fill color
    scale_fill_identity() +
    
    # set theme
    theme_void()
  
  care_map3 <- ggplot() +
    
    # add base map for hawaii
    geom_sf(data = hawaii, aes(fill = color)) +
    
    # set fill color
    scale_fill_identity() +
    
    # set map limits
    coord_sf(xlim = c(-161.3044, -154.8141),
             ylim = c(18.90612, 22.20168)) +
    
    # set theme
    theme_void()
  
  care_map <- ggdraw() +
    draw_plot(care_map1, 0, 0, 1, 1) +
    draw_plot(care_map2, 0, 0.1, 0.24, 0.24) +
    draw_plot(care_map3, 0.25, 0.02, 0.15, 0.15)
  
  # save map as png
  ggsave("outputs/22-Aki-2Colors.png",
         plot = care_map,
         width = 8, height = 6, units = "in")
  
}
