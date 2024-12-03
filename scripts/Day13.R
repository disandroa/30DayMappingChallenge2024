# 30-Day Mapping Challenge
# Day 13: A New Tool
# Using tmap to create an animated map.

# Akira Di Sandro
# start date: 2024-11-12
# last updated: 2024-12-02


# set-up ----
{
  # load packages
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(tmap)
  
  # set CRS
  my_crs <- 'EPSG:2272' # EPSG:2272 - NAD83 / Pennsylvania South (ftUS)
}

# load data ----
{
  # loop to load tracts for 2012-2022
  for (yr in 12:22) {
    year <- as.numeric(paste0("20", yr))
    df_name <- paste0("tracts", yr)
    
    tracts <- get_acs(geography = "tract",
                      variables = c("B01001_001E", # total population
                                    "B01001D_001E" # Asian population
                      ),
                      year = year,
                      state = 42,
                      county = 101,
                      geometry = T,
                      output = "wide") %>%
      st_transform(crs = my_crs) %>%
      rename(TotalPop = B01001_001E,
             TotalAsianPop = B01001D_001E
      ) %>%
      dplyr::select(-ends_with("M", ignore.case = F)) %>% 
      mutate(PctAsian = TotalAsianPop / TotalPop,
             year = year)
    
    assign(df_name, tracts)
  }
  
  # source: https://opendataphilly.org/datasets/hydrology/
  hydro <- st_read("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/Hydrographic_Features_Poly/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson") %>% 
    st_transform(crs = my_crs)
  
}

# data wrangling ----
{
  pctAsian <- rbind(tracts12 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts13 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts14 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts15 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts16 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts17 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts18 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts19 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts20 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts21 %>% 
                      dplyr::select(GEOID, PctAsian, year),
                    
                    tracts22 %>% 
                      dplyr::select(GEOID, PctAsian, year))
}

# create map ----
{
  # define cusstom color palette
  my_pal <- colorRampPalette(c("#180F3EFF", "#F1605DFF"))
  
  # custom breaks and labels for legend
  my_breaks <- c(0, 0.13, 0.26, 0.39, 0.53, 0.66)
  my_labels <- c("0%", "13%", "26%", "39%", "53%", "66%")
  
  pctAsian_tm_anim <- tm_shape(pctAsian) +
    tm_polygons(
      col = "PctAsian",
      style = "cont",
      pal = my_pal(100),
      breaks = my_breaks,
      labels = my_labels
    ) +
    tm_facets(by = "year", 
              free.coords = F,
              ncol = 1,
              nrow = 1
              ) +
    tm_layout(legend.position = c("right","center"),
              bg.color = "#555555",
              title = "Share of Asian Population in Philadelphia, 2012-2022",
              title.position = c("center", "top"),   
              title.color = "lightgrey",
              title.fontfamily = "AppleGothic",
              title.fontface = "bold",
              inner.margins = c(0.1, 0.1, 0.1, 0.1),                  
              legend.title.size = 1,
              legend.text.size = 0.6,
              legend.text.fontfamily = "AppleGothic",
              panel.show = T,
              panel.label.bg.color = "#555555",
              panel.label.fontfamily = "AppleGothic",
              panel.label.color = "lightgrey") +
    tm_credits(
      text = "30-Day Map Challenge\nDay 13: New Tool\nAuthor: Akira Di Sandro\nSource: ACS 2012-2022\nR Packages used: tidyverse, tidycensus, sf, tmap",
      position = c("right", "bottom"),
      size = 0.4,
      col = "lightgrey",
      fontfamily = "AppleGothic",
      fontface = "italic"
    ) +
    tm_shape(hydro) +
    tm_fill(
      col = "#96dbe3"
    )
  
  # save as gif
  tmap_animation(
    pctAsian_tm_anim,
    filename = "outputs/13-Aki-NewTool.gif",
    delay = 50, width = 500, height = 500
  )
}