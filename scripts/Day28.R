# 30-Day Mapping Challenge
# Day 28: Blue Planet
# Mapping the Great Lakes with a description of their importance to Native American societies.

# Akira Di Sandro
# start date: 2024-11-28
# last updated: 2024-11-29


# set-up ----
{
  # load packages
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(glue)
  library(ggtext)
  
  # set CRS for the great lakes
  crs_greatlakes <- 'EPSG:3174'
  
}

# load and clean data ----
{
  # get data for US States that touch the Great Lakes
  states <-  ne_states(country = "united states of america", 
                       returnclass = "sf") %>% 
    dplyr::select(adm1_code,name,postal) %>% 
    filter(grepl("IL|IN|WI|MI|MN|OH|PA|NY|IA", postal)) %>% 
    st_transform(crs = crs_greatlakes)
  
  # get data for Canadian provinces
  provinces <- ne_states(country = "canada", 
                         returnclass = "sf") %>% 
    dplyr::select(adm1_code,name,postal) %>% 
    filter(grepl("ON|QC", postal)) %>% 
    st_transform(crs = crs_greatlakes)
  
  # get Great Lakes data
  # source: https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd
  great_lakes <- ne_download(scale = "large",
                             type = "lakes", 
                             category = "physical",
                             returnclass = "sf") %>% 
    dplyr::select(name,name_alt) %>% 
    filter(grepl("Great Lakes", name_alt)) %>% 
    st_transform(crs = crs_greatlakes)
  
  # tribes that surround the great lakes: 
  # references:
  #   https://www.eekwi.org/great-lakes/humans-and-great-lakes/native-peoples-great-lakes-region#:~:text=Notable%20tribes%20around%20the%20Great,%2C%20and%20Sioux%20(Lakota).
  #   https://kidsforward.org/wisconsins-native-communities/
  #   https://www.mpm.edu/content/wirp/ICW-21
  #   https://www.britannica.com/topic/Native-American/The-Arctic
  #   https://native-land.ca/
  NA_data <- st_read("data/day28_data/northeast_indigenous_peoples.geojson") %>%
    st_transform(crs = crs_greatlakes) %>% 
    mutate(color = c("#6FB382FF", "#AF9699FF", "#92BBD9FF", "#44A57CFF",
                     "#590514FF", "#06425AFF", "#AD8152FF", "#BBA78CFF",
                     "#ECE28BFF", "#833437FF", "#E48C2AFF", "#d3a7d4",
                     "#CEC917FF", "#B50A2AFF", "#E9D097FF", "#E3D1C3FF",
                     "#F3E8CCFF", "#80C7C9FF"),
           name = stringr::str_to_title(name))
  
}

# create map ----
{
  # annotations
  text_sup <- "L. Superior"
  text_ont <- "L. Ontario"
  text_mic <- "L. Michigan"
  text_hur <- "L. Huron"
  text_eri <- "L. Erie"
  text_MN <- "MN"
  text_MI <- "MI"
  text_OH <- "OH"
  text_PA <- "PA"
  text_NY <- "NY"
  text_IA <- "IA"
  text_WI <- "WI"
  text_IL <- "IL"
  text_IN <- "IN"
  text_ON <- "ON"
  text_QC <- "QC"
  
  text_subtitle <- glue(
    "Home to many Native American tribes\n
    throughout history, this map shows\n
    some of the most prominent groups."
    )
  
  # function to get coordinates of centroid of each lake
  getLakeCoords <- function(lakename) {
    loc <- great_lakes %>% 
      filter(grepl(lakename, name)) %>% 
      st_centroid() %>% 
      st_coordinates()
  }
    
  # location for annotations
  loc_sup <- getLakeCoords("Sup")
  loc_ont <- getLakeCoords("Ont")
  loc_mic <- getLakeCoords("Mic")
  loc_hur <- getLakeCoords("Hur")
  loc_eri <- getLakeCoords("Eri")
  loc_MN <- c(450000, 1270000)
  loc_MI <- states %>% 
    filter(postal == "MI") %>% 
    st_centroid() %>% 
    st_coordinates()
  loc_OH <- c(1041125, 535000)
  loc_PA <- c(1426587, 535000)
  loc_NY <- c(1450000, 650000)
  loc_IA <- c(450000, 609267)
  loc_WI <- states %>% 
    filter(postal == "WI") %>% 
    st_centroid() %>% 
    st_coordinates()
  loc_IL <- c(520000, 535000)
  loc_IN <- c(785505, 535000)
  loc_ON <- c(1150000, 1200000)
  loc_QC <- c(1600000, 1100000)
  loc_subtitle <- c(1370000, 1310000)
  
  }

  # Native American tribe annotations
  {
    # function to get centroid of NA tribe locations
    getNACoords <- function(tribe) {
      loc <- NA_data %>% 
        filter(grepl(tribe, name, ignore.case = T)) %>% 
        st_centroid() %>% 
        st_coordinates()
    }
    
    loc_ill <- getNACoords("illinois")
    loc_sus <- getNACoords("neutral")
    loc_iro <- getNACoords("iroquois")
    loc_sus <- getNACoords("susquehannock")
    loc_erie <- getNACoords("erie")
    loc_kic <- getNACoords("kickapoo")
    loc_pot <- getNACoords("potawatomi")
    loc_mia <- getNACoords("miami")
    loc_alg <- getNACoords("algonquin")
    loc_ott <- getNACoords("ottawa")
    loc_hoc <- getNACoords("ho-chunk")
    loc_oji <- getNACoords("ojibwe")
    loc_sha <- getNACoords("shawnee")
    loc_iow <- getNACoords("ioway")
    loc_huron <- getNACoords("huron")
    loc_men <- getNACoords("menominee")
    loc_sau <- getNACoords("sauk-fox")
    loc_pet <- getNACoords("petun")
  }
  
  # map
  GL_map <- ggplot() +
    
    # surrounding states/provinces
    geom_sf(data = states, fill = "#555555", color = "lightgrey") +
    geom_sf(data = provinces, fill = "#444444", color = "lightgrey") +
    
    # great lakes
    geom_sf(data = great_lakes, fill = "#96dbe3", color = "#96dbe3") +
    
    # NA tribes
    geom_sf(data = NA_data, aes(fill = color), alpha = 0.25, color = "transparent") +
    
    # manually defined colors
    scale_fill_identity() +
    scale_color_identity() +
    
    # set boundaries for map
    coord_sf(xlim = c(398512,1700000),
             ylim = c(555490,1415721)) +
    
    # annotations
    labs(title = "The Great Lakes",
         caption = "
         30-Day Map Challenge\n
         Day 28: Blue Planet\n
         Author: Akira Di Sandro\n
         Source: RNaturalEarth\n
         R Packages used: tidyverse, sf, rnaturalearth, ggplot2, ggtext, glue") +
    
    geom_richtext(aes(x = loc_sup[1], y = loc_sup[2] + 25000, label = text_sup),
                  size = 14/.pt, color = "#1f7d87", family = "AppleGothic",
                  fill = NA, label.color = NA) + # lake superior
    geom_richtext(aes(x = loc_ont[1], y = loc_ont[2], label = text_ont),
                  size = 14/.pt, color = "#1f7d87", family = "AppleGothic",
                  fill = NA, label.color = NA) + # lake ontario
    geom_richtext(aes(x = loc_mic[1] - 15000, y = loc_mic[2], label = text_mic, angle = 75),
                  size = 14/.pt, color = "#1f7d87", family = "AppleGothic",
                  fill = NA, label.color = NA) + # lake michigan
    geom_richtext(aes(x = loc_hur[1], y = loc_hur[2], label = text_hur),
                  size = 14/.pt, color = "#1f7d87", family = "AppleGothic",
                  fill = NA, label.color = NA) + # lake huron
    geom_richtext(aes(x = loc_eri[1], y = loc_eri[2], label = text_eri),
                  size = 14/.pt, color = "#1f7d87", family = "AppleGothic",
                  fill = NA, label.color = NA) + # lake erie
    
    geom_richtext(aes(x = loc_WI[1], y = loc_WI[2], label = text_WI),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # WI
    geom_richtext(aes(x = loc_MI[1], y = loc_MI[2], label = text_MI),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # MI
    geom_richtext(aes(x = loc_OH[1], y = loc_OH[2], label = text_OH),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # OH
    geom_richtext(aes(x = loc_MN[1], y = loc_MN[2], label = text_MN),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # MN
    geom_richtext(aes(x = loc_IN[1], y = loc_IN[2], label = text_IN),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # IN
    geom_richtext(aes(x = loc_PA[1], y = loc_PA[2], label = text_PA),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # PA
    geom_richtext(aes(x = loc_IL[1], y = loc_IL[2], label = text_IL),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # IL
    geom_richtext(aes(x = loc_IA[1], y = loc_IA[2], label = text_IA),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # IA
    geom_richtext(aes(x = loc_NY[1], y = loc_NY[2], label = text_NY),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # NY
    geom_richtext(aes(x = loc_ON[1], y = loc_ON[2], label = text_ON),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # ON
    geom_richtext(aes(x = loc_QC[1], y = loc_QC[2], label = text_QC),
                  size = 10/.pt, color = "darkgrey", family = "AppleGothic",
                  fill = NA, label.color = NA) + # QC
    
    geom_richtext(aes(x = loc_subtitle[1], y = loc_subtitle[2], label = text_subtitle, hjust = 0),
                  size = 10/.pt, color = "black", family = "AppleGothic",
                  fill = NA, label.color = NA) + # subtitle
    
    geom_richtext(aes(x = loc_ill[1] + 90000, y = loc_IL[2], label = NA_data$name[1]),
                  size = 10/.pt, color = NA_data$color[1], family = "AppleGothic",
                  fill = NA, label.color = NA) + # illinois
    geom_richtext(aes(x = loc_neu[1], y = loc_neu[2], label = NA_data$name[2]),
                  size = 10/.pt, color = NA_data$color[2], family = "AppleGothic",
                  fill = NA, label.color = NA) + # neutral
    geom_richtext(aes(x = loc_iro[1] - 150000, y = loc_iro[2], label = NA_data$name[3]),
                  size = 10/.pt, color = NA_data$color[3], family = "AppleGothic",
                  fill = NA, label.color = NA) + # iroquois
    geom_richtext(aes(x = loc_sus[1], y = loc_PA[2] + 25000, label = NA_data$name[4]),
                  size = 10/.pt, color = NA_data$color[4], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Susquehannock
    geom_richtext(aes(x = loc_erie[1], y = loc_erie[2], label = NA_data$name[5]),
                  size = 10/.pt, color = NA_data$color[5], family = "AppleGothic",
                  fill = NA, label.color = NA) + # erie
    geom_richtext(aes(x = loc_kic[1], y = loc_kic[2], label = NA_data$name[6]),
                  size = 10/.pt, color = NA_data$color[6], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Kickapoo
    geom_richtext(aes(x = loc_pot[1], y = loc_pot[2] - 25000, label = NA_data$name[7]),
                  size = 10/.pt, color = NA_data$color[7], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Potawatomi
    geom_richtext(aes(x = loc_mia[1], y = loc_IN[2], label = NA_data$name[8]),
                  size = 10/.pt, color = NA_data$color[8], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Miami
    geom_richtext(aes(x = loc_alg[1], y = loc_alg[2], label = NA_data$name[9]),
                  size = 10/.pt, color = NA_data$color[9], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Algonquin
    geom_richtext(aes(x = loc_ott[1], y = loc_ott[2], label = NA_data$name[10]),
                  size = 10/.pt, color = NA_data$color[10], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Ottawa
    geom_richtext(aes(x = loc_hoc[1], y = loc_hoc[2], label = NA_data$name[11]),
                  size = 10/.pt, color = NA_data$color[11], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Ho-Chunk
    geom_richtext(aes(x = loc_oji[1], y = loc_oji[2], label = NA_data$name[12]),
                  size = 10/.pt, color = NA_data$color[12], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Ojibwe
    # geom_richtext(aes(x = loc_sha[1], y = loc_sha[2], label = NA_data$name[13]),
    #               size = 10/.pt, color = NA_data$color[13], family = "AppleGothic",
    #               fill = NA, label.color = NA) + # Shawnee, actually does not appear on map
    geom_richtext(aes(x = loc_iow[1] + 200000, y = loc_iow[2], label = NA_data$name[14]),
                  size = 10/.pt, color = NA_data$color[14], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Ioway
    geom_richtext(aes(x = loc_huron[1], y = loc_huron[2], label = NA_data$name[15]),
                  size = 10/.pt, color = NA_data$color[15], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Huron
    geom_richtext(aes(x = loc_men[1], y = loc_men[2], label = NA_data$name[16]),
                  size = 10/.pt, color = NA_data$color[16], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Menominee
    geom_richtext(aes(x = loc_sau[1], y = loc_sau[2] - 25000, label = NA_data$name[17]),
                  size = 10/.pt, color = NA_data$color[17], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Sauk-Fox
    geom_richtext(aes(x = loc_pet[1], y = loc_pet[2], label = NA_data$name[18]),
                  size = 10/.pt, color = NA_data$color[18], family = "AppleGothic",
                  fill = NA, label.color = NA) + # Petun
    
    
    # theme specifications 
    theme_void() +
    theme(plot.title = element_text(face = "bold", 
                                    size = 30, 
                                    margin = margin(5, 10, -40, 0), 
                                    hjust = 1,
                                    family = "AppleGothic"),
          plot.caption = element_text(face = "italic",
                                      size = 5,
                                      hjust = 1,
                                      margin = margin(-40, 5, 0, 0),
                                      lineheight = 0.5,
                                      family = "AppleGothic"))
  
  # save map as png
  ggsave("outputs/28-Aki-BluePlanet.png",
         plot = GL_map,
         width = 10, height = 7, units = "in")
  
}
