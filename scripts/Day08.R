# 30-Day Mapping Challenge
# Day 8
# I visited the DRC (specifically the Lubumbashi and Kolwezi areas) in the summer of 2023, not educated on 
# the modern-day slavery going on in mines nearby. I wanted to make a map demonstrating where violence is 
# occurring in DRC provinces compared to where healthzones are located to visualize which areas of DRC need 
# more support. At first, I was considering making a bivariate map of count of healthzones (HZ) and % change 
# in political violence events, but quickly realized that this was erasing the amount of violence happening 
# in certain areas of this country.

# Akira Di Sandro
# start date: 2024-11-09
# last updated: 2024-11-10


# set-up ----
{
  # load packages
  library(tidyverse)
  library(sf)
  library(biscale)
  library(stringr)
  library(cowplot)
  library(glue)
  library(ggtext)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  # DRC CRS (UTM Zone 34S)
  DRC_CRS <- 32734
  
}

# load data ----
{
  # shp file of all health zones in DRC
  # source: https://data.humdata.org/dataset/dr-congo-health-0
  DRC_healthzones <- st_read("data/day08_data/rdc_zone_de_sante_09092019/RDC_Zone_de_sante_09092019.shx") %>% 
    st_transform(crs = DRC_CRS)
  
  # DRC political violence count
  # source: https://data.humdata.org/dataset/democratic-republic-of-congo-acled-conflict-data
  DRC_pol_vio <- readxl::read_xlsx("data/day08_data/democratic-republic-of-congo_hrp_political_violence_events_and_fatalities_by_month-year_as-of-0.xlsx",
                                   sheet = 2)
  
  # download shape file for DRC provinces
  # source: https://datacatalog.worldbank.org/search/dataset/0040240/Democratic-Republic-of-the-Congo---Administrative-Boundaries
  DRC_provinces <- st_read("https://datacatalogfiles.worldbank.org/ddh-published/0040240/DR0050122/codadmbndaadm120170407.geojson?versionId=2023-01-19T05:05:08.3848692Z") %>% 
    st_transform(crs = DRC_CRS)
  
  # get DRC's surrounding countries
  world <- ne_countries(scale = "medium", returnclass = "sf")
  background_countries <- world %>% 
    filter(name %in% c("Angola", "Burundi", "Cameroon", "Central African Rep.", "Chad", "Ethiopia", "Eq. Guinea", "Gabon", "Congo", "Kenya", "Malawi", "Mozambique", "Nigeria", "Rwanda", "S. Sudan", "Tanzania", "Uganda", "Zambia")) %>% 
    st_transform(crs = DRC_CRS)
  rm(world)
  
}

# data wrangling ----
{
  # one big polygon of drc
  DRC_bounds <- DRC_provinces %>% 
    st_union()
  
  # rename provinces to match the other dataframes
  DRC_PV <- DRC_pol_vio %>% 
    mutate(PROVINCE = case_when(Admin1 == "Kasai" ~ "Kasaï",
                                Admin1 == "Kasai-Central" ~ "Kasaï-Central",
                                Admin1 == "Kasai-Oriental" ~ "Kasaï-Oriental",
                                Admin1 == "Mai-Ndombe" ~ "Maï-Ndombe",
                                .default = Admin1))
  
  # function to turn coordx and coordy into point geometry
  coord_to_pointgeo <- function(coord_col) {
    # separate coord_col into its separate components
    separated <- stringr::str_match(coord_col, "(\\d+)° (\\d+)' (\\d+\\,?\\d*)\" ([NSEW])")
    
    degrees <- as.numeric(separated[, 2])
    minutes <- as.numeric(separated[, 3])
    seconds <- as.numeric(sub(",",".", separated[, 4]))
    direction <- separated[, 5]
    
    decimal_degrees <- degrees + minutes / 60 + seconds / 3600
    ifelse(direction %in% c("S", "W"), -decimal_degrees, decimal_degrees)
    
  }
  
  # DRC healthzones with mappable point geometries and adding territory data
  DRC_HZ_points <- DRC_healthzones %>% 
    st_drop_geometry() %>% 
    mutate(lon = coord_to_pointgeo(coordx),
           lat = coord_to_pointgeo(coordy)) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform(crs = DRC_CRS) %>% 
    dplyr::select(OBJECTID,ID,PROVINCE,Nom,Population,coordx:geometry)
  
  # add column for area (in km^2) for each province
  DRC_provinces$area_province <- as.numeric(st_area(DRC_provinces)) / 1e6
  
  # total count of healthzones by province
  DRC_HZ_prov <- DRC_healthzones %>% 
    st_drop_geometry() %>% 
    group_by(PROVINCE) %>% 
    summarise(HZ_count = n()) %>% 
    left_join(DRC_provinces %>% 
                dplyr::select(NOM,area_province) %>% 
                rename(PROVINCE = NOM), 
              by = "PROVINCE") %>% 
    mutate(HZ_per_sqkm = HZ_count / area_province) %>% 
    st_as_sf()
  
  # combine political violence events and fatalities by province and year for 2022 and 2023
  # DRC_PV_prov <- DRC_PV %>% 
  #   filter(Year %in% c(2022, 2023)) %>% 
  #   dplyr::select(PROVINCE, Year, Events, Fatalities) %>% 
  #   group_by(PROVINCE, Year) %>% 
  #   summarise(prov_total_events = sum(Events),
  #             prov_total_fatal = sum(Fatalities)) %>% 
  #   ungroup() %>% 
  #   pivot_wider(id_cols = PROVINCE,
  #               names_from = Year,
  #               values_from = c(prov_total_events, prov_total_fatal)) %>% 
  #   mutate(prov_event_change = prov_total_events_2023 - prov_total_events_2022,
  #          prov_event_pctchange = ifelse(prov_total_events_2022 == 0, NA, prov_event_change / prov_total_events_2022),
  #          prov_fatal_change = prov_total_fatal_2023 - prov_total_fatal_2022,
  #          prov_fatal_pctchange = ifelse(prov_total_fatal_2022 == 0, NA, prov_fatal_change / prov_total_fatal_2022)) %>% 
  #   dplyr::select(matches("PROVI|change"))
  
  # political violence fatalities in 2023
  DRC_PV_2023 <- DRC_PV %>% 
    filter(Year == 2023) %>% 
    dplyr::select(PROVINCE, Events, Fatalities) %>% 
    group_by(PROVINCE) %>% 
    summarize(prov_total_events_2023 = sum(Events),
              prov_total_fatals_2023 = sum(Fatalities)) %>% 
    ungroup()
  
  # combine data of political violence with shp file of provinces
  DRC_prov_tomap <- left_join(DRC_HZ_prov,
                              DRC_PV_2023, # look at raw count of PV events or fatalities instead
                              by = "PROVINCE")
  
  # make bivariate classes for HZ count per sqkm and % change in PV event 
  DRC_prov_biclass <- bi_class(DRC_prov_tomap,
                               x = prov_total_events_2023, 
                               y = HZ_per_sqkm, 
                               style = "quantile", dim = 4) %>% 
    st_as_sf()
  
}

# create map ----
{
  # get bi_pal palette colors
  # bi_pal("DkViolet2", dim = 4, preview = F)
  
  grey_color <- "#d3d3d3" # for 1-1
  blue_color <- "#4279b0" # for highest class of HZ/sqkm
  blue_1_3_color <- "#7397bb" # for 1-3
  wine_color <- "#9e3547" # for highest class of prov_event_pctchange
  purple_color <- "#311e3b" # for 4-4
  
  # subtitle with color specification
  annotation_subtitle <- glue("<span style='color:{blue_color};'>**Count of Healthzones (per sq km)**</span> and Count of<br><span style='color:{wine_color};'>**Political Violence (PV) Events**</span> in 2023")
  
  # other plot annotations
  annotation_1_1 <- glue(
  "**Bas-Uele**:\n
  4 reported events of PV\n
  (3 reported fatalities)\n
  11 healthzones"
  )
  annotation_1_3 <- glue(
  "**Sud-Ubangi**:\n
  3 reported events of PV\n
  (2 reported fatalities)\n
  16 healthzones"
  )
  annotation_4_1 <- glue(
  "**Mai-Ndombe**:\n
  31 reported events of PV\n
  (93 reported fatalities)\n
  14 healthzones"
  )
  annotation_4_4 <- glue(
  "**Nord-Kivu**:\n
  1,102 reported events of PV\n
  (1,808 reported fatalities)\n
  34 healthzones"
  )
  
  annotation_HZ <- glue(
  "represents a healthzone"
  )
  
  # bivariate map of healthzone count per sq km and count of political violence events from 2023
  map <- 
    ggplot() +
    
    # add surrounding countries
    geom_sf(data = background_countries,
            fill = "#555555",
            color = "lightgrey",
            lwd = 0.5) +
    
    # add map of the rest of the provinces with biclass coloring
    geom_sf(data = DRC_prov_biclass, 
            aes(fill = bi_class),
            color = "transparent",
            show.legend = F) +
    
    # add border around DRC 
    geom_sf(data = DRC_bounds,
            fill = "transparent",
            color = "lightgrey",
            lwd = 0.5) +
    
    # highlight the provinces with annotations in white 
    geom_sf(data = DRC_prov_biclass %>% 
              filter(PROVINCE %in% c("Bas-Uele","Sud-Ubangi","Maï-Ndombe","Nord-Kivu")), 
            fill = "transparent",
            color = "white",
            lwd = 1,
            show.legend = F) +
    
    # add points for healthzones
    geom_sf(data = DRC_HZ_points, shape = 18, fill = "black") +
    
    # add diamond for annotation
    geom_point(aes(x = -497500, y = 10570000), shape = 18, fill = "black") +
    
    # define color palette for biclass
    bi_scale_fill(pal = "DkViolet2", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
    
    # map limits
    coord_sf(xlim = c(-500000, 1820000),
             ylim = c(8400000, 10800000)) +
    
    # Add the colored annotations with leader lines
    geom_richtext(aes(x = -497500, y = 10720000, label = annotation_subtitle), 
                  color = "black", size = 10/.pt, lineheight = 0.5,
                  hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    geom_richtext(aes(x = -480000, y = 10570000, label = annotation_HZ), 
                  color = "black", size = 8/.pt, lineheight = 0.5,
                  hjust = 0, family = "AppleGothic",
                  fill = NA, label.color = NA) +
    geom_richtext(aes(x = 950000, y = 10685000, label = annotation_1_1), 
                  color = grey_color, size = 5/.pt, lineheight = 0.5,
                  hjust = 0, family = "AppleGothic",
                  fill = alpha(grey_color, 0.2), label.color = NA) +
    geom_richtext(aes(x = -200000, y = 10360000, label = annotation_1_3), 
                  color = blue_1_3_color, size = 5/.pt, lineheight = 0.5,
                  hjust = 0, family = "AppleGothic",
                  fill = alpha(grey_color, 0.2), label.color = NA) +
    geom_richtext(aes(x = -450000, y = 9730000, label = annotation_4_1), 
                  color = wine_color, size = 5/.pt, lineheight = 0.5,
                  hjust = 0, family = "AppleGothic",
                  fill = alpha(grey_color, 0.2), label.color = NA) +
    geom_richtext(aes(x = 1490000, y = 9900000, label = annotation_4_4), 
                  color = purple_color, size = 5/.pt, lineheight = 0.5,
                  hjust = 0, family = "AppleGothic",
                  fill = alpha(grey_color, 0.2), label.color = NA) +
    
    # Title and subtitle using ggtext for colored styling
    labs(title = "Democratic Republic of Congo (DRC) Provinces",
         caption = "
         30-Day Map Challenge\n
         Day 8: Humanitarian Data Exchange (HDX)\n
         Author: Akira Di Sandro\n
         Source: HDX, World Bank Group Data Catalog\n
         R Packages used: tidyverse, sf, biscale, stringr, glue, ggtext, cowplot") +
    
    # adjust theme
    theme_void() +
    theme(plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    margin = margin(0, 0, -20, 10), 
                                    hjust = 0,
                                    family = "AppleGothic"),
          plot.caption = element_text(face = "italic",
                                      size = 5,
                                      hjust = 1,
                                      margin = margin(-40, 20, 5, 0),
                                      lineheight = 0.5,
                                      family = "AppleGothic"),
          plot.background = element_rect(fill = "#96dbe3"),
          panel.background = element_rect(fill = "#96dbe3"))
  
  
  # Legend
  legend <- bi_legend(pal = "DkViolet2",   
                      flip_axes = FALSE,
                      rotate_pal = FALSE,
                      dim = 4,
                      xlab = "% Change in PV events",
                      ylab = "Healthzone Count (per sq km)",
                      size = 10) +
    theme(plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          axis.title.x = element_text(size = 6,
                                      family = "AppleGothic",
                                      face = "bold"),
          axis.title.y = element_text(size = 6,
                                      family = "AppleGothic",
                                      face = "bold"),
          panel.grid.major = element_blank())
  
  # Combine map and legend using cowplot
  map_and_legend <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.1, 0.1, 0.28, 0.28)
  
  # Display the final map with text annotations and leader lines
  map_and_legend
  
  # save image as png
  ggsave("outputs/08-Aki-HDX.png",
         plot = map_and_legend,
         width = 6.5, height = 8.5, units = "in")
  
}
