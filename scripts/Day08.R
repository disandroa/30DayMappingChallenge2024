# 30-Day Mapping Challenge
# Day 8

# Akira Di Sandro
# start date: 2024-11-09
# last updated: 2024-11-09


# set-up ----
{
  # load packages
  library(tidyverse)
  library(sf)
  library(biscale)
  
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
  
  # download shape file for DRC territories
  # source: https://datacatalog.worldbank.org/search/dataset/0040240/Democratic-Republic-of-the-Congo---Administrative-Boundaries
  DRC_territories <- st_read("https://datacatalogfiles.worldbank.org/ddh-published/0040240/DR0050124/codadmbndaadm2rgc20170711.geojson?versionId=2023-01-19T05:05:13.5848812Z") %>% 
    st_transform(crs = DRC_CRS)
  
}

# data wrangling ----
{
  # group polygons to province level
  DRC_provinces <- DRC_healthzones %>% 
    group_by(PROVINCE) %>% 
    summarize()
  
  # add column for area (in km^2) for each province
  DRC_provinces$area_providence <- as.numeric(st_area(DRC_provinces)) / 1e6
  
  # total count of healthzones by province
  DRC_HZ_prov <- DRC_healthzones %>% 
    st_drop_geometry() %>% 
    group_by(PROVINCE) %>% 
    summarise(HZ_count = n()) %>% 
    left_join(DRC_provinces, by = "PROVINCE") %>% 
    mutate(HZ_per_sqkm = HZ_count / area_providence)
  
  # combine political violence events and fatalities by province and year for 2022 and 2023
  DRC_PV_prov <- DRC_pol_vio %>% 
    filter(Year %in% c(2022, 2023)) %>% 
    dplyr::select(Admin1, Year, Events, Fatalities) %>% 
    rename(PROVINCE = Admin1) %>% 
    group_by(PROVINCE, Year) %>% 
    summarise(prov_total_events = sum(Events),
              prov_total_fatal = sum(Fatalities)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = PROVINCE,
                names_from = Year,
                values_from = c(prov_total_events, prov_total_fatal)) %>% 
    mutate(prov_event_change = prov_total_events_2023 - prov_total_events_2022,
           prov_event_pctchange = ifelse(prov_total_events_2022 == 0, NA, prov_event_change / prov_total_events_2022),
           prov_fatal_change = prov_total_fatal_2023 - prov_total_fatal_2022,
           prov_fatal_pctchange = ifelse(prov_total_fatal_2022 == 0, NA, prov_fatal_change / prov_total_fatal_2022)) %>% 
    dplyr::select(matches("PROVI|change"))
  
  # combine data of political violence with shp file of provinces
  DRC_tomap <- left_join(DRC_HZ_prov,
                         DRC_PV_prov,
                         by = "PROVINCE")
  
  # make bivariate classes for HZ count per sqkm and % change in PV event 
  DRC_biclass <- bi_class(DRC_tomap, x = HZ_per_sqkm, y = prov_event_pctchange, style = "quantile", dim = 4) %>% 
    st_as_sf()
  
}

# create map ----
{
  # idea: highlight the increase in violence in different areas of the DRC
  # increase from 2010-2020
  # increase from 2022-2023
  
  # idea: bivariate map of healthzone count per sq km and change in fatalities from political violence from 2022-2023
  ggplot() +
    geom_sf(data = DRC_healthzones) +
    geom_point(data = DRC_healthzones, aes(x = coordx, y = coordy))
  
}