# 30-Day Mapping Challenge
# Day 1: Philadelphia Trees

# Akira Di Sandro
# start date: 2024-11-01
# last updated: 2024-11-01


# set-up ----
{
  # load packages
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(stringr)
  
  # define CRS
  my_crs <- 'EPSG:2272' # EPSG:2272 - NAD83 / Pennsylvania South (ftUS)
}

# load data ----
{
  philly_trees <- st_read("https://hub.arcgis.com/api/v3/datasets/503632092a0741bb92fbcde496722691_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1") %>% 
    st_transform(my_crs)
  
  # philly_neighborhoods <- st_read("https://github.com/blackmad/neighborhoods/blob/master/philadelphia.geojson")
  # getting the following error: 
  #   Error: Cannot open "https://github.com/blackmad/neighborhoods/blob/master/philadelphia.geojson"; The file doesn't seem to exist.
  # In addition: Warning message:
  # In CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
  #   GDAL Error 4: Failed to read TopoJSON data
  # I tried loading philly neighborhoods from various sources (that indeed exist) but I keep getting this same error
}

# data wrangling ----
{
  # examining data
  {
    # length(unique(philly_trees$tree_name)) # 299 unique tree names
    # sort(table(philly_trees$tree_name)) # from 1-16,335 instances of trees for each tree type
    
    # LOC_X: 	 	  X coordinate value (longitude) in WGS 1984 Decimal Degrees.
    # LOC_Y: 	 	  Y coordinate value (latitude) in WGS 1984 Decimal Degrees.
    # TREE_DBH: 	The diameter at breast height (DBH), in inches for the tree.
    # TREE_NAME: 	Latin name/Common name for the tree.
    # YEAR: 	 	  Year that this data represents.
  }
  
  phl_trees <- philly_trees %>% 
    mutate()
}

# create map ----
{
  # initial map to look at data
  phl_treemap_01 <- philly_trees %>% 
    ggplot() +
    geom_sf()
  
  
}