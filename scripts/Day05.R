# 30-Day Mapping Challenge
# Day 5: A Journey
# Chronicling my eventful winter vacation from December 2022 - January 2023.

# Akira Di Sandro
# start date: 2024-11-05
# last updated: 2024-11-05

# source: https://medium.com/@shaofeij/visualize-your-flight-log-using-r-and-ggplot-e3bf35c5f5e1
{
  library(maps)
  library(ggrepel)
  library(tidyverse)
  library(flightplot) # use airports from this
  
  # airports i care about
  airport_codes <- c("PHL", "ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO")
  
  # make sure typo is fixed in airports
  my_airports <- airports %>% 
    rename(Longitude = Longtitude) %>% 
    dplyr::select(IATA, City, Latitude, Longitude) %>% 
    filter(IATA %in% airport_codes) %>% 
    rbind(data.frame(IATA = "YIA",   # add YIA since it's missing from this dataset
                     City = "Yogyakarta",
                     Latitude = -7.8746,
                     Longitude = 110.1087)) %>% 
    mutate(lon = Longitude + 360*(Longitude < 0),
           code = factor(IATA, levels = airport_codes)) %>% 
    arrange(code) %>% 
    mutate(
      # who was i with in this location?
      companion = c("self", "family", "self", "self", "friends", "friends", "self", "friends", "self"),
      # how long did I spend at this location? (in days)
      duration = c(99, 4, .1, .3, 2, 2, .85, 5, .1), 
      # how long was the flight itself?
      flight_dur = c(2.33, 13.5, 8, 1.25, 1.67, 4.1, 7.75, 13.15, 5.25), 
      # which leg of my trip was it?
      leg = c("trip1", rep("trip2", 3), "trip3", "trip4", "trip5", rep("trip6", 2)), 
      # defining the shape of the airport marker depending on who i was with;
      shape = case_when(companion == "family" ~ 17, # triangle if with family
                        companion == "friends" ~ 18, # diamond if with friends
                        .default = 20), # circle if alone
      # adjust text size according to my duration of stay in each location;
      textsize = case_when(duration <= .5 ~ 3, # 1 if i was there less than half of the day
                           duration <= 1 ~ 3.6, # 1.2 if i was there for more than half the day but no more than one whole day
                           duration <= 2 ~ 4.2, # 1.4 if i was there for more than 1 day but no more than two days
                           .default = 4.8), # 1.6 if i was there for more than 2 days
      # adjust flight color depending on leg of trip
      # flight_col = case_when(leg == "trip1" ~ "darkred",
      #                        leg == "trip2" ~ "coral3",
      #                        leg == "trip3" ~ "darkred",
      #                        leg == "trip4" ~ "coral3",
      #                        leg == "trip5" ~ "darkred",
      #                        leg == "trip6" ~ "coral3"),
      # adjust size of flight according to actual length of flight
      flight_size = case_when(flight_dur <= 5 ~ .3,
                              flight_dur <= 10 ~ .5,
                              .default = .7),
      # specify location of text labels (specifically wanted to move Chicago and SF)
      text_lat = case_when(City == "Chicago" ~ Latitude + 1.5,
                           City == "San Francisco" ~ Latitude - 5,
                           City == "Brisbane" ~ Latitude - 5,
                           City == "Tokyo" ~ Latitude - 2,
                           City == "Manila" ~ Latitude + 1.7,
                           .default = Latitude),
      text_lon = case_when(City == "Chicago" ~ lon + 3,
                           City == "Tokyo" ~ lon + 3,
                           City == "Manila" ~ lon + 4,
                           City == "Jakarta" ~ lon - 3,
                           .default = lon)
      )
  
  # define flights
  my_trips <- data.frame(Departure = c("PHL", "ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO"),
                         Arrival = c("ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO", "PHL"),
                         dep_lon = my_airports$lon,
                         dep_lat = my_airports$Latitude,
                         arr_lon = c(my_airports$lon[-1], my_airports$lon[1]),
                         arr_lat = c(my_airports$Latitude[-1], my_airports$Latitude[1]))
  
  # defining my routes
  my_routes <- my_trips %>% 
    dplyr::select(-c(Departure, Arrival)) %>% 
    cbind(my_airports %>% 
            dplyr::select(
              # flight_col, 
              flight_size))
  
  # adjust direction
  around <- ((pmax(my_routes$dep_lon, my_routes$arr_lon) > 300 & 
                pmin(my_routes$dep_lon, my_routes$arr_lon) < 160) | 
               (pmax(my_routes$dep_lon, my_routes$arr_lon) > 200 & 
                  pmin(my_routes$dep_lon, my_routes$arr_lon) < 60))
  westbound <- (my_routes$arr_lon < my_routes$dep_lon) # Westbound flights
  AdjustDirection <- (westbound != around)
  my_routes$dep_lat[AdjustDirection] <- my_trips$arr_lat[AdjustDirection]
  my_routes$dep_lon[AdjustDirection] <- my_trips$arr_lon[AdjustDirection]
  my_routes$arr_lat[AdjustDirection] <- my_trips$dep_lat[AdjustDirection]
  my_routes$arr_lon[AdjustDirection] <- my_trips$dep_lon[AdjustDirection]
  
  # create map
  {
    # base map
    worldmap <- borders("world2", colour="cornsilk", fill="cornsilk")
    
    my_journey <- ggplot() + 
      worldmap + 
      theme_void() +
      geom_curve(data = my_routes, aes(x = dep_lon, y = dep_lat, xend = arr_lon, yend = arr_lat,
                                       # col = flight_col, 
                                       size = flight_size), 
                 col = "darkred",
                 curvature = -.3) +
      scale_color_identity() +
      scale_size_identity() +
      geom_point(data = my_airports, aes(x = lon, y = Latitude, shape = shape), 
                 col = "coral") + 
      scale_shape_identity() +
      geom_text_repel(data = my_airports, 
                      aes(x = text_lon, y = text_lat, label = City, size = textsize), 
                      col = "coral", segment.color = NA, box.padding = .1, point.padding = .1, 
                      xlim = c(0,360),
                      family = "AppleGothic",
                      fontface = "bold"
                      ) +
      labs(title = "Mapping Out My Winter Vacation\nfrom Dec '22 to Jan '23",
           caption = 
           "30-Day Map Challenge\n
           Day 5: A Journey\n
           Author: Akira Di Sandro\n
           R Packages used: tidyverse, ggrepel, maps, flightplot"
           ) +
      theme(
        plot.title = element_text(face = "bold", 
                                  size = 16, 
                                  margin = margin(60, 0, -100, 80), 
                                  hjust = 0,
                                  color = "coral",
                                  family = "AppleGothic"),
        plot.caption = element_text(face = "italic",
                                    size = 5,
                                    hjust = 1,
                                    color = "coral",
                                    margin = margin(-60, 30, 0, 0),
                                    lineheight = 0.5,
                                    family = "AppleGothic"),
        panel.background = element_rect(fill = "darkslategrey"),
        plot.background = element_rect(fill = "darkslategrey"),
        plot.margin = margin(0, 0, 0, 0)
      )
    
    # save map
    ggsave("outputs/05-Aki-Journey.png",
           plot = my_journey,
           width = 8, height = 5, units = "in")
  }
  
}

# extra code
# things that I tried before landing on my final map

# my initial attempt ----
{
  ## set-up ----
  # {
  #   # load packages
  #   library(tidyverse)
  #   library(flightplot) # r package with handy tools and data for airports + flights
  #   
  # }
  # 
  # # load data ----
  # {
  #   # take a look at available airports
  #   # airports
  #   
  #   # select airports of interest for my plot
  #   # PHL -> ORD -> NRT -> CGK -> YIA -> DPS -> MNL -> BNE -> SFO -> PHL
  #   airport_codes <- c("PHL", "ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO")
  #   
  #   my_airports <- airports %>% 
  #     filter(IATA %in% airport_codes) %>% 
  #     rename(Longitude = Longtitude) %>% 
  #     rbind(data.frame(ID = 99999,  # they didn't have YIA in this dataset so I'm added it in (ID is a random number)
  #                      Name = "Yogyakarta International Airport",
  #                      City = "Yogyakarta",
  #                      Country = "Indonesia",
  #                      IATA = "YIA",
  #                      ICAO = "WAHI",
  #                      Latitude = -7.8746,
  #                      Longitude = 110.1087,
  #                      Altitude = 99, # unsure what altitude is here so 99 is a placeholder
  #                      Timezone = 7,
  #                      DST = "N",
  #                      TZName = "Asia/Jakarta",
  #                      Type = "airport",
  #                      Source = "Aki"))
  #   
  #   
  #   # define the trips that I took on this journey
  #   my_trips <- data.frame(Departure = c("PHL", "ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO"),
  #                          Arrival = c("ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO", "PHL"))
  # }
  # 
  # # create map ----
  # {
  #   plot_flights(my_trips,
  #                crop = F,
  #                land_color = "darkgrey",
  #                water_color = "#96dbe3",
  #                dom_color = "coral",
  #                int_color = "#093d02",
  #                times_as_thickness = T)
  #   
  #   
  # }
  # 
  # # looking into the plot_flights code
  # function (trips, crop = TRUE, land_color = "#f6e8c3", water_color = "aliceblue", 
  #           dom_color = "#3288bd", int_color = "#d53e4f", alpha = 0.5, 
  #           times_as_thickness = TRUE) 
  # {
  #   colnames(trips) <- c("Departure", "Arrival")
  #   my_airports <- tibble::tibble(IATA = unique(c(trips$Departure, 
  #                                                 trips$Arrival)))
  #   my_airports <- inner_join(my_airports, airports, by = "IATA")
  #   trips %<>% arrange_path() %>% group_by(.data$Departure, .data$Arrival) %>% 
  #     summarise(n = n()) %>% ungroup()
  #   trips %<>% inner_join(airports, by = c(Departure = "IATA")) %>% 
  #     inner_join(airports, by = c(Arrival = "IATA")) %>% mutate(International = ifelse(.data$Country.x == 
  #                                                                                        .data$Country.y, FALSE, TRUE))
  #   routes <- geosphere::gcIntermediate(p1 = select(trips, .data$Longtitude.x, 
  #                                                   .data$Latitude.x), p2 = select(trips, .data$Longtitude.y, 
  #                                                                                  .data$Latitude.y), n = 500, breakAtDateLine = TRUE, addStartEnd = TRUE, 
  #                                       sp = TRUE)
  #   routes %<>% sf::st_as_sf() %>% mutate(n = trips$n, int = trips$International)
  #   if (!times_as_thickness) {
  #     routes$n <- 1
  #   }
  #   if (crop == TRUE) {
  #     long_limits <- get_map_border(my_airports$Longtitude, 
  #                                   type = "long")
  #     lat_limits <- get_map_border(my_airports$Latitude, type = "lat")
  #   }
  #   else if (crop == "NA") {
  #     long_limits <- c(-180, -40)
  #     lat_limits <- c(0, 90)
  #   }
  #   else if (crop == "48States") {
  #     long_limits <- c(-131, -61)
  #     lat_limits <- c(23, 51)
  #   }
  #   else {
  #     long_limits <- c(-180, 180)
  #     lat_limits <- c(-90, 90)
  #   }
  #   ggplot() + 
  #     geom_sf(data = world, 
  #             fill = land_color, 
  #             size = 0) + 
  #     theme(panel.background = element_rect(fill = water_color)) + 
  #     geom_sf(data = routes, 
  #             mapping = aes(size = n/2, color = factor(.data$int)), 
  #             linetype = "solid") + 
  #     geom_point(data = my_airports,
  #                aes(x = .data$Longtitude, y = .data$Latitude)) + 
  #     ggrepel::geom_text_repel(data = my_airports, 
  #                              scale_color_manual(values = alpha(c(dom_color, int_color), alpha)) + 
  #                                scale_size_identity() + 
  #                                scale_x_continuous(limits = long_limits, expand = c(0, 0)) + 
  #                                scale_y_continuous(limits = lat_limits, expand = c(0, 0)) + 
  #                                labs(x = NULL, y = NULL) + 
  #                                theme(legend.position = "none")
  # }
}
# that was easy...but i'm not satisfied with the results...i wanna make it prettier

# second attempt ----
{
  # using https://r-graph-gallery.com/how-to-draw-connecting-routes-on-map-with-r-and-great-circles.html for help
  
  ## set-up ----
  # {
  #   # load packages
  #   library(tidyverse)
  #   library(maps)
  #   library(flightplot) # using this package for airports dataframe
  #   library(geosphere)
  #   
  #   # no margins
  #   par(mar=c(0,0,0,0))
  #   
  # }
  # 
  # # create dataset ----
  # {
  #   # select airports of interest for my plot
  #   # PHL -> ORD -> NRT -> CGK -> YIA -> DPS -> MNL -> BNE -> SFO -> PHL
  #   airport_codes <- c("PHL", "ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO")
  #   
  #   my_airports <- airports %>% 
  #     filter(IATA %in% airport_codes) %>% 
  #     rename(Longitude = Longtitude) %>% 
  #     dplyr::select(Name:IATA, Latitude:Longitude) %>% 
  #     rbind(data.frame(Name = "Yogyakarta International Airport", # they didn't have YIA in this dataset so I'm added it in
  #                      City = "Yogyakarta",
  #                      Country = "Indonesia",
  #                      IATA = "YIA",
  #                      Latitude = -7.8746,
  #                      Longitude = 110.1087)) %>% 
  #     mutate(code = factor(IATA, levels = airport_codes)) %>% 
  #     arrange(code)
  #   
  #   # define the trips that I took on this journey
  #   my_trips <- data.frame(Departure = c("PHL", "ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO"),
  #                          Arrival = c("ORD", "NRT", "CGK", "YIA", "DPS", "MNL", "BNE", "SFO", "PHL"),
  #                          dep_lon = my_airports$Longitude,
  #                          dep_lat = my_airports$Latitude,
  #                          arr_lon = c(my_airports$Longitude[-1], my_airports$Longitude[1]),
  #                          arr_lat = c(my_airports$Latitude[-1], my_airports$Latitude[1]))
  #   
  #   # flight lines
  #   {
  #     # function to fix flight connection lines
  #     plot_my_connection = function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  #       inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), 
  #                               n = 50, 
  #                               addStartEnd = T, 
  #                               breakAtDateLine = F)
  #       inter = data.frame(inter)
  #       diff_of_lon = abs(dep_lon) + abs(arr_lon)
  #       if(diff_of_lon > 180){
  #         lines(subset(inter, lon >= 0), ...)
  #         lines(subset(inter, lon < 0), ...)
  #       }else{
  #         lines(inter, ...)
  #       }
  #     }
  #   }
  #   
  #   
  # }
  # 
  # # create map ----
  # {
  #   # world map
  #   maps::map('world',
  #             col = "wheat", fill = T, bg = "#96dbe3", lwd = 0.05,
  #             mar = rep(0, 4), 
  #             border = 0, ylim = c(-90, 90), xlim = c(-180,180) 
  #   )
  #   
  #   # plot flights
  #   # trip 1: PHL -> ORD
  #   # visiting family for christmas
  #   plot_my_connection(my_trips[1, 3], my_trips[1, 4], my_trips[1, 5], my_trips[1, 6], col="slateblue1", lwd=2) # flight length: 
  #   
  #   # trip 2: ORD -> NRT -> CGK -> YIA
  #   # chicago to yogyakarta to meet up with friends
  #   plot_my_connection(my_trips[2, 3], my_trips[2, 4], my_trips[2, 5], my_trips[2, 6], col="slateblue4", lwd=2)
  #   plot_my_connection(my_trips[3, 3], my_trips[3, 4], my_trips[3, 5], my_trips[3, 6], col="slateblue4", lwd=2)
  #   plot_my_connection(my_trips[4, 3], my_trips[4, 4], my_trips[4, 5], my_trips[4, 6], col="slateblue4", lwd=2)
  #   
  #   # trip 3: YIA -> DPS
  #   # second location w/ friends
  #   plot_my_connection(my_trips[5, 3], my_trips[5, 4], my_trips[5, 5], my_trips[5, 6], col="slateblue1", lwd=2)
  #   
  #   # trip 4: DPS -> MNL
  #   # solo 17 hours in manila
  #   plot_my_connection(my_trips[6, 3], my_trips[6, 4], my_trips[6, 5], my_trips[6, 6], col="slateblue4", lwd=2)
  #   
  #   # trip 5: MNL -> BNE
  #   # visiting another friend
  #   plot_my_connection(my_trips[7, 3], my_trips[7, 4], my_trips[7, 5], my_trips[7, 6], col="slateblue1", lwd=2)
  #   
  #   # trip 6: BNE -> SFO -> PHL
  #   # back to reality
  #   plot_my_connection(my_trips[8, 3], my_trips[8, 4], my_trips[8, 5], my_trips[8, 6], col="slateblue4", lwd=2)
  #   plot_my_connection(my_trips[9, 3], my_trips[9, 4], my_trips[9, 5], my_trips[9, 6], col="slateblue4", lwd=2)
  #   
  #   # plot airports
  #   points(x = my_airports$Longitude, 
  #          y = my_airports$Latitude, 
  #          col = "coral", 
  #          cex = 0.75,
  #          pch = c(20, 17, 20, 20, 18, 18, 20, 18, 20) # shape corresponds to who i was there with (20 = circle = alone; 17 = triangle = with family; 18 = diamond = with friends)
  #   )
  #   
  #   # add text
  #   text(my_airports$City, 
  #        x = my_airports$Longitude, 
  #        y = my_airports$Latitude, 
  #        col="chocolate4", 
  #        cex = c(1, 1, 0.5, 0.5, 0.8, 0.8, 0.65, 1, 0.5),  # size corresponds to amount of time spent there (0.5 = only stayed in airport; 0.65 = spent <= 1 day there; 0.8 = spent <= 2 days there; 1 spent > 2 days there)
  #        pos = c(4, 3, 2, 2, 1, 4, 4, 1, 1)) # arrange text so that they don't overlap
  #   
  # }
}

# successful attempt at a pacific centered world map ----
{
  # library(tidyverse)
  # library(sf)
  # library(rnaturalearth)
  # 
  # worldMap <- ne_countries(scale = "medium", returnclass = "sf") %>%
  #   st_make_valid()
  # 
  # target_crs <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=133")
  # 
  # # define a long & slim polygon that overlaps the meridian line & set its CRS to match
  # # that of world
  # # Centered in lon 133
  # 
  # offset <- 180 - 133
  # 
  # polygon <- st_polygon(
  #   x = list(rbind(
  #     c(-0.0001 - offset, 90),
  #     c(0 - offset, 90),
  #     c(0 - offset, -90),
  #     c(-0.0001 - offset, -90),
  #     c(-0.0001 - offset, 90)
  #   ))) %>%
  #   st_sfc() %>%
  #   st_set_crs(4326)
  # 
  # # modify world dataset to remove overlapping portions with world's polygons
  # world <- worldMap %>% 
  #   st_difference(polygon) %>% 
  #   st_transform(crs = target_crs)
  # 
  # # test to see if it's plotting with pacific in the center
  # ggplot(data = world, aes(group = admin)) +
  #   geom_sf(fill = "grey")
  # 
  # 
  # world_cropped <- st_crop(world,
  #                          st_as_sfc(
  #                            st_bbox(
  #                              c(xmin= 90, ymin = -40, xmax = 300, ymax = 80), crs = 4326)
  #                          ) %>% 
  #                            st_transform(target_crs)
  # )
  # 
  # ggplot(data = world_cropped, aes(group = admin)) +
  #   geom_sf(fill = "wheat") +
  #   theme_void() +
  #   theme(
  #     panel.background = element_rect(fill = "#96dbe3"),
  #     plot.margin = margin(0, 0, 0, 0)
  #   )
  
}

