# plot-voyages.R


###### get_ship_observations function ######

get_voyage_details <- function(ports = NA, from = NA, to = NA, ships = NA, 
                               range_lon = NA, range_lat= NA, range_dates = NA) {
  # INPUT: One or more string or vector for one or more parameters
  #        port is a port2 value (lower case)
  #        from is a port2 value (lower case)
  #        to is a port2 value (lower case)
  #        ships is a ShipName (upper case)
  #        range_dates are strings in form like c("1788-12-01", "1790-01-01")
  #        range_lon are two numbers -180 <= lon <= 180 like c(-12.0, -20.5)
  #        range_lat are two numbers -90 <= lat <= 90 like c(45, 90)

  # input values must be exact strings found in df_voyages
  # 
  # RETURN: dataframe with observations that meet all parameters' criteria
  
  # browser()
  
  df <- df_voyages # from global environment
  
  # silently drop bad function parameter values
  # Note: NA has length 1, and intersect doesn't seem to be consistent in setting either NA or null value
  ports <- intersect(ports, c(unique(df$port_from), unique(df$port_to)))
  if ( length(ports) == 0 || is.na(ports)) {
    ports <- character(0)
  }
  from <- intersect(from, unique(df$port_from))
  if ( length(from) == 0 || is.na(from)) {
    from <- character(0)
  }
  to <- intersect(to, unique(df$port_to))
  if ( length(to) == 0 || is.na(to)) {
    to <- character(0)
  }
  ships <- intersect(ships, c(unique(df$ShipName)))
  if ( length(ships) == 0 || is.na(ships)) {
    ships <- character(0)
  }
  if ( length(range_dates) == 0 || is.na(range_dates)) {
    range_dates <- double(0)
  } else {
    # TODO: add bounds checking and type checking here
  }
  if ( length(range_lon) == 0 || is.na(range_lon)) {
    range_lon <- double(0)
  } else {
    # TODO: add bounds checking here
  }
  if ( length(range_lat) == 0 || is.na(range_lat)) {
    range_lat <- double(0)
  } else {
    # TODO: add bounds checking here
  }
  
  if ( length(ports) + length(ships) + length(from) + length(to) + length(range_lon) + length(range_lat) == 0)  {
    message("Error: No known ports or ship names provided",
            "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    return()
  }
  
  # filter ports
  if ( length(ports) > 0 ) {
    df <- df %>%
      filter(port_from %in% ports | port_to %in% ports) # was VoyageFrom, VoyageTo
  }
  # filter from/to
  if ( length(from) > 0 & length(to) > 0 ) {
    df <- df %>%
      filter(port_from %in% from | port_to %in% to) # & port_from != port_to
  } else {
    if ( length(from) > 0 ) {
      df <- df %>%
        filter(port_from %in% from)
    } else {
      if ( length(to) > 0 ) {
        df <- df %>%
          filter(port_to %in% to)
      }
    }
  }
  # filter ships
  if ( length(ships) > 0 ) {
    df <- df %>%
      filter(ShipName %in% ships)
  }
  # filter by lon/lat
  if ( length(range_lon) == 2 ) {
    df <- df %>%
      filter(longitude >= min(range_lon) & longitude <= max(range_lon))
  } else {
    if ( length(range_lon) != 0 ) { # || !is.na(range_lon)
      message("Warning: range_lon must be two numbers; ignoring range_lon",
              "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    }
  }
  if ( length(range_lat) == 2 ) {
    df <- df %>%
      filter(latitude >= min(range_lat) & latitude <= max(range_lat))
  } else {
    if ( length(range_lat) != 0 ) { # || !is.na(range_lat)
      message("Warning: range_lat must be two numbers; ignoring range_lat",
              "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    }
  }
  #filter dates
  if ( length(range_dates) == 2 ) {
    df <- df %>%
      filter(ObsDate >= min(ymd(range_dates)) & ObsDate <= max(ymd(range_dates)))
  } else {
    if ( length(range_dates) != 0 ) { # || !is.na(range_lat)
      message("Warning: range_dates must be two dates; ignoring range_lat",
              "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    }
  }
  
  if ( nrow(df) == 0 ) {
    message("Error: No recognized ports or ship match the parameters provided",
            "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    return()
  }
  
  df
  
}

###### summarize_ship_observations function ######

summarize_voyages <- function(ports = NA, from = NA, to = NA, ships = NA, 
                              range_lon = NA, range_lat = NA, range_dates = NA) {
  
  df <- get_voyage_details(ports = ports, from = from, to = to, ships = ships, 
                           range_lon = range_lon, range_lat = range_lat, range_dates = range_dates)
  if ( length(df) == 0 || is.na(df) ) {
    return()
  }
  
  my_tbl <- df %>%
    group_by(ShipName, VoyageIni, VoyageFrom, port_from, country_from, VoyageTo, port_to, country_to, Nationality, Company) %>%
    summarize(date_first = min(ObsDate),
              date_last = max(ObsDate),
              n_days = as.integer(difftime(date_last, date_first, units = "days")),
              n_obs = n()) %>%
    ungroup() %>%
    arrange(VoyageIni, ShipName)
  
  my_gt <- my_tbl %>%
    gt() %>%
    tab_source_note(
      source_note = glue("{nrow(distinct(my_tbl, ShipName))} ships and {nrow(my_tbl)} voyages;",
                         " ships from {nrow(distinct(my_tbl, Nationality))} nationalities")
    ) %>%
    opt_table_font(
      font = c(
        "Arial Narrow",
        default_fonts()
      )
    )
 
    my_gt
  
}

###### plot_voyages_function ######

plot_voyages <- function(ports = NA, from = NA, to = NA, ships = NA, 
                         range_lon = NA, range_lat = NA, range_dates = NA, color_var = NA) {
  # INPUT: One or more string or vector for one or more parameters
  #        port is a port2 value (lower case)
  #        from is a port2 value (lower case)
  #        to is a port2 value (lower case)
  #        ships is a ShipName (upper case)
  #        color_var is the variable to use for coloring the voyage lines (can be "nationality" or "year")
  # input values must be exact strings found in df_voyages
  # 
  # RETURN: nothing (so call directly or call with walk() rather than map() )
  # SIDE EFFECT: print one ggplot plot of all voyages to/from the list of ports for relevant ships in the relevant area
  
  ELEMENTS_TITLE_LENGTH <- 4
  ELEMENTS_CAPTION_LENGTH <- 10
  WRAP_TITLE_LENGTH <- 40
  WRAP_CAPTION_LENGTH <- 60
  
  # browser()
  
  df <- get_voyage_details(ports = ports, from = from, to = to, ships = ships, 
                           range_lon = range_lon, range_lat= range_lat, range_dates = range_dates)
  if ( length(df) == 0 || is.na(df) ) {
    return()
  }
  
  # repeat this here, so params are the right length
  # silently drop bad function parameter values
  # Note: NA has length 1, and intersect doesn't seem to be consistent in setting either NA or null value
  ports <- intersect(ports, c(unique(df$port_from), unique(df$port_to)))
  if ( length(ports) == 0 || is.na(ports)) {
    ports <- character(0)  # not sure why this is needed; it's not for ships
  }
  from <- intersect(from, unique(df$port_from))
  if ( length(from) == 0 || is.na(from)) {
    from <- character(0)  # not sure why this is needed; it's not for ships
  }
  to <- intersect(to, unique(df$port_to))
  if ( length(to) == 0 || is.na(to)) {
    to <- character(0)  # not sure why this is needed; it's not for ships
  }
  ships <- intersect(ships, c(unique(df$ShipName)))
  if ( length(ships) == 0 || is.na(ships)) {
    ships <- character(0)  # not sure why this is needed; it's not for ships
  }
  
  if ( length(range_lon) == 0 || is.na(range_lon)) {
    range_lon <- double(0)  # not sure why this is needed; it's not for ships
  } else {
    # TODO: add bounds checking here
  }
  if ( length(range_lat) == 0 || is.na(range_lat)) {
    range_lat <- double(0)  # not sure why this is needed; it's not for ships
  } else {
    # TODO: add bounds checking here
  }
  if ( length(range_dates) == 0 || is.na(range_dates)) {
    range_dates <- double(0)  # not sure why this is needed; it's not for ships
  } else {
    # TODO: add bounds checking and type checking here
  }
   if ( !color_var %in% c("nationality", "year")) {
    color_var <- "nationality" # the default
  }
  
  voyages_for_plot_temp <- df %>%
    group_by(ShipName, VoyageIni, VoyageFrom, VoyageTo, ShipType, Nationality, color_route) %>%
    mutate(voyage_duration_days = as.integer(max(ObsDate) - min(ObsDate))) %>%
    ungroup() %>%
    filter(voyage_duration_days >= MIN_VOYAGE_DURATION) %>%
    mutate(
      direction = if_else(VoyageFrom %in% ports, "origin", "destination"), # TODO: is this too simplistic?
      #direction = if_else(VoyageFrom %in% ports, "A", "B"), # TODO: is this too simplistic?
      direction = if_else(port_from %in% from, "origin", direction),
      direction = if_else(port_to %in% to, "destination", direction),
      VoyageInfo = glue("{year(ymd(VoyageIni))} ({voyage_duration_days}) {str_to_title(VoyageFrom)} - {str_to_title(VoyageTo)}")
    )
  
  ###### make plot strings ######
  
  my_ship_string <- string_collapse(sort(unique(voyages_for_plot_temp$ShipName)), 
                                    ELEMENTS_TITLE_LENGTH)
  
  my_ports_string <- string_collapse(sort(unique(c(voyages_for_plot_temp$VoyageFrom, 
                                                  voyages_for_plot_temp$VoyageTo))), 
                                     ELEMENTS_CAPTION_LENGTH)
  my_ports_from_string <- string_collapse(sort(unique(voyages_for_plot_temp$VoyageFrom)), 
                                          ELEMENTS_TITLE_LENGTH)
  my_ports_to_string <- string_collapse(sort(unique(voyages_for_plot_temp$VoyageTo)), 
                                        ELEMENTS_TITLE_LENGTH)
  
  my_port2_string <- string_collapse(sort(unique(c(voyages_for_plot_temp$port_from, 
                                                   voyages_for_plot_temp$port_to))), 
                                     ELEMENTS_TITLE_LENGTH)
  my_port2_from_string <- string_collapse(sort(unique(voyages_for_plot_temp$port_from)), 
                                          ELEMENTS_TITLE_LENGTH)
  my_port2_to_string <- string_collapse(sort(unique(voyages_for_plot_temp$port_to)), 
                                        ELEMENTS_TITLE_LENGTH)
  
  my_title <- "Voyages"
  
  if ( length(ships)  > 0 ) {
    my_title <- paste0(my_title, glue(" of {my_ship_string}"))
  }
  if ( length(ports) > 0 ) {
    my_title = paste0(my_title, glue(" to/from {my_port2_string}"))
  }
  if ( length(from) > 0 ) {
    my_title <- paste0(my_title, glue(" from {my_port2_from_string}"))
  }
  if ( length(to) > 0 ) {
    my_title <- paste0(my_title, glue(" to {my_port2_to_string}"))
  }
  if ( length(range_lon) == 2 | length(range_lat) == 2 ) {
    my_title <- paste0(my_title, " in area")
  }
  if ( length(range_lon) == 2 ) {
    my_title <- paste0(my_title, " lon(", glue_collapse(range_lon, sep = ", "), ")")
  }
  if ( length(range_lat) == 2 ) {
    my_title <- paste0(my_title, " lat(", glue_collapse(range_lat, sep = ", "), ")")
  }
  # not including date range in the title, since it's in the subtitle
  # if ( length(range_dates) == 2 ) {
  #   my_title <- paste0(my_title, " dates", glue_collapse(range_lat, sep = " - "), ")")
  # }

  my_title <- str_wrap(my_title, WRAP_TITLE_LENGTH) # one last time in case we added lon() or lat()
  
  
  ###### points, lines, bounding box ######
  
  # use points or lines?
  # TODO: would be better if only the voyages crossing 180 use points
  #       or even better, handle the case gracefully and use lines in all cases
  if (min(voyages_for_plot_temp$longitude) < -175 | max(voyages_for_plot_temp$longitude) > 175) {
    # crosses 180 longitude, so leave data as points
    voyages_for_plot <- voyages_for_plot_temp %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = "WGS84") %>%
      st_transform(crs = "+proj=wintri")
  } else {
    # we can cast to lines, since we don't cross 180 longitude
    voyages_for_plot <- voyages_for_plot_temp %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = "WGS84") %>%
      # following st_cast example at
      # https://stackoverflow.com/questions/50908771/create-multilines-from-points-grouped-by-id-with-sf-package
      group_by(ShipName, VoyageInfo, VoyageIni, VoyageFrom, VoyageTo, ShipType, 
               Nationality, color_route, direction) %>%
      summarise(do_union = FALSE) %>%
      st_cast("LINESTRING") %>%
      ungroup() %>%
      st_transform(crs = "+proj=wintri")
  }
  
  # Expand cropping box
  xx <- voyages_for_plot %>%
    st_coordinates()
  #my_xmin = min(xx[, 1]) * 1.1
  if(min(xx[, 1]) > 0) {
    my_xmin = min(xx[, 1]) * 0.91
  } else {
    my_xmin = min(xx[, 1]) * 1.1
  }
  # my_xmax = max(xx[, 1]) * 1.1
  if(max(xx[, 1]) < 0) {
    my_xmax = max(xx[, 1]) * 0.91
  } else {
    my_xmax = max(xx[, 1]) * 1.1
  }
  # my_ymin = min(xx[, 2]) * 1.2
  if(min(xx[, 2]) > 0) {
    my_ymin = min(xx[, 2]) * 0.9
  } else {
    my_ymin = min(xx[, 2]) * 1.2
  }
  # my_ymax = max(xx[, 2]) * 1.2
  if(max(xx[, 2]) < 0) {
    my_ymax = max(xx[, 2]) * 0.9
  } else {
    my_ymax = max(xx[, 2]) * 1.2
  }
  
  # distance of one degree lat is constant; not true for lon
  # my_x_range <- my_xmax - my_xmin 
  my_y_range <- my_ymax - my_ymin
  my_buffer_dist <- 0.01 * my_y_range / 111111 # https://www.quora.com/How-many-meters-is-1-degree-latitude?share=1
  
  my_ports_buffer_temp <- df_ports %>%
    filter(port2 %in% df$port_from | port2 %in% df$port_to) %>%
    distinct(port2, .keep_all = TRUE)
  
  if(!any(is.na(my_ports_buffer_temp$lon)) & !any(is.na(my_ports_buffer_temp$lat))) {
    my_ports_buffer <- 
      st_as_sf(my_ports_buffer_temp, coords = c("lon", "lat")) %>%
      st_set_crs("WGS84") %>%
      st_buffer(dist = my_buffer_dist) %>%
      distinct(geometry, .keep_all = TRUE)
    show_port <- TRUE
  } else {
    my_ports_buffer <- NA
    show_port <- FALSE
  }
  
  ###### plot ######
  
  #color_var <- "year"
  
  p <- ggplot() +
    geom_sf(data = water_outline, fill = "#000000") +  #fill = "#FFFFFF") + #"#f7fbff") +  # "#56B4E950" blue-coloured water  
    geom_sf(data = st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)), color = "gray80", size = 0.25/.pt)+
    geom_sf(data = world, 
            fill = "#E69F00B0", # "tan",   #"sandy brown"  # "#E69F00B0"
            # fill = "#E69F00B0")  + # brown-coloured land
            color = "white smoke", #"snow"
            size = 0.2
    ) +
    { if(show_port) {
      geom_sf(data = my_ports_buffer,
              color = "yellow", fill = "yellow")
    }
    } +
    { if ( str_to_lower(color_var) == "nationality" ) {
      geom_sf(data = voyages_for_plot,
              size = 0.2, alpha = 1, # was size = 0.3
              aes(
                color = color_route, #Nationality,
                linetype = direction) 
      )
    }
    } +
    { if ( str_to_lower(color_var) == "nationality" ) {
      scale_color_identity(labels = str_to_title(color_routes$Nationality),
                           breaks = color_routes$color_route,
                           guide = "legend")
    } 
    } +
    { if ( str_to_lower(color_var) == "year" ) {
      geom_sf(data = voyages_for_plot,
              size = 0.2, alpha = 1, # was size = 0.3
              aes(#color = color_route, #Nationality,
                color = year(VoyageIni),
                linetype = direction)
      )
    }
    } +
    { if ( str_to_lower(color_var) == "year" ) {
      scale_color_gradient2(low = "red", mid = "yellow", high = "lime green",
                            midpoint = 1783)
    }
    } +
    cowplot::theme_map() + 
    # scale_color_identity(labels = str_to_title(color_routes$Nationality),
    #                      breaks = color_routes$color_route,
    #                      guide = "legend") +
    # scale_color_gradient2(low = "red", mid = "yellow", high = "lime green",
    #                       midpoint = 1783) +
    guides(color = guide_legend(override.aes = list(size=4))) +
    theme(
      legend.position = "right",
      # legend.title.align = 0.5,
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      # legend.text = element_text(size = rel(0.6)),
      # plot.caption = element_text(size = rel(0.5), face = "italic")
    ) +
    coord_sf(
      crs = "+proj=wintri",
      datum  = NULL,
      xlim = c(my_xmin, my_xmax),
      ylim = c(my_ymin, my_ymax)
    ) +
    labs(title = my_title,
         subtitle = # Note: in the following distinct() must cast to data.frame to get rid of geometry column, 
           #   so distinct() works as expected
           glue("{nrow(distinct(as.data.frame(voyages_for_plot), ShipName, VoyageIni))} voyages",
                " starting {min(voyages_for_plot$VoyageIni)} to {max(voyages_for_plot$VoyageIni)}"),
         color = NULL, #color = "Nationality:",
         linetype = NULL, #"Direction:",
         caption = str_wrap(glue("Includes the following logbook ports: {my_ports_string}"), WRAP_CAPTION_LENGTH)
    )
  
  p
  
}

###### test ######

# test
# plot_voyages()

voyage_list <- df_voyages %>% 
  filter(port_from == "hampton road" | port_to == "hampton road")

plot_voyages(ships = voyage_list$ShipName,
             from = "hampton road",
             to = "hampton road" 
)

# plot_voyages(ports = c("nouvelle hollande"))
# plot_voyages(
#   ports = "cork",
#   ships = c("ACASTA", "DOVER CASTLE", "GREYHOUND", "ILCHESTER", "AQUILON")
# )
# 
# plot_voyages(
#   ships = df_voyages %>%
#     filter(country_from %in% c("united states") & country_to %in% c("spain") |
#            country_to %in% c("united states") & country_from %in% c("spain")
#     ) %>%
#     distinct(ShipName) %>%
#     pull(ShipName),
#   ports = df_voyages %>%
#     filter(country_from %in% c("united states") & country_to %in% c("spain") |
#              country_to %in% c("united states") & country_from %in% c("spain")
#     ) %>%
#     pull(port_from, port_to)
# ) 
# 
# plot_voyages(
#   ships = df_voyages %>%
#     filter(country_from %in% c("united states") & country_to %in% c("uk") |
#              country_to %in% c("united states") & country_from %in% c("uk")
#     ) %>%
#     distinct(ShipName) %>%
#     pull(ShipName),
#   ports = df_voyages %>%
#     filter(country_from %in% c("united states") & country_to %in% c("uk") |
#              country_to %in% c("united states") & country_from %in% c("uk")
#     ) %>%
#     pull(port_from, port_to)
# ) 
# 
# plot_voyages(
#   ships = df_voyages %>%
#     filter(country_from %in% c("united states") & country_to %in% c("indonesia") |
#              country_to %in% c("united states") & country_from %in% c("indonesia")
#     ) %>%
#     distinct(ShipName) %>%
#     pull(ShipName),
#   ports = df_voyages %>%
#     filter(country_from %in% c("united states") & country_to %in% c("indonesia") |
#              country_to %in% c("united states") & country_from %in% c("indonesia")
#     ) %>%
#     pull(port_from, port_to)
# ) 
# 
# plot_voyages(
#   ships = df_voyages %>%
#     filter(country_from %in% c("cuba") & country_to %in% c("spain") #|
#              #country_to %in% c("mexico") & country_from %in% c("spain)
#     ) %>%
#     distinct(ShipName) %>%
#     pull(ShipName)
#   # ports = df_voyages %>%
#   #   filter(country_from %in% c("cuba") & country_to %in% c("spain") #|
#   #            #country_to %in% c("mexico") & country_from %in% c("spain")
#   #   ) %>%
#   #   pull(port_from, port_to)
# ) 
# # above not working:
# # Error: No recognized ports or ship names provided
# # usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA)
# 
# 
# # from <- NA
# # to <- NA
# # ports <- NA
# # ships <- NA
# # my_ports <- NA
# # my_port2 <- NA
# 
# 
# ports <- NA
# ships <- NA
# to = c("veracruz", "cádiz")
# from = c("montevideo")
# 
# plot_voyages(
#   from = c("montevideo", "havana"),
#   to = c("veracruz", "cádiz")
# )
# 
# to = c("cádiz")
# from = c("montevideo")
# 
# to <- c("veracruz", "cádiz")
# from <- c("veracruz", "cádiz")
# 
# 
# plot_voyages(from = "veracruz", to = "cádiz")
# plot_voyages(to = "veracruz", from = "cádiz")
# 
# plot_voyages(to = c("cádiz"),
#              from = c("montevideo")
# )
# 
# plot_voyages(from = c("cádiz"),
#              to = c("montevideo")
# )
# 
# plot_voyages(to = df_ports %>%
#                filter(country == "united states") %>%
#                distinct(port2) %>%
#                pull(),
#              from = df_ports %>%
#                filter(country == "portugal") %>%
#                distinct(port2) %>%
#                pull()
# )
# 
# plot_voyages(from = df_ports %>%
#                filter(country == "south africa") %>%
#                distinct(port2) %>%
#                pull(),
#              to = df_ports %>%
#                filter(country == "india") %>%
#                distinct(port2) %>%
#                pull()
# )
# 
# plot_voyages(to = df_ports %>%
#                filter(country == "south africa") %>%
#                distinct(port2) %>%
#                pull(),
#              from = df_ports %>%
#                filter(country == "india") %>%
#                distinct(port2) %>%
#                pull()
# )
# 
# # we can see the strategy of finding the right lattitude then moving west to reach St Helena
# plot_voyages(to = "st helena",
#              from = df_ports %>%
#                filter(country == "india") %>%
#                distinct(port2) %>%
#                pull()
# )
# # same when coming from Indonesia
# plot_voyages(to = "st helena",
#              from = df_ports %>%
#                filter(country == "indonesia") %>%
#                distinct(port2) %>%
#                pull()
# )
# 
# # going the other way
# plot_voyages(from = "st helena",
#              to = df_ports %>%
#                filter(country == "indonesia") %>%
#                distinct(port2) %>%
#                pull()
# )
# plot_voyages(from = "st helena",
#              to = df_ports %>%
#                filter(country == "india") %>%
#                distinct(port2) %>%
#                pull()
# )
# 
# plot_voyages(to = c("cádiz", "havana"),
#              from = c("veracruz", "cádiz")
# )
# plot_voyages(to = c("cádiz", "havana"),
#              from = c("cádiz", "havana")
# )
# 
# from <- NA
# to <- NA
# ships <- NA
# ports = "cádiz"
# plot_voyages(ports = "cádiz")
# plot_voyages(ships = "SWALLOW")
# plot_voyages(ports = "madras",
#              ships = "SWALLOW")
# 
# 
# from <- NA
# to <- NA
# ships <- "RESOLUTION"
# ports = NA
# plot_voyages(ports = "wellington",
#              ships = c("RESOLUTION", "ENDEAVOUR")
# )
# 
# plot_voyages(#ports = "wellington",
#              ships = c("ENDEAVOUR")
# )
# 
# plot_voyages(ports = c("cape corse castle", "elmira"))
#              
# ports <- NA
# ships <- NA
# to <- c("veracruz", "cádiz")
# from <- c("veracruz", "cádiz")
# plot_voyages(from = c("veracruz", "cádiz"),
#              to = c("veracruz", "cádiz"))
# 
# plot_voyages(ports = c("dickis cove", "dix cove"))
# 
# plot_voyages(ships = "SCORPION")
# 
# plot_voyages(ports = c("ascension island", "st helena"))
# plot_voyages(to = c("ascension island"),
#              from = c("st helena")
# )
# 
# #from works but not to. Why?
# plot_voyages(to = df_ports %>%
#                filter(country == "chile") %>%
#                distinct(port2) %>%
#                pull() #, 
#              # from = df_ports %>%
#              #   filter(country == "chile") %>%
#              #   distinct(port2) %>%
#              #   pull()
# )
# ports <- NA
# ships <- NA
# from <- NA
# to <- df_ports %>%
#   filter(country == "chile") %>%
#   distinct(port2) %>%
#   pull()
# 
# to_chile <- df_ports %>%
#   filter(country == "cana") %>%
#   distinct(port2) %>%
#   pull()
# 
# df_voyages %>% 
#   filter(port_to %in% to_chile,
#          longitude> -5)
# 
# plot_voyages(from = "bermuda")
# 
# plot_voyages(from = "little popo")
# 
# ports <- NA
# ships <- NA
# from <- NA
# to <- NA
# range_lon <- NA
# range_lat <- NA
# range_lon <- c(-100, -140)
# range_lat <- c(20, 89)
# 
# plot_voyages(range_lon = c(-100, -140),
#              range_lat = c(0, 89)
#              )
# 
# plot_voyages(#range_lon = c(-100, -140),
#              range_lat = c(-60, -89)
# )
# 
# 
# aa <- get_ship_observations(from = "calcutta") %>%
#   group_by(ShipName, VoyageIni, VoyageFrom, port_from, country_from, VoyageTo, port_to, country_to, Nationality, Company) %>%
#   summarize(date_first = min(ObsDate),
#             date_last = max(ObsDate),
#             n_days = as.integer(difftime(date_last, date_first, units = "days")),
#             n_obs = n()) %>%
#   ungroup()
# 
# aa %>%
#   gt() %>%
#   opt_table_font(
#     font = c(
#       "Arial Narrow",
#       default_fonts()
#     )
#   )
# 
# 
# plot_voyages(to = "calcutta", 
#              from = "goa")
# 
# summarize_ship_observations(from = "calcutta", 
#                             to = "goa")
# 
# ship_list <- df_voyages %>%
#   filter(country_from == "sri lanka") %>%
#   distinct(ShipName, port_from, port_to)
# summarize_ship_observations(from = ship_list$port_from, 
#                             to = ship_list$port_to)
# plot_voyages(to = ship_list$port_from#, 
#              #to = ship_list$port_to
#              )
# 
# 
# summarize_ship_observations(from = "calcutta", 
#                             to = "goa")
