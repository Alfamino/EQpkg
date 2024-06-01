#library(tidyverse)
#d <- read_delim("earthquakes.tsv")
#d <- d[2:6421,]
##########################
## WEEK 1
##########################
#library(lubridate)
#d$Date <- lubridate::ymd(paste(d$Year, d$Mo, d$Dy, sep = "-"))
#d$Latitude <- as.numeric(d$Latitude)
#d$Longitude <-  as.numeric(d$Longitude)

#library(stringr)


#' Clean and Prepare NOAA Significant Earthquakes dataset
#'
#' @description eq_clean_data cleans and prepares NOAA Significant Earthquakes dataset for further analysis.
#'
#' @param d - a NOAA dataset as data frame.
#'
#' @return The modified data frame.
#'
#' @details This function performs the following cleaning steps:
#'
#' 1. **Date Conversion:** Combines separate year (`Year`), month (`Mo`), and day (`Dy`) columns into a single `Date` column using `lubridate::ymd()`. The date format is assumed to be YYYY-MM-DD.
#' 2. **Numeric Conversion:** Converts the `Latitude` and `Longitude` columns to numeric format using `as.numeric()`.
#'
#' @note It is assumed to have separate columns for year (`Year`), month (`Mo`), and day (`Dy`), as well as `Latitude` and `Longitude` columns (potentially character format).
#' This function assumes specific column names and data formats. Modify the code if your data uses different structures.
#' The `lubridate` package is required for date manipulation.
#'
#' @seealso `lubridate::ymd()`
#'
#' @import lubridate
#'
#' @examples
#' \dontrun{  clean_data <- eq_clean_data(data) }
#'
#' @export
eq_clean_data <- function(d){
  d$Date <- lubridate::ymd(paste(d$Year, d$Mo, d$Dy, sep = "-"))
  d$Latitude <- as.numeric(d$Latitude)
  d$Longitude <- as.numeric(d$Longitude)
  d
}


#' Strip Location Names in NOAA Significant Earthquakes dataset
#'
#' @description eq_location_clean cleans and formats location names in a data frame. The names in orignal dataset cotain Country and City in a singe cell.
#'
#' @param d - a NOAA dataset as data frame.
#'
#' @return The modified data frame (`d`) with a modified dataset.
#'
#' @details This function cleans location names in the `"Location Name"` column, assuming the format might include a city/region followed by a colon (":") and a country name. It performs the following steps:
#'
#' @note Data frame must contain a column named `"Location Name"` with potentially country-suffixed locations.
#' This function assumes the `"Location Name"` column might have locations with or without colons separating city/region and country. It uses the `stringr` package for string manipulation functions.
#'
#' @seealso `strsplit`, `str_trim`, `str_to_title`, `tolower` (from stringr package)
#'
#' @examples
#' \dontrun{ clean_data <- eq_location_clean(data) }
#'
#' @export
eq_location_clean <- function(d){
  d$`Location Name` <- sapply(strsplit(d$`Location Name`, ":"), function(x) {
    if (length(x) > 1) {
      stringr::str_trim(stringr::str_to_title(tolower(x[2])))
    } else {
      stringr::str_trim(stringr::str_to_title(tolower(x)))
    }
  })
  d
}



#' A geom to draw a timeline of earth quakes
#'
#' @description
#' A geom is used to draw many different earth quakes form NOAA Significant Earthquakes data set
#'
#' @param xmin - starting date
#' @param xmax - end date
#'
#' @note x - mandatory the dates of earthquakes, size - the magnitude, fill - the number of deaths
#'
#' @inheritParams ggplot2::geom_point
#'
#' @examples
#' \dontrun{ d %>% filter(COUNTRY %in% c('Poland', 'China')) %>%
#' ggplot(  aes(  x = Date ,
#'                y = COUNTRY,
#'                fill=Deaths ,
#'                size=Mag )) +
#'   geom_timeline( xmin = ymd('1999-01-01'), xmax = ymd('2024-12-31')) }
#'
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,xmin=NULL,xmax=NULL, ...) {
  ggplot2::layer(geom = GeomTimeline, mapping = mapping,
                 data = data, stat = stat, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(xmin=xmin,xmax=xmax,na.rm = na.rm, ...))}


#' Implementation of the GeomTimeline
#'
#' @description
#' A main implementation of the genom
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline",
                                 ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(
                                   fill= "blue",
                                   size=100,
                                   shape = 19,
                                   y=0.1,
                                   alpha=0.2),
                                 draw_key = ggplot2::draw_key_point,
                                 setup_data = function(self, data, params) {
                                   data <- drop_na(data)
                                   data <- dplyr::filter(data,
                                                         x >= params$xmin &
                                                           x < params$xmax)
                                   data
                                 },
                                 draw_panel  = function(data, panel_scales, coord,xmin,xmax) {
                                   coords <- coord$transform(data, panel_scales)

                                   points <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(col = coords$fill,
                                                     fill = coords$fill,
                                                     size = .pt*coords$size*2,
                                                     alpha = coords$alpha
                                     )
                                   )

                                   ys <- unique(coords$y)
                                   lines <- grid::polylineGrob(
                                     x = rep(c(0, 1), each = length(ys)),
                                     y = c(ys, ys),
                                     id = rep(seq_along(ys), 2),
                                     gp = grid::gpar(col = "grey70",lwd = 1)
                                   )
                                   grid::gList(lines, points)

                                 })





#' A geom to draw a timeline witg names of earth quakes
#'
#' @description
#' A geom is used to draw many different earth quakes with names form NOAA Significant Earthquakes data set
#'
#' @param xmin - starting date
#' @param xmax - end date
#' @param n_max - number of Cities to show
#'
#' @inheritParams ggplot2::geom_point
#'
#' @note
#' * x - mandatory the dates of earthquakes
#' * label - mandatory the City name
#' * size - the magnitude
#' * fill - the number of deaths
#'
#' @examples
#' \dontrun{ d %>% filter(COUNTRY %in% c('Poland', 'China')) %>%
#' ggplot(  aes(  x = Date ,
#'                y = COUNTRY,
#'                fill=Deaths ,
#'                size=Mag )) +
#'   geom_timeline( xmin = ymd('2000-01-01'),
#'                  xmax = ymd('2022-12-31') )+
#'   geom_timeline_label( xmin = ymd('1999-01-01'),
#'                        xmax = ymd('2024-12-31'),
#'                        n_max=2 ,
#'                        aes(name=CITY)) }
#'
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE,xmin=NULL,xmax=NULL,n_max=1, ...) {
  ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping,
                 data = data, stat = stat, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(xmin=xmin,xmax=xmax,na.rm = na.rm,n_max=n_max, ...))}


#' Implementation of the GeomTimelineLabel
#'
#' @description
#' A main implementation of the genom
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel",
                                      ggplot2::Geom,
                                      required_aes = c("x","name"),
                                      default_aes = ggplot2::aes(
                                        fill= "blue",
                                        size=100,
                                        shape = 19,
                                        y=0.1,
                                        alpha=0.2),
                                      draw_key = ggplot2::draw_key_point,
                                      setup_data = function(self, data, params) {
                                        data <- drop_na(data)
                                        data <- dplyr::filter(data,
                                                              x >= params$xmin &
                                                                x < params$xmax)

                                        data <- data %>%
                                          dplyr::group_by( y ) %>%
                                          arrange(desc( fill )) %>%
                                          dplyr::top_n( params$n_max , fill )
                                        #print(data)
                                        data
                                      },

                                      draw_panel  = function(data, panel_scales, coord,xmin,xmax,n_max) {
                                        coords <- coord$transform(data, panel_scales)

                                        points <- grid::pointsGrob(
                                          x = coords$x,
                                          y = coords$y,
                                          pch = coords$shape,
                                          gp = grid::gpar(col = coords$fill,
                                                          fill = coords$fill,
                                                          size = .pt*coords$size*2,
                                                          alpha = coords$alpha
                                          )
                                        )

                                        ys <- unique(coords$y)

                                        lines <- grid::polylineGrob(
                                          x = rep(c(0, 1), each = length(ys)),
                                          y = c(ys, ys),
                                          id = rep(seq_along(ys), 2),
                                          gp = grid::gpar(col = "grey70",lwd = 1)
                                        )

                                        lines2 <- grid::polylineGrob(
                                          x = c(coords$x, coords$x),
                                          y = c(coords$y, coords$y + 0.1),
                                          id = rep(1:dim(coords)[1],2),
                                          gp = grid::gpar(col = "light grey",lwd = .pt)
                                        )

                                        names <- grid::textGrob(
                                          label = coords$name,
                                          x = coords$x + 0.01,
                                          y = coords$y + 0.1,
                                          rot = 45,
                                          just = c("left", "bottom"),
                                          gp = grid::gpar(col = "grey",lwd = .pt)
                                        )
                                        grid::gList(lines,lines2,names, points)
                                      })



#library(leaflet)


#' Create Popup Label for Earthquake Data
#'
#' @description eq_create_label generates an HTML formatted text that cotain information about an earthquake event:
#'  * City
#'  * Magnitude
#'  * Number of deaths
#'
#' @param data - a data frame
#'
#' @note Data frame must contain columns for city (CITY), magnitude (Mag), and deaths (Deaths).
#'
#' @return A character vector with HTML formatted labels for each row in the data frame.
#'
#'
#' @examples
#'
#'
#' data <- data.frame(
#'   CITY = c("San Francisco", NA, "Tokyo"),
#'   Mag = c(6.7, NA, 8.3),
#'   Deaths = c(3, NA, 125)
#' )
#'
#' labels <- eq_create_label(data)
#'
#' @export
eq_create_label <- function(data) {
  str1 <- ifelse(is.na(data$CITY), "",paste("<b>Location:</b>",data$CITY))
  str2 <- ifelse(is.na(data$Mag), "",paste("<b>Magnitude</b>", data$Mag))
  str3 <- ifelse(is.na(data$Deaths), "" , paste("<b>Total deaths:</b>",data$Deaths) )
  paste(str1, str2, str3 , sep= "<br>")
}

#' @title Create Leaflet Map for Earthquake Data
#'
#' @description eq_map generates a Leaflet map visualizing earthquake data.
#'
#' @param dane - A data frame
#' @param annot_col -  The name of the column in `dane` containing text for marker popups.
#'
#' @note containing earthquake data, including columns for longitude (`Longitude`), latitude (`Latitude`), magnitude (`Mag`), and an annotation column specified by `annot_col`.
#'
#' @return A Leaflet map object.
#'
#' @import tidyverse
#' @import leaflet
#'
#' @examples
#'
#' \dontrun{ d %>% dplyr::filter(COUNTRY == "Mexico" & lubridate::year(Date) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>% eq_map("popup_text") }
#'
#' @export
eq_map <- function( dane , annot_col ){
  dane$tmp <- unlist(dane[,annot_col])
  dane %>%
    dplyr::mutate(Lon = `Longitude`, Lat = `Latitude`) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      ~Lon, ~Lat,
      radius = ~Mag*1.5,
      popup = paste( dane$tmp ))
}




