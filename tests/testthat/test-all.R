library(ggplot2)
library(leaflet)
library(tidyverse)

test_that("eq_clean_data converts date format", {
  sample_data <- data.frame(
    Year = c(2023, 2023, 2023),
    Mo = c(1, 2, 3),
    Dy = c(10, 11, 12),
    Latitude = c(40.71, -33.87, 35.68),
    Longitude = c(-74.00, 151.21, 139.69)
  )

  expected <- data.frame(
    Date = as.Date(c("2023-01-10", "2023-02-11", "2023-03-12")),
    Latitude = c(40.71, -33.87, 35.68),
    Longitude = c(-74.00, 151.21, 139.69)
  )

 cleaned_data <- eq_clean_data(sample_data)

 expect_equal(cleaned_data$Date, expected$Date)
 expect_true(all(sapply(cleaned_data[, c("Latitude", "Longitude")], is.numeric)))
})


test_that("eq_location_clean cleans location names", {
  #library(tidyverse)
  sample_data <- tibble(`Location Name` = c("Tokyo: Japan", "Lisbon", "New York City: USA"))

  expected <- tibble(`Location Name` = c("Japan", "Lisbon", "Usa"))
  cleaned_data <- eq_location_clean(sample_data)
  expect_equal(cleaned_data$`Location Name`, expected$`Location Name`)
})



test_that("geom_timeline draws points on timeline", {
  #library(ggplot2)

  sample_data <- expected <- data.frame(
    Mag = c(10,10,10),
    Date = as.Date(c("2023-01-10", "2023-02-11", "2023-03-12")),
    Latitude = c(40.71, -33.87, 35.68),
    Longitude = c(-74.00, 151.21, 139.69)
  )

  p <- ggplot(sample_data, aes(x = Date, size = Mag)) +
    geom_timeline(xmin = ymd("2000-01-01"), xmax = ymd("2024-12-31"))

  expect_s3_class(p, 'ggplot')
  expect_length( ggplot2::layer_data(p)$x , 3 )
})



test_that("geom_timeline_label draws points on timeline", {
  #library(ggplot2)

  sample_data <- expected <- data.frame(
    CITY = c("Krakow", "Lisbon", "Tuskolasy"),
    Mag = c(10,10,10),
    Date = as.Date(c("2023-01-10", "2023-02-11", "2023-03-12")),
    Latitude = c(40.71, -33.87, 35.68),
    Longitude = c(-74.00, 151.21, 139.69)
  )

  p <- ggplot(sample_data, aes(x = Date, y=CITY,fill=Mag,size = Mag)) +
    geom_timeline(xmin = ymd("2000-01-01"), xmax = ymd("2024-12-31")) +
    geom_timeline_label( xmin = ymd('1999-01-01'),
                         xmax = ymd('2024-12-31'),
                         n_max=2 ,
                         aes(name=CITY))

  expect_s3_class(p, 'ggplot')
  expect_length( ggplot2::layer_data(p)$x , 3 )
})




test_that("geom_timeline_label draws points on timeline", {
  #library(leaflet)

  sample_data <- data.frame(
    CITY = c("Krakow", "Lisbon", "Tuskolasy"),
    Mag = c(10,10,10),
    Date = as.Date(c("2023-01-10", "2023-02-11", "2023-03-12")),
    Latitude = c(40.71, -33.87, 35.68),
    Longitude = c(-74.00, 151.21, 139.69),
    Deaths = c(3, NA, 125)
  )

  p <- eq_create_label(sample_data)
  expected <- c("<b>Location:</b> Krakow<br><b>Magnitude</b> 10<br><b>Total deaths:</b> 3",
                "<b>Location:</b> Lisbon<br><b>Magnitude</b> 10<br>",
                "<b>Location:</b> Tuskolasy<br><b>Magnitude</b> 10<br><b>Total deaths:</b> 125")
  expect_equal(p, expected)

})



test_that("geom_timeline_label draws points on timeline", {
  #library(leaflet)

  sample_data <- expected <- data.frame(
    CITY = c("Krakow", "Lisbon", "Tuskolasy"),
    Mag = c(10,10,10),
    Date = as.Date(c("2023-01-10", "2023-02-11", "2023-03-12")),
    Latitude = c(40.71, -33.87, 35.68),
    Longitude = c(-74.00, 151.21, 139.69)
  )

  p <- eq_map(sample_data, 'Date')

  expect_s3_class(p, 'leaflet')
  expect_s3_class(p, 'htmlwidget')
})
