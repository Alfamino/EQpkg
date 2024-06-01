# Clean and Prepare NOAA Significant Earthquakes Dataset

This document describes several R functions used to clean and prepare a dataset of significant earthquakes from NOAA. These functions perform data cleaning steps and create visualizations for the earthquakes.

**List of Functions:**

1. **`eq_clean_data`**

   * Cleans and prepares a NOAA dataset by:
       * Combining separate year, month, and day columns into a single date column using `lubridate::ymd()`.
       * Converting latitude and longitude columns to numeric format using `as.numeric()`.

2. **`eq_location_clean`**

   * Cleans and formats location names in a data frame, assuming the format might include a city/region followed by a colon and a country name.
   * Uses `stringr` package for string manipulation functions.

3. **`geom_timeline`**

   * A custom geom used with `ggplot2` to draw a timeline of earthquakes on a chart.
   * It takes arguments like `xmin` and `xmax` to specify the date range for the timeline.

4. **`GeomTimeline` (internal function)**

   * The implementation of `geom_timeline`.
   * It filters data based on the provided date range and draws points on the chart.

5. **`geom_timeline_label`**

   * Another custom geom for `ggplot2` that draws a timeline with earthquake city names on a chart.
   * It allows specifying the number of cities to display using `n_max`.

6. **`GeomTimelineLabel` (internal function)**

   * The implementation of `geom_timeline_label`.
   * It filters data based on the date range and displays city names with limited duplicates.

7. **`eq_create_label`**

   * Generates HTML formatted text for earthquake popups on a map.
   * It combines information like city, magnitude, and number of deaths.

8. **`eq_map`**

   * Creates a Leaflet map to visualize earthquake data.
   * It takes a data frame and the name of the column containing popup text as arguments.
