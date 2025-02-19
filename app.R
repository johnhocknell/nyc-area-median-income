# File:   app.R
# Author: John Hocknell
# Date:   Fall 2024
# Description:
  # Creates an interactive Shiny map app analyzing area median incomes in NYC 
  # by allowing users to use a widget to select from the following geographic 
  # scales: county, community district tabulation area (CDTA), zip code
  # tabulation area (ZCTA), and Census tract. Data is acquired through
  # Google Sheets for faster performance and persistent storage.

library(tidyr)
library(dplyr)
library(shiny)
library(shinyBS)
library(bslib)
library(sf)
library(leaflet)
library(classInt)
library(googlesheets4)
library(htmlwidgets)

# Put googlesheets4 into a de-authorized state to access public Google Sheets
gs4_deauth()


# ==============================================================================
# Define functions


read_income_sheet <- function(link) {
  return(read_geom_sheet(link) %>%
           mutate_at(vars(mhi, mfi), as.numeric)) # Convert incomes to numeric
}

# Function to read in and clean income data from a linked Google Sheet
read_geom_sheet <- function(link) {
  
  sheet <- read_sheet(link, trim_ws = FALSE) # Read Sheet using goolesheets4
  sheet[sheet == "NA"] <- NA # Convert NA text to actual NA
  sheet <- sheet %>%
    select(-1) %>%
    combine_geometry_text() %>%
    st_cast("MULTIPOLYGON")
  
  if (any(c("mhi", "mfi") %in% names(sheet))) {
    sheet <- sheet %>% mutate_at(vars(mhi, mfi), as.numeric)
  }
  
  return(sheet)
}

# Function to combine split geometry text strings and convert to sf object
combine_geometry_text <- function(split_dataframe) {
  
  result_df <- split_dataframe
  
  # Identify columns that are part of geometry text splits
  split_cols <- grep("^geometry_part\\d+", names(split_dataframe))
  
  # If no split columns exist, return the original dataframe
  if (length(split_cols) == 0) {
    result_df <- result_df %>%  
      st_as_sf(wkt = "geometry", crs = 4326) %>%
      st_make_valid()
    
    return(result_df)
  }
  
  # Combine split text columns for each row
  for (i in 1:nrow(result_df)) {
    # Collect all non-NA parts for this row
    text_parts <- na.omit(as.character(result_df[i, split_cols]))
    
    # Combine the parts if any exist
    if (length(text_parts) > 0) {
      combined_text <- paste(text_parts, collapse = "")
      
      # Set the combined text to the geometry column
      result_df$geometry[i] <- combined_text
      
      # Remove the split columns
      result_df <- result_df[, !names(result_df) %in% split_cols]
    }
  }
  
  # Remove split columns and convert to sf object
  result_df <- result_df %>% 
    select(-all_of(split_cols)) %>%   
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    st_make_valid()
  
  return(result_df)
}

# Function for customizing the legend label and shape for borough borders
addLegendCustom <- function(map, color, labels, borders){
  
  legend_color <- paste0(color, "; width: 15px; height: 0px; border:2px solid ", borders, "; margin-top: 18px")
  legend_label <- paste0("<div style='display: inline-block;height: 0px;margin-top: 4px;line-height: 0px; margin-top: 20px'>", labels, "</div>")
  
  return(addLegend(map, colors = legend_color, labels = legend_label, opacity = 0.7))
}

# Function for reading and cleaning geometries from NYC DCP found at
# https://www.nyc.gov/site/planning/data-maps/open-data/districts-download-metadata.page
read_geom <- function(link) {
  
  return(st_read(link) %>%
           select(-c("OBJECTID", "Shape__Area", "Shape__Length")) %>%
           st_transform(4326))
}

# Function for joining geometries to income data based on GEOID 
geom_join <- function(income, geometry) {
  
  return(left_join(income,
                   geometry[, c("GEOID", "geometry")],
                   join_by(GEOID == GEOID)) %>%
           st_as_sf(crs = 4326) %>%
           st_make_valid())
}

# Function for handling income data in the hover labels
label_income <- function(data) {
  
  if (is.na(data)) {
    return("Not available")
  } else if(data == 250001) {
    return("$250,000 or more")
  } else if(data == 200000) {
    return("$200,000 or more")
  }
  
  return(paste0("$", prettyNum(prettyNum(data, big.mark = ","))))
}

# Function for comparing MHI and AMI that returns text with a descriptive
# comparison
income_comparison <- function(difference) {
  
  if (is.na(difference)) {
    return("No or partial data availabile: the area median income<br/>
           and the median household income cannot be compared.")
  }
  
  txt <- "HUD's area median income is<strong>" 
  
  if (difference == 0) {
    print("ZERO")
    txt <- paste0(txt, " the same as")
  } else if (difference > 0) {
    print("POSITIVE")
    txt <- sprintf("%s $%s greater than",
                   txt,
                   prettyNum(prettyNum(difference, big.mark = ",")))
    
  } else if (difference < 0) {
    print("NEGATIVE")
    txt <- sprintf("%s $%s less than",
                   txt,
                   prettyNum(prettyNum(abs(difference), big.mark = ",")))
  }
  
  txt <- paste0(txt, "</strong><br/>the median household income for this area.")
  
  return(txt)
}

# Function for creating a leaflet map of income data
create_lfmap <- function(data, title, borderWeight) {
  
  # Define a color palette using Fisher-Jenks
  pal <- colorBin(palette = "Greens",
                  bins = classIntervals(data$mhi, n = 5, style = "fisher")$brks,
                  pretty = FALSE)
  
  # Create custom hover label text
  labels <- sprintf("<span style='font-size: 20px'><b>%s</b></span><br/><br/>
  
  
                    <table>
                      <tr>
                        <td>2024 median <i>household</i> income:</td>
                        <td>&emsp;<b style='color:#74C476 !important;'>%s</b></td>
                      </tr>
                      <tr>
                        <td>2024 area median income:</td>
                        <td>&emsp;<strong>$155,300</strong</td>
                      </tr>
                    </table><br/>
  
  
                    %s<br/><br/>
                    
                    <span style='font-size: 12px'>* HUD estimates AMI for 2024 using 2022 data that is adjusted for<br/>
                    &nbsp;&nbsp;&nbsp;inflation by a factor of %s. The same adjustment is made for 2024 MHI.</span>",
                    data$NAME,
                    lapply(floor(data$mhi * inflation_factor), label_income),
                    lapply((155300 - data$mhi), income_comparison),
                    inflation_factor) %>%
    lapply(htmltools::HTML)
  
  # Create the leaflet map
  map <- leaflet(data = data,
                 options = leafletOptions(
                   zoomControl = FALSE,
                   zoomSnap = 0.25,
                   zoomDelta = 1)) %>%
    addProviderTiles("CartoDB.DarkMatter") %>%
    addMapPane("polygons", zIndex = 410) %>% # polygons below
    addMapPane("borough_borders", zIndex = 420) %>% # borders on top
    addPolygons(fillColor = ~pal(mhi), # add polygons using custom pal
                weight = borderWeight,
                opacity = 1,
                color = "black",
                dashArray = "7",
                fillOpacity = 0.65,
                label = labels, # add custom hover labels
                options = pathOptions(pane = "polygons"),
                highlightOptions = highlightOptions(weight = 5,
                                                    color = "white",
                                                    dashArray = "",
                                                    fillOpacity = 1,
                                                    bringToFront = TRUE),
                labelOptions = labelOptions(textsize = "15px",
                                            direction = "auto",
                                            style = list(
                                              "font-weight" = "normal",
                                              padding = "3px 8px"))) %>%
    addPolygons(data = boroughs, # add borough borders
                fill = FALSE,
                stroke = TRUE,
                color = "white",
                opacity = 1,
                weight = 2,
                options = pathOptions(pane = "borough_borders")) %>%
    addLegend(pal = pal,
              values = ~mhi,
              opacity = 0.7,
              title = title,
              position = "bottomright",
              na.label = "Not available",
              labFormat = labelFormat(prefix = '', # remove decimals
                                      suffix = '',
                                      between = ' &ndash; ',
                                      digits = 0,
                                      big.mark = ',',
                                      transform = identity)) %>%
    addLegendCustom(color = "white",
                    labels = "Boroughs",
                    borders = "white")
  
  # Ensure the NA label is directly below the income labels
  if(anyNA(data$mhi) == TRUE) {
    map$x$calls[[6]]$args[[1]]$colors <-
      c(map$x$calls[[6]]$args[[1]]$colors, map$x$calls[[6]]$args[[1]]$na_color)
    map$x$calls[[6]]$args[[1]]$labels <-
      c(map$x$calls[[6]]$args[[1]]$labels, map$x$calls[[6]]$args[[1]]$na_label)
    map$x$calls[[6]]$args[[1]]$na_color <- NULL
    map$x$calls[[6]]$args[[1]]$na_label <- NULL
  }
  
  # Append the borough borders legend to the income labels legend
  map$x$calls[[6]]$args[[1]]$colors <-
    c(map$x$calls[[6]]$args[[1]]$colors, map$x$calls[[7]]$args[[1]]$colors)
  map$x$calls[[6]]$args[[1]]$labels <-
    c(map$x$calls[[6]]$args[[1]]$labels, map$x$calls[[7]]$args[[1]]$labels)
  map$x$calls[[7]] <- NULL # remove original boroughs legend

  return(map)
}


# ==============================================================================
# Read in the income data for each geographic scale. Data acquired in get_data.r
# using get_acs(), but is too slow for the Shiny app, so the the data is
# uploaded to and accessed by public Google Sheets.
nyc_county_income <- read_geom_sheet("https://docs.google.com/spreadsheets/d/1WjfpcZ_c6GDb2efmdcz-F5RwDhzScaJpRmTGaqooE40/edit?usp=sharing")
nyc_cdta_income <- read_geom_sheet("https://docs.google.com/spreadsheets/d/1s6xcjuW7EzNJfU163FQrecQqMlsKmsoisH5vxjrUuCg/edit?usp=sharing")
nyc_tract_income <- read_geom_sheet("https://docs.google.com/spreadsheets/d/1gQ0pIS3tfD9V3tNNQ31kgbBCWPrReJkikZ5x56hCwjg/edit?usp=sharing")
nyc_zcta_income <- read_geom_sheet("https://docs.google.com/spreadsheets/d/1Ro_UjcWyV3lJ2NvdZdPmYpFVl_sCoLJ6dajtzj4vOLI/edit?usp=sharing")

# Read in borough boundaries data
boroughs <- read_geom_sheet("https://docs.google.com/spreadsheets/d/1AP6rFlhWzBbJ86OGEsd-robXLcgh9O9D5ttEk7EE7js/edit?usp=sharing")

# ==============================================================================
# Read in NYC Housing and Vacancy Survey data to create custom adjustments. Data
# acquired from https://www.nyc.gov/site/hpd/about/research.page and accessed
# through Google sheets for performance

borough_IDs <-
  data.frame(
    "GEOID" = c("36005", "36047", "36061", "36081", "36085"),
    "boroCode" = c("2", "3", "1", "4", "5"))

hhSize_adjustments <- read_sheet("https://docs.google.com/spreadsheets/d/1LLZzbosZxWI2c5f4b3lESAcn8Y33P_T6_TkZA2XrwOA/edit?usp=sharing",
                                 trim_ws = FALSE) %>%
  group_by(BORO) %>%
  summarise(multipliers = list(as.numeric(mltplr))) %>%
  ungroup() %>%
  left_join(borough_IDs[, c("boroCode", "GEOID")], # join GEOIDs
            relationship = "many-to-one",
            join_by(BORO == boroCode))

# Common percentages used for AMI
ami_percents <- data.frame(
  "percent" = c("30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%", "110%", "120%", "130%", "165%"),
  "multiplier" = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.65))
hhsize_text <- c("Household Size", "1", "2", "3", "4", "5", "6", "7", "8")

# Calculate the adjustments for each county by multiplying its MHI by its
# adjustment multiplier, then calculate a matrix/chart of AMI incomes by
# multiplying ami_percents by adjustments
nyc_county_income <- nyc_county_income %>%
  left_join(hhSize_adjustments[c("GEOID", "multipliers")], by = "GEOID") %>%
  mutate(adjustments = mapply(function(mhi, multipliers) {
    mhi * multipliers
  }, mhi, multipliers, SIMPLIFY = TRUE)) %>%
  mutate(MHI_chart = mapply(function(adjustments) {
    t(ami_percents$multiplier %*% t(adjustments))
  }, adjustments, SIMPLIFY = TRUE))

# HUD estimates the current year's AMI using ACS surveys from previous years
# by adjusting for inflation between them. For 2024, HUD used the 2022 ACS. The
# CPI inflation factor between 2022 and 2024 is 1.0618
inflation_factor <- 1.0618

# ==============================================================================
# Create the Shiny app 

# Define UI
ui <- fillPage(
  
  # Fill the window with the leaflet map
  leafletOutput("map_output", width = "100%", height = "100%"),
  
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,
    top = 12,
    left = "auto",
    right = 50,
    bottom = "auto",
    width = 250,
    height = "auto",
    style = "opacity: 0.9",
    div(
      style = "display:flex;  margin-top:10px; margin-left: 20px; margin-right: 20px; justify-content:left; align-items:center",
      selectInput(
        "selected_map",
        "Select a geographic scale:",
        choices = list(
          "County" = "a",
          "Community District" = "b",
          "ZIP Code" = "c",
          "Census Tract" = "d"
        )
      )
    )),
  
  # Create a moveable panel
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,
    top = 10,
    left = 10,
    width = "400px",
    height = "95%",
    style = "opacity: 0.9; overflow: auto",
    div(style = "display:inline-block; margin-left: 10px; margin-right: 10px; justify-content:left; align-items:center",
        h3("Exploring Area Median Income in NYC",
           style = "font-weight: bold")),
    div(style = "display:inline-block; margin-left: 15px; margin-right: 10px; justify-content:left; align-items:center;",
        h5("Area median income, known as AMI, commonly refers to the income limits that determine eligibility for affordable housing. For 2024, the 100% AMI in New York City for a family of four is $155,300. This interactive map explores alternative ways of measuring AMI, seeking to improve it by addressing some of its criticisms.")),
    div(style = "display:inline-block; margin-left: 10px; margin-right: 10px; justify-content:left; align-items:center;",
        h5("While AMI should be a simple calculation – the income amount that splits the incomes of an area's population in half – the income limits used to determine affordable housing eligibility are based on convoluted calculations made by the US Department of Housing and Urban Development. The AMI formula is based on median family incomes from the American Community Survey (ACS), whose definition of family excludes one-person households and households with unrelated members. For New York City, AMI is calculated using a wider metropolitan region that includes Westchester, Putnam, and Rockland counties, further misrepresenting median incomes by including wealthier suburban areas. Then, a high housing cost adjustment is made for areas with high rents, which greatly inflates AMI. Finally, another adjustment is made based on the differences in income between family sizes. These are the most common criticisms of AMI, and they all point to how its calculation does not accurately convey the actual median incomes of New Yorkers.")),
    div(style = "display:inline-block; margin-left: 10px; margin-right: 10px; justify-content:left; align-items:center;",
        h5(HTML("Interact with the map by using the drop down list to select different geographic levels to explore incomes that more accurately reflect local areas. Hover over the map to view the median income by household and family to understand how these distinctions influence AMI. These incomes come directly from the ACS and do not include the high housing cost adjustment. For more information, <a href='https://docs.google.com/document/d/1uNfp_t-AXRNXNBiv6tWZc1Uw5TUGdHiScmDA6CLcnHo/edit?usp=sharing'>check out this paper</a> that disucsses this application and how AMI is calculated in depth."))),
    div(style = "display:inline-block; margin-left: 10px; margin-right: 10px; justify-content:left; align-items:center;",
        h5("Made by John Hocknell, BA Geography at CUNY Hunter College with the help and advising of Dr. Jochen Albrecht."))  
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression creates and returns the selected map
  # bindCache caches the map so it is only created once, improving performance
  selectedMap <- reactive({
    switch(
      input$selected_map,
      "a" = create_lfmap(nyc_county_income,
                         "Median Household Income</br>by County", 2),
      "b" = create_lfmap(nyc_cdta_income,
                         "Median Household Income</br>by Community District (CDTA)", 1),
      "c" = create_lfmap(nyc_zcta_income,
                         "Median Household Income</br>by Zip Code (ZCTA)", 1),
      "d" = create_lfmap(nyc_tract_income,
                         "Median Household Income</br>by Census Tract", 0.75)
    )
  }) %>%
    bindCache(input$selected_map)
  
  # Render the selected Leaflet map
  output$map_output <- renderLeaflet({
    selectedMap() %>% # Call the reactive expression to render the correct map
      onRender(
        "function(el, x) {
          L.control.zoom({position:'topright'}).addTo(this);
        }"
      )
  }) %>%
    bindCache(selectedMap()) # caches the reactive expression for performance

}

# Run the application
shinyApp(ui = ui, server = server)