# File:         get_data.R
# Author:       John Hocknell
# Date:         Fall 2024
# Description:
  # Acquires, cleans, formats, and saves median income data with geometries for 
  # efficient access by the interactive Shiny app. Median family income (MFI) 
  # and median household income (MHI) are sourced from the Census 2022 American
  # Community Survey.


library(tidyr)
library(dplyr)
library(shiny)
library(sf)
library(leaflet)
library(classInt)
library(googlesheets4)
library(geojsonio)
library(openxlsx)
library(tidycensus)
library(zctaCrosswalk)
library(tigris)
library(htmlwidgets)
options(tigris_use_cache = TRUE)


# ==============================================================================
# Define functions

# Function for cleaning data acquired from tidycensus get_acs()
clean_acs <- function(data) {
  
  return(data %>%
           select(-moe) %>%
           pivot_wider(names_from = variable, values_from = estimate))
}

# Function to split geometry column into strings of max length 50,000 to work 
# with Google Sheets max cell character count
split_geometry_text <- function(sf_dataframe, max_length = 50000) {
  # Create a copy of the dataframe to modify
  result_df <- sf_dataframe %>%
    mutate(geom_txt = st_as_text(geometry)) %>%
    st_drop_geometry() %>%
    rename(geometry = geom_txt)
  
  # Identify rows that exceed the max length
  long_polygon_indices <- which(nchar(result_df$geometry) > max_length)
  
  # If no long polygons, return the original dataframe with text column
  if (length(long_polygon_indices) == 0) {
    return(result_df)
  }
  
  # Process each long polygon
  for (idx in long_polygon_indices) {
    # Get the original multipolygon text
    full_text <- result_df$geometry[idx]
    
    # Calculate number of splits needed
    num_splits <- ceiling(nchar(full_text) / max_length)
    
    # Create split columns
    for (split_num in 1:num_splits) {
      start_pos <- (split_num - 1) * max_length + 1
      end_pos <- min(split_num * max_length, nchar(full_text))
      
      # Create column name
      col_name <- paste0("geometry_part", split_num)
      
      # Add the column to the result dataframe
      result_df[idx, col_name] <- substr(full_text, start_pos, end_pos)
    }
    
    # Remove the original long text column
    result_df$geometry[idx] <- NA
  }
  
  return(result_df)
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

# ==============================================================================
# Get ACS income data for each geographic scale

year <- 2022 # year of interest
income_vars <- c(mhi = "DP03_0062", mfi = "DP03_0086") # names of ACS vars

# Borough equivalents for filtering and easier joining
borough_IDs <-
  data.frame(
    "GEOID" = c("36005", "36047", "36061", "36081", "36085"),
    "county_FIPS" = c(005, 047, 061, 081, 085),
    "borough" = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
    "boroCode" = c("2", "3", "1", "4", "5"),
    "abbreviation" = c("BX", "BK", "MN", "QN", "SI"))

# GEOIDs of adjacent counties ()
adjacent_GEOIDs <- data.frame(
  "GEOID" = c("36059", "36119", "34003", "34017", "34039", "34023", "34025"),
  "county" = c("Nassau", "Westchester", "Bergen", "Hudson", "Union", "Middlesex", "Monmouth"))

# Get ACS income by county for NYC
nyc_county_income <- get_acs(
  survey = "acs5",
  geography = "county",
  state = 36,
  county = borough_IDs$county_FIPS,
  variables = income_vars,
  year = year,
  geometry = TRUE, # get geometry (Water included)
  cb = FALSE # use core TIGER/Line shapefiles for shoreline mapping
) %>%
  clean_acs()

# Dissolve NYC counties and remove geometry column from nyc_county_income
county_dissolve <- nyc_county_income %>%
  select(c(GEOID, geometry)) %>%
  st_union(by_feature = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid()
nyc_county_income <- st_drop_geometry(nyc_county_income)

# Get ACS income by ZCTA for NYC
nyc_zcta_income <- get_acs(
  survey = "acs5",
  geography = "zcta",
  zcta = get_zctas_by_county(borough_IDs$GEOID),
  variables = income_vars,
  year = year,
  geometry = TRUE, # get geometry (Water included)
  cb = FALSE # use core TIGER/Line shapefiles for shoreline mapping
) %>%
  clean_acs() %>%
  st_transform(4326) %>% 
  st_make_valid()

# Get ACS income for ZCTAs in counties adjacent to NYC
adjacent_counties_zctas <- get_acs(
  survey = "acs5",
  geography = "zcta",
  zcta = get_zctas_by_county(adjacent_GEOIDs$GEOID),
  variables = income_vars,
  year = year,
  geometry = TRUE, # get geometry (Water included)
  cb = FALSE # use core TIGER/Line shapefiles for shoreline mapping
) %>%
  clean_acs() %>%
  st_transform(4326) %>%
  st_make_valid()

# Get all ZCTAs within NYC and all those that are adjacent
zctas_wDist <- st_is_within_distance(county_dissolve, adjacent_counties_zctas, sparse = FALSE, dist = 50)
adjacent_zctas <- 
  adjacent_counties_zctas[apply(zctas_wDist, 2, any), ] %>%
  erase_water(year = year) %>%
  st_collection_extract(type = c("POLYGON", "POINT", "LINESTRING"))
nyc_zcta_income <- rbind(nyc_zcta_income, adjacent_zctas)
remove(adjacent_counties_zctas) # remove not needed data

# Get ACS incomes by Census tract
nyc_tract_income <- get_acs(
  survey = "acs5",
  geography = "tract",
  state = 36,
  county = borough_IDs$county_FIPS,
  variables = c(mhi = "DP03_0062", mfi = "DP03_0086"),
  year = year,
  geometry = TRUE, # get geometry (Water included)
  cb = FALSE # use core TIGER/Line shapefiles for shoreline mapping
) %>%
  filter(!(GEOID %in% c(36085990100, 36081990100))) %>%
  clean_acs() %>%
  st_transform(4326) %>%
  st_make_valid()

# Get ACS income for Cenus tracts in counties adjacent to NYC
adjacent_counties_tracts <- get_acs(
  survey = "acs5",
  geography = "tract",
  state = c(34, 36),
  variables = income_vars,
  year = year,
  geometry = TRUE, # get geometry (Water included)
  cb = FALSE, # use core TIGER/Line shapefiles for shoreline mapping
) %>% 
  filter(substr(GEOID, 1, 5) %in% adjacent_GEOIDs$GEOID) %>%
  clean_acs() %>%
  st_transform(4326) %>%
  st_make_valid()

# Get all tracts within NYC and all those that are adjacent
tracts_wDist <- st_is_within_distance(nyc_tract_income,
                                      adjacent_counties_tracts,
                                      sparse = FALSE,
                                      dist = 1)
adjacent_tracts <- 
  adjacent_counties_tracts[apply(tracts_wDist, 2, any), ] %>%
  erase_water(year = year) %>%
  st_collection_extract(type = c("POLYGON", "POINT", "LINESTRING"))
remove(county_dissolve, adjacent_counties_tracts, tracts_wDist)

# Remove geometry, NYC DCP geom will be used instead
nyc_tract_income <- st_drop_geometry(nyc_tract_income)

# Use NYC DCP ACS data tables to get income data for NYC CDTAs
nyc_cdta_income <-
  read.xlsx("https://s-media.nyc.gov/agencies/dcp/assets/files/excel/data-tools/census/acs/Econ_1822_CDTA.xlsx") %>%
  select(GeoType, CDTAType, GeogName, GeoID, Borough, MdHHIncE, MdFamIncE) %>%
  rename(mhi = MdHHIncE, 
         mfi = MdFamIncE,
         NAME = GeogName,
         GEOID = GeoID)

# ==============================================================================
# Get geometry data and join it to income data

borough_IDs <-
  data.frame(
    "GEOID" = c("36005", "36047", "36061", "36081", "36085"),
    "county_FIPS" = c(005, 047, 061, 081, 085),
    "borough" = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
    "boroCode" = c("2", "3", "1", "4", "5"),
    "abbreviation" = c("BX", "BK", "MN", "QN", "SI"))

# Get geometries from NYC DCP Bytes of the Big Apple data found at
# https://www.nyc.gov/site/planning/data-maps/open-data.page
# These geometries are quicker to access and nicer than those from tidycensus
boroughs <- read_geom("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson")
tracts <- read_geom("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Census_Tracts_for_2020_US_Census/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson")
cdtas <- 
  read_geom("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Community_Districts/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson") %>%
  mutate(BoroCD = as.character(BoroCD), boroCode = substr(BoroCD, 1, 1))

# Join borough geometries to county incomes using GEOIDs
boroughs <- left_join(boroughs, 
                      borough_IDs[, c("borough", "GEOID")],
                      join_by(BoroName == borough))
nyc_county_income <- geom_join(nyc_county_income, boroughs)

# Create the CDTA GEOID then use it to join CDTA geometries to CDTA incomes 
cdtas <- left_join(cdtas, borough_IDs[, c("boroCode", "abbreviation")],
                   relationship = "many-to-one", join_by(boroCode == boroCode))
cdtas$GEOID <- paste(cdtas$abbreviation, substr(cdtas$BoroCD, 2, 3), sep = "")
nyc_cdta_income <-
  geom_join(nyc_cdta_income, cdtas) %>%
  select("GEOID", "NAME", "mhi", "mfi", "geometry",
         -c("GeoType", "CDTAType", "Borough"))

# Join tract geometries to tract incomes and attach adjacent tracts
nyc_tract_income <- 
  geom_join(nyc_tract_income, tracts) %>%
  st_cast("MULTIPOLYGON") %>%
  rbind(adjacent_tracts)

# Clean names
nyc_zcta_income$NAME <- gsub("ZCTA5 ", "", nyc_zcta_income$NAME)
nyc_tract_income$NAME <- gsub(";", ",", nyc_tract_income$NAME)


# ==============================================================================
# Get NYC Housing and Vacancy Survey data to create custom adjustments
# Data from https://www.nyc.gov/site/hpd/about/research.page

# Read in NYCHVS data for all units (one record for each unit)
nychvs_units <- read.csv("https://www.nyc.gov/assets/hpd/data/allunits_puf_23.csv")

# Read in NYCHVS data for all occupied units (one record for each household)
# HHINC_REC1: household income; HHSIZE: household size
nychvs_occupied <- read.csv("https://www.nyc.gov/assets/hpd/data/occupied_puf_23.csv")

# Join occupied units to all units
nychvs_joined <-
  full_join(nychvs_units, nychvs_occupied, by = join_by(CONTROL == CONTROL)) %>%
  mutate(BORO = as.character(BORO))

# Calculate income adjustments like HUD does, except using household incomes.
# A household of size 4 is used as the base.
hhSize_adjustments <-
  aggregate(HHINC_REC1 ~ HHSIZE + BORO, nychvs_joined, median) %>%
  filter(HHSIZE <= 8) %>%
  group_by(BORO) %>%
  mutate(mltplr = HHINC_REC1 / HHINC_REC1[HHSIZE == 4]) %>%
  arrange(HHSIZE) %>%
  ungroup() %>%
  left_join(borough_IDs[, c("boroCode", "GEOID")], # join GEOIDs
            relationship = "many-to-one",
            join_by(BORO == boroCode))


# =============================================================================
# Save each income dataset as a CSV

write.csv(split_geometry_text(nyc_county_income),
          "data/tabular/processed/google_sheets/nyc_county_income.csv")
write.csv(split_geometry_text(nyc_zcta_income),
          "data/tabular/processed/google_sheets/nyc_zcta_income.csv")
write.csv(split_geometry_text(nyc_tract_income),
          "data/tabular/processed/google_sheets/nyc_tract_income.csv")
write.csv(split_geometry_text(nyc_cdta_income),
          "data/tabular/processed/google_sheets/nyc_cdta_income.csv")
write.csv(hhSize_adjustments,
          "data/tabular/processed/google_sheets/hhSize_adjustments.csv")