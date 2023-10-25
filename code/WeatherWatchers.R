library(googlesheets4)
library(googledrive)
library(sf)
library(tidyverse)
library(mapview)
library(stringr)

if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}
if (Sys.getenv("USER") == "adamw") {gmail = "adamw@buffalo.edu"}

# Authenticate and access the Google Sheet
drive_auth(email = gmail)
gs4_auth(token = drive_token())

# Get data
folks <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1blTwiT-67q8vdElK8wdubm_edto2vx7smxwlAmmkdOs/edit?resourcekey#gid=1332651624")
names(folks) <- c("DateTime", "Name", "Number", "Email", "LatLong", "Box", "Comment")
boxes <- st_read("data/20231024_G3_AVNG_PRISM_boxes.gpkg")

# Fix lat lon
folks$Lat <- as.numeric(str_split_i(folks$LatLong, ", ", i = 1))
folks$Lon <- as.numeric(str_split_i(folks$LatLong, ", ", i = 2))

# Make sf object
folks <- st_as_sf(folks, coords = c("Lon", "Lat"), crs = 4326)
boxes <- boxes %>% st_transform(st_crs(folks))

# Map
folks %>% select(Name, Number) %>% mapview(label = "Number", color = "magenta", col.regions = "white") +
  mapview(boxes, label = "box_nr")
