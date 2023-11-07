
## packages 
library(terra)
library(sf)
library(tidyverse)
library(rgeos)
library(dplyr)
library(units)
library(googlesheets4)
library(googledrive)

if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}
if (Sys.getenv("USER") == "adamw") {gmail = "adamw@buffalo.edu"}

# Authenticate and access the Google Sheet
drive_auth(email = gmail)
gs4_auth(token = drive_token())


#### PART 2: Box Prioritization ####

## read in flight boxes and team region of interest (ROI) polygons 
#boxes_dir= "data/20230919_G3_AVIRIS_PRISM_boxes_CLOUD_ELEV.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE ROIS
#rois_dir="data/20230907_Team ROIs.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE ROIS

boxes <- st_read("data/20231021_G3_AVIRIS_PRISM_boxes_CLOUD_ELEV.gpkg") %>%  st_make_valid()
rois <- st_read("data/20231019_Team_ROIs_addedPIs.gpkg")



#GVlines<-st_read("https://popo.jpl.nasa.gov/mmgis-aviris/Missions/BIOSCAPE/Layers/flightplans/Bioscape_101023_GV_lines.json")
#GIIIlines<-st_read("https://popo.jpl.nasa.gov/mmgis-aviris/Missions/BIOSCAPE/Layers/flightplans/G3_lines_20231021d.json")
#plot(GIIIlines)


## sum up area each PI requested (total)
roi_areas <- rois %>% 
  mutate(area = st_area(.)) %>% 
  group_by(team_PI) %>% 
  summarize(pi_total_area = sum(area))

## add column for area already acquired (in m^2)
### NEED TO READ IN OR CALCULATE pi_area_acquired below "###" - currently hard-wired to "0"

# ## update area acquired 
pi_area_acquired <- c("0", #Adler   
                      "0", #CalVal
                      "0", #Cawse-Nicholson
                      "0", #Cho
                      "0", #Clark
                      "0", #Fitzpatrick
                      "0", #Guild
                      "0", #Merow
                      "0", #Rossi
                      "0", #Stovall
                      "0", #Townsend
                      "0", #van Aardt
                      "0", #Wu
                      "0" #van Niekerk
                      )

###
area_acquired <- data.frame(team_PI = roi_areas$team_PI, pi_area_acquired = pi_area_acquired)
#area_acquired <- data.frame(team_PI = roi_areas$team_PI, pi_area_acquired = 0)
###
area_acquired$pi_area_acquired = as.numeric(area_acquired$pi_area_acquired)
area_acquired$pi_area_acquired = set_units(area_acquired$pi_area_acquired, m^2)
area_acquired$pi_area_acquired = as_units(area_acquired$pi_area_acquired)

## clip ROIs to flight boxes, dissolve, and calculate areas (in m^2)
areas_pi_flightbox <- 
  st_intersection(rois, boxes) %>% 
  mutate(polygon_area = st_area(.)) %>% 
  group_by(box_nr, team_PI, priority) %>% 
  summarize(polygon_area = sum(polygon_area) ) %>% 
  left_join(st_set_geometry(roi_areas,NULL),by="team_PI") %>% 
  left_join(area_acquired, by="team_PI") %>%
  mutate(area_remaining=(pi_total_area-pi_area_acquired)/pi_total_area)

## calculate area-based priority index by box ### ADD PRIORITY HERE?
box_priority_area <- areas_pi_flightbox %>% 
  mutate(pi_area_in_box = polygon_area/pi_total_area) %>% 
  mutate(pi_area_priority = pi_area_in_box * area_remaining)  %>% #View() #you are penalised if you already have an acquisition
  group_by(box_nr) %>% 
  summarize(area_based_box_priority = sum(pi_area_priority)) 
  
## pull in a cloud risk value for each box 
box_priority_cloud <- st_set_geometry(boxes,NULL) #remove geometry from boxes, coerce to dataframe
box_priority_cloud <- box_priority_cloud [, c("box_nr", "cloudmean")] # pull out box_nr and cloud mean

## add in cloud values and calculate new combined priority metric
box_priority_area_cloud <- box_priority_area %>% 
  left_join(box_priority_cloud, by="box_nr") %>%
  mutate(cloudscale=cloudmean) %>% 
  mutate(across(cloudscale, function(x){scales::rescale(cloudmean,c(20,100))})) %>% #change to adjust strength of cloud
  mutate (priority_area_cloud = (cloudscale) * area_based_box_priority) # make cloud percentage into a proportion, then multiply by area based priority score 

#print(box_priority_area_cloud, n=100) 

## Sort and export
psheet="https://docs.google.com/spreadsheets/d/1D4Xba_yucp1o9eHkRmvrHdxQgRG4HbHVOu9U8ajDIY8/edit#gid=0" #separate sheet
#psheet="https://docs.google.com/spreadsheets/d/1x_mmDL6JhNivV-Mk5HOFGxodkphaYjVicXkB4k8j9tE" #combined data sheet


today=lubridate::today() #set sheet name

box_priority_area_cloud %>% 
  arrange(desc(area_based_box_priority)) %>%
  st_set_geometry(NULL) %>%
  select(-cloudscale) %>% 
  mutate(across(area_based_box_priority, function(x){round(as.numeric(x/sum(area_based_box_priority)*100))})) %>%
  mutate(across(cloudmean, round)) %>%
  mutate(across(priority_area_cloud, function(x){round(as.numeric(x/max(priority_area_cloud)*100))})) %>%
  arrange(desc(priority_area_cloud))%>%
  left_join(st_set_geometry(boxes,NULL),c("box_nr")) %>% 
  # mutate(
    # !!paste0(format(lubridate::ymd(today+1), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+1), "%b%d"),"_G5"):="",
    # !!paste0(format(lubridate::ymd(today+2), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+2), "%b%d"),"_G5"):="",
    # !!paste0(format(lubridate::ymd(today+3), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+3), "%b%d"),"_G5"):="",
    # !!paste0(format(lubridate::ymd(today+4), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+4), "%b%d"),"_G5"):="",
  # ) %>% 
  select(starts_with("Oct"),starts_with("Nov"),box_nr,target,
         priority=priority_area_cloud,priority_cloud=cloudmean.x,priority_team=area_based_box_priority,Sync,
         AVIRIS,PRISM,HyTES,LVIS,PIs) %>% 
  write_sheet(ss = psheet,
              sheet = as.character(today))

