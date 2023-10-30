## packages 
library(quarto)
library(knitr)
library(sf)
library(tidyverse)
library(rgeos)
library(dplyr)
library(units)
library(lubridate)
library(leaflet)
library(piggyback)
library( geojsonio)
library(readxl)
library(googlesheets4)

# push to release
repo="BioSCape-io/BioSCape-flight-planning"
tag=paste0("v",format(lubridate::today(),"%Y%m%d"))


#if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}
#if (Sys.getenv("USER") == "adamw") {gmail = "adamw@buffalo.edu"}

# Authenticate and access the Google Sheet
#drive_auth(email = gmail)
#gs4_auth(token = drive_token())

## Download files

# QA spreadsheet
#qurl="https://docs.google.com/spreadsheets/d/1mKEFMiQ_J0mK3mOpt_t74PyFWwpga42e/edit?usp=sharing&ouid=100268570982256677112&rtpof=true&sd=true"
#drive_download(qurl,path="data/QA_lines.xlsx",overwrite=T)
qurl="https://drive.google.com/uc?export=download&id=1mKEFMiQ_J0mK3mOpt_t74PyFWwpga42e"

tryCatch(download.file(qurl,destfile="data/QA_Lines.xlsx"),
         error = function(e){e},
         warning = function(w){w})


# PI requests
#purl="https://docs.google.com/spreadsheets/d/1BnejyLTEeGbFO3TmF3nmddxclUnJiLBu/edit?usp=sharing&ouid=100268570982256677112&rtpof=true&sd=true"
#drive_download(purl,path="data/TeamRequirements.xlsx",overwrite=T)
purl="https://drive.google.com/uc?export=download&id=1BnejyLTEeGbFO3TmF3nmddxclUnJiLBu"

tryCatch(download.file(purl,destfile="data/TeamRequirements.xlsx"),
         error = function(e){e},
         warning = function(w){w})

#boxes <- st_read("data/20231026_combinedboxes.gpkg") 

# visions url
  vurl="https://popo.jpl.nasa.gov/mmgis-aviris/Missions/BIOSCAPE/Layers/"

# get boxes
  box_g5= st_read(paste0(vurl,"flightboxes/20231018_G5_LVIS_boxes_metadata.json")) %>% 
    mutate(aircraft="G5",
           box_nr=paste0("G5_",box_nr)) # add the G5 prefix
  
  box_g3 = st_read(paste0(vurl,"flightboxes/20231024_G3_AVNG_PRISM_boxes_am.geojson")) %>% 
    filter(box_nr!="G3_25_EW") %>% 
      mutate(aircraft="G3",
           box_nr=gsub("_AM","",box_nr), # drop the AM tags
           box_nr=gsub("_NS","",box_nr)) # drop the NS tags
  
  boxes=bind_rows(
    mutate(box_g5,aircraft,box_nr=as.character(box_nr),instrument,target,geometry),
    select(box_g3,aircraft,box_nr,instrument,target,geometry))%>%
    st_make_valid() %>%
    st_transform(9221)  



## clip ROIs to flight boxes, dissolve, and calculate areas (in m^2)
  
  instruments=tryCatch(readxl::read_xlsx("data/TeamRequirements.xlsx"),
                       error = function(e){e},
                       warning = function(w){w})
  
  
  #rois <- st_read(paste0(vurl,"flightboxes/20230907_Team_ROIs.json")) %>% st_transform(9221)
  
  rois <- st_read("data/20231026_Team_ROIs_addedPIs.gpkg") %>% 
    left_join(select(instruments,team_PI=PI,target))
  
# download lines from Visions
  g5lines="data/g5lines.json"
  download.file(paste0(vurl,"flightplans/Bioscape_101023_GV_lines.json"),destfile=g5lines)
  g3lines="data/g3lines.json"
  download.file(paste0(vurl,"flightplans/G3_plans_20231024_am.json"),destfile=g3lines)
  
  g5_lines =  st_read(g5lines) %>% 
    st_transform(9221) %>% 
    mutate(box=substr(Name,1,3))
  
  g3_lines = st_read(g3lines) %>% 
    st_transform(9221) %>% 
    st_transform(st_crs(rois)) 
  
# QA link
  system("ls data/")

  QA_lines_G3 = read_xlsx("data/QA_Lines.xlsx", sheet = "G3") %>% 
    #QA_lines_G3 = read_xlsx(qurl, sheet = "G3") %>% 
    left_join(g3_lines,by=c("Line"="Name")) %>% 
    mutate(aircraft="G3") %>% 
    select(box=Box,aircraft,target=Target, line=Line,prism=Status_PRISM,
           avirisng=Status_ANG,date=Date_Flown,geometry) %>% 
    pivot_longer(c(prism,avirisng),names_to="instrument",values_to="status")
  
  QA_lines_G5 = read_xlsx("data/QA_Lines.xlsx", sheet = "G5") %>% 
    mutate(aircraft="G5") %>% 
    left_join(g5_lines,by=c("Line"="Name")) %>% 
    select(box=Box_Number,aircraft,target=Target, line=Line,hytes=Status_HyTES,
           lvis=Status_LVIS,date=Date_Flown,geometry) %>% 
    pivot_longer(c(hytes,lvis),names_to="instrument",values_to="status")
  
  lines=bind_rows(QA_lines_G3,QA_lines_G5) %>% 
    filter(is.na(status)|status!=-1) %>%  # drop morning/afternoon if the other is flown
    st_as_sf() 
  
  
# box level summaries
  
  box_summary <- lines %>% 
    st_set_geometry(NULL) %>% 
    group_by(aircraft,box,instrument) %>% 
    summarize(lines=n(),
              lines_flown=sum(!is.na(status),na.rm=T),
              lines_complete=sum(status>0.5,na.rm=T),
              lines_summary=paste0(lines_complete,"/",lines)) 
  
  
  
  ## sum up area each PI requested (total)
  pi_roi_areas <- rois %>% 
    mutate(area = st_area(.)) %>% 
    group_by(team_PI, target) %>% 
    summarize(pi_total_area = sum(area)) %>% 
    mutate(pi_total_area=set_units(pi_total_area,"km^2"))
  
## Get swath-level PI information
  swaths <- lines %>% 
    mutate(
      geometry=case_when(
        instrument == "lvis" ~ st_buffer(lines, dist=1000)$geometry,
        instrument == "hytes" ~ st_buffer(., dist=6000)$geometry,
        instrument == "prism" ~ st_buffer(., dist=1000)$geometry,
        instrument == "avirisng" ~ st_buffer(., dist=1000)$geometry,
        TRUE ~ NA
      )
    ) #%>% 
  #  separate(line,c("aircraft","box","line","fl"),sep="_",extra="drop",remove = F) #%>% 
  #  select(-Orientation,-lat1,-lon1,lat2,-lon2,-az12, -az21,-'Flight _ltitude_(ASL)')
  
  
  st_agr(swaths) = "constant"
  st_agr(rois) = "constant"
  st_agr(boxes) = "constant"
  options(warn=0)
  
  swath_rois <- swaths %>% 
    st_intersection(st_transform(rois,st_crs(swaths))) %>% 
    filter(tolower(target)==tolower(target.1)) %>%  #drop mismatched terrestrial and aquatic
    mutate(pi_area = set_units(set_units(st_area(.),"km^2"),NULL)) # area of each swath per pi
  
  
