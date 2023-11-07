# this script generates the static box-level summaries
# including cloud data, PIs associated with each box
# instruments associated with each box
# and writes it out within the repo.  This is designed to be run
# before the box_priorities script.

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


#### PART 1: Pulling out elevation and cloud values for the flight boxes ####
# note: we are still missing wind...

## download cloud and elevation data
# this is slow so I've commented it out, you only need to run it once! 

options(timeout=1000)

if (!file.exists("data/webdata_downloads/cloud_10.tif")) {
  download.file("http://data.earthenv.org/cloud/MODCF_monthlymean_10.tif",
                destfile="data/webdata_downloads/cloud_10.tif")} #october 
if (!file.exists("data/webdata_downloads/cloud_11.tif")) {
  download.file("http://data.earthenv.org/cloud/MODCF_monthlymean_11.tif",
                destfile="data/webdata_downloads/cloud_11.tif")} #november 
if (!file.exists("data/webdata_downloads/earthenv_maxelev_1km.tif")) {
  download.file("https://data.earthenv.org/topography/elevation_1KMma_SRTM.tif",
                destfile="data/webdata_downloads/earthenv_maxelev_1km.tif")} #1km elevation from SRTM

cloud10=rast("data/webdata_downloads/cloud_10.tif")
cloud11=rast("data/webdata_downloads/cloud_11.tif")
elev=rast("data/webdata_downloads/earthenv_maxelev_1km.tif")

## read in gpkg of flight boxes 
outboxfile= "data/20231026_combinedboxes.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES

g3_boxfile= "data/20231024_G3_AVNG_PRISM_boxes.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES

g5_boxfile= "data/20231023_G5_LVIS_boxes.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES
g5_outboxfile= "data/20231023_G5_LVIS_CLOUD_ELEV.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES


## reproject flight boxes into crs of cloud layers
g3_boxes= st_read(g3_boxfile) %>% #st_read(file.path(dir,boxfile)) %>% 
  st_transform(st_crs(cloud10))

g5_boxes= st_read(g5_boxfile) %>% #st_read(file.path(dir,boxfile)) %>% 
  st_transform(st_crs(cloud10))

boxes1=bind_rows(
  select(g3_boxes,box=box_nr,target,instrument,geom) %>% 
    mutate(aircraft="G3",target=trimws(target)) %>%  #drop white spaces in names
      filter(!grepl("PM",box)) %>% 
      filter(!grepl("NS",box)) %>% 
      filter(!grepl("EW",box)) %>% 
      mutate(box_nr=gsub("_AM","",box)),
  select(g5_boxes,box=box_nr,target,instrument,geom) %>% 
    mutate(aircraft="G5"))

## extract cloud and elevation stats
boxes1$cloud10=terra::extract(cloud10,boxes1,fun=mean)[,2]/100
boxes1$cloud11=terra::extract(cloud11,boxes1,fun=mean)[,2]/100
boxes1$maxmaxelev=terra::extract(elev,boxes1,fun=max)[,2]
boxes1$meanmaxelev=terra::extract(elev,boxes1,fun=mean)[,2]

## pull out mean cloud values for each box 
# this is average cloud in october and november for each box
# high average cloud means high risk for that box (less likely to be clear on any given day)
boxes <- 
  boxes1%>%
  mutate(cloudmean=(cloud10+cloud11)/2)%>% #mean oct/nov cloud
  st_transform(9221)  #transform back to original CRS (epsg9221)
  
## update ROIs 
rois <- st_read("data/20231019_Team_ROIs.gpkg")
outroifile= "data/20231026_Team_ROIs_addedPIs.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES
outroifilegeojson= "data/20231026_Team_ROIs_addedPIs.geojson" # UPDATE THIS FILENAME AS YOU UPDATE BOXES


## Fix missing PI's issues
rois$team_PI <- str_replace(rois$team_PI, "Slingsby", "Townsend")

hmm <- rois %>% filter(team_PI == "Townsend") %>% 
  mutate(team_PI1 = "Fitzpatrick", team_PI2 = "Merow") %>%
  pivot_longer(cols = starts_with("team_PI"), values_to = "team_PI") %>%
  select(!name)

rois <- rbind(rois, hmm)

## fix and clean up geometries
rois <- st_make_valid(rois)

## export rois with added PIs
st_write(rois,dsn = outroifile,append=F) 
## export rois with added PIs for Visions
st_write(rois,dsn = outroifilegeojson,append=F) 


## Add PI information
library(readxl)

rois <- st_read(outroifile) %>% st_make_valid()
instruments=readxl::read_xlsx("data/20231021_TeamRequirements.xlsx")

## Get box-level PI information
rois_target=left_join(rois,instruments,by=c("team_PI"="PI"))

if(F){
unique(rois$team_PI)
unique(rois_target$team_PI)
unique(rois_target$team_PI)

pi="Van Aardt"
filter(rois_target,team_PI==pi)
filter(rois,team_PI==pi)
filter(instruments,PI==pi)
}
#rois$team_PI%in%rois$team_PI

aquatic_box_report <- 
  boxes %>% 
  filter(target%in%c("aquatic")) %>% 
  mutate(box_nr=gsub("_AM","",box_nr)) %>% 
  st_intersection(filter(rois_target,target==c("aquatic"))) %>% 
  mutate(polygon_area = st_area(.)) %>% 
  group_by(box_nr, team_PI,target) %>% 
  summarize(AVIRIS=sum(AVIRIS)>0,PRISM=sum(PRISM)>0,LVIS=sum(LVIS)>0,HyTES=sum(HyTES)>0,Synchronous=sum(Synchronous)>0)


terrestrial_box_report <- 
  boxes %>% 
  filter(target=="terrestrial") %>% 
  st_intersection(filter(rois_target,target%in%c("terrestrial aquatic ","terrestrial"))) %>% 
  mutate(polygon_area = st_area(.)) %>% 
  group_by(box_nr, team_PI,target) %>% 
  summarize(AVIRIS=sum(AVIRIS)>0,PRISM=sum(PRISM)>0,LVIS=sum(LVIS)>0,HyTES=sum(HyTES)>0,Synchronous=sum(Synchronous)>0)

boxes_PIs <- bind_rows(aquatic_box_report,terrestrial_box_report) %>% 
  group_by(box_nr,target) %>% 
  summarize(AVIRIS=sum(AVIRIS,na.rm=T),
            PRISM=sum(PRISM,na.rm=T),
            HyTES=sum(HyTES,na.rm=T),
            LVIS=sum(LVIS,na.rm=T),
            Sync=sum(Synchronous)>0,
            PIs=paste(team_PI,collapse=", "))%>% 
  arrange(box_nr) %>% 
  st_set_geometry(NULL) #%>% View()

## export boxes with added cloud info 
boxes %>% 
  left_join(ungroup(boxes_PIs),by=c("box_nr","target")) %>% 
  st_write(dsn = outboxfile,append=F) 







#### Update geopackages for Windy


if(F){  # only update when needed!
G3B=st_read("data/20231024_G3_AVNG_PRISM_boxes.gpkg") %>% 
  filter(!grepl("PM",box_nr)) %>% 
  filter(!grepl("NS",box_nr)) %>% 
  filter(!grepl("EW",box_nr)) %>% 
  mutate(box_nr=gsub("_AM","",box_nr)) %>% 
  left_join(boxes_PIs) %>% 
  mutate(aircraft="G3",
          name=box_nr,
         description=paste0(target,": (",PIs,")")) 

G5B=st_read("data/20231023_G5_LVIS_boxes.gpkg") %>% 
  mutate(aircraft="G5",
         name=paste0("G5_",box_nr))#,
#         description=paste0(target,": (",mean_resolution,")"))

bind_rows(G3B,G5B) %>% 
  select(box_nr,aircraft, instrument,target,mean_resolution,name,description) %>% 
  st_write("data/20231026_boxes_windy.gpkg",append=F)

}



