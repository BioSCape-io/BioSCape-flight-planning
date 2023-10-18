OLD


## packages 
library(terra)
library(sf)
library(tidyverse)
library(rgeos)
library(dplyr)
library(units)

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
                

# download.file("http://data.earthenv.org/cloud/MODCF_monthlymean_11.tif",
#            destfile="data/webdata_downloads/cloud_11.tif") #november
# download.file("https://data.earthenv.org/topography/elevation_1KMma_SRTM.tif",
#             destfile="data/webdata_downloads/earthenv_maxelev_1km.tif") #1km elevation from SRTM

cloud10=rast("data/webdata_downloads/cloud_10.tif")
cloud11=rast("data/webdata_downloads/cloud_11.tif")
elev=rast("data/webdata_downloads/earthenv_maxelev_1km.tif")

## read in gpkg of flight boxes 
boxfile= "data/20230919_G3_AVIRIS_PRISM_boxes.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES
outboxfile= "data/20230919_G3_AVIRIS_PRISM_boxes_CLOUD_ELEV.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES

## reproject flight boxes into crs of cloud layers
boxes1= st_read(boxfile) %>% #st_read(file.path(dir,boxfile)) %>% 
  st_transform(st_crs(cloud10))

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
  st_transform(9221) #transform back to original CRS (epsg9221)

## export boxes with added cloud info 
st_write(boxes,dsn = outboxfile,append=F) 


#### PART 2: Working out how to prioritise boxes ####

library(googlesheets4)

instruments=readxl::read_xlsx("../../../Google Drive/Shared drives/BioSCape_Admin/GIS files/20231017_TeamRequirements.xlsx")


## read in flight boxes and team region of interest (ROI) polygons 
#boxes_dir= "data/20230919_G3_AVIRIS_PRISM_boxes_CLOUD_ELEV.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE ROIS
#rois_dir="data/20230907_Team ROIs.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE ROIS

boxes <- st_read("data/20230919_G3_AVIRIS_PRISM_boxes_CLOUD_ELEV.gpkg")
rois <- st_read("data/20230907_Team ROIs.gpkg")

## fix and clean up geometries
boxes <- st_make_valid(boxes)
rois <- st_make_valid(rois)

## sum up area each PI requested (total)
roi_areas <- rois %>% 
  mutate(area = st_area(.)) %>% 
  group_by(team_PI) %>% 
  summarize(pi_total_area = sum(area) )

## add column for area already acquired (in m^2)
team_PI <- c("Adler", "Cawse-Nicholson", "Cho", "Clark", "Guild", 
             "Rossi", "Slingsby", "Stovall", "Van Aardt", "Wu", "van Niekerk")

## update area acquired - THIS IS VERY IMPORTANT
pi_area_acquired <- c("0", #Adler
                      "0", #Cawse-Nicholson
                      "0", #Cho
                      "0", #Clark
                      "0", #Guild
                      "0", #Rossi
                      "0", #Slingsby
                      "0", #Stovall
                      "0", #van Aardt
                      "0", #Wu
                      "0" #van Niekerk
                      )

area_acquired <- data.frame(team_PI, pi_area_acquired)
area_acquired$pi_area_acquired = as.numeric(area_acquired$pi_area_acquired)
area_acquired$pi_area_acquired = set_units(area_acquired$pi_area_acquired, m^2)
area_acquired$pi_area_acquired = as_units(area_acquired$pi_area_acquired)

## clip ROIs to flight boxes, dissolve, and calculate areas (in m^2)
areas_pi_flightbox <- 
  st_intersection(rois, boxes) %>% 
  left_join(instruments,by=c("team_PI"="PI")) %>% 
  mutate(polygon_area = st_area(.)) %>% 
  group_by(box_nr, team_PI, priority) %>% 
  summarize(polygon_area = sum(polygon_area) ) %>% 
  left_join(st_set_geometry(roi_areas,NULL),by="team_PI") %>% 
  left_join(area_acquired, by="team_PI") %>%
  mutate(area_remaining=(pi_total_area-pi_area_acquired)/pi_total_area)

## calculate area-based priority index by box

box_priority_area <- areas_pi_flightbox %>% 
  mutate(pi_area_in_box = polygon_area/pi_total_area) %>% 
  mutate (pi_area_priority = pi_area_in_box * area_remaining)  %>% #you are penalised if you already have an acquisition
  group_by (box_nr) %>% 
  summarize(area_based_box_priority = sum(pi_area_priority) ) 
  
## pull in a cloud risk value for each box 

box_priority_cloud <- st_set_geometry(boxes,NULL) #remove geometry from boxes, coerce to dataframe
box_priority_cloud <- box_priority_cloud [, c("box_nr", "cloudmean")] # pull out box_nr and cloud mean

## add in cloud values and calculate new combined priority metric
box_priority_area_cloud <- box_priority_area %>% 
  left_join(box_priority_cloud, by="box_nr") %>%
  mutate (priority_area_cloud = (cloudmean/100) * area_based_box_priority) # make cloud percentage into a proportion, then multiply by area based priority score 

box_priority_area_cloud %>% 
  arrange(desc(area_based_box_priority)) %>% 
  print(n=100) 


### Get box - level instruments

## Get box-level PI information
rois_target=left_join(rois,instruments,by=c("team_PI"="PI"))
unique(rois$team_PI)
unique(rois_target$team_PI)
unique(rois_target$team_PI)

pi="Van Aardt"
filter(rois_target,team_PI==pi)
filter(rois,team_PI==pi)
filter(instruments,PI==pi)

#rois$team_PI%in%rois$team_PI

  aquatic_box_report <- 
    boxes %>% 
    filter(target=="aquatic") %>% 
    st_intersection(filter(rois_target,target=="aquatic")) %>% 
    mutate(polygon_area = st_area(.)) %>% 
    group_by(box_nr, team_PI,target) %>% 
    summarize(AVIRIS=sum(AVIRIS)>0,PRISM=sum(PRISM)>0,LVIS=sum(LVIS)>0,HyTES=sum(HyTES)>0)
    
  
    terrestrial_box_report <- 
      boxes %>% 
      filter(target=="terrestrial") %>% 
      st_intersection(filter(rois_target,target=="terrestrial")) %>% 
      mutate(polygon_area = st_area(.)) %>% 
      group_by(box_nr, team_PI,target) %>% 
      summarize(AVIRIS=sum(AVIRIS)>0,PRISM=sum(PRISM)>0,LVIS=sum(LVIS)>0,HyTES=sum(HyTES)>0)
    
      
      bind_rows(aquatic_box_report,terrestrial_box_report) %>% 
      rowwise() %>% 
      mutate(G3=(AVIRIS+PRISM)>0,
           G5=(LVIS+HyTES)>0) %>%
        ungroup() %>% 
      group_by(box_nr,target) %>% 
    summarize(AVIRIS=sum(AVIRIS,na.rm=T),
            PRISM=sum(AVIRIS,na.rm=T),
            HyTES=sum(HyTES,na.rm=T),
            LVIS=sum(LVIS,na.rm=T),
            G3=sum(G3,na.rm=T),
            G5=sum(G5,na.rm=T),
            PIs=paste(team_PI,collapse=", "))%>% 
        arrange(box_nr) %>% 
  st_set_geometry(NULL) %>% View()


### Merge with original boxes for mapping
boxes2=left_join(boxes, st_set_geometry(box_priority_area_cloud,NULL),by="box_nr") %>% 
  mutate(area_based_box_priority=as.numeric(area_based_box_priority)) 

library(leaflet)
  leaflet() %>%
  addTiles() %>%  # Add default basemap
  addPolygons(data = st_transform(boxes,crs = st_crs(4326)), 
              fillColor = ~colorNumeric(palette = "YlOrRd", domain = boxes2$area_based_box_priority)(boxes2$area_based_box_priority),
              fillOpacity = 0.5) %>% 
    addLegend(
      position = "bottomright",
      pal = colorNumeric(
        palette = "YlOrRd",
        domain = boxes2$area_based_box_priority
      ),
      values = boxes2$area_based_box_priority,
      title = "Priority"
    )
