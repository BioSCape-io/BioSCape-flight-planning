
## packages 
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

## Download files

# QA spreadsheet
qurl="https://docs.google.com/spreadsheets/d/1mKEFMiQ_J0mK3mOpt_t74PyFWwpga42e/edit?usp=sharing&ouid=100268570982256677112&rtpof=true&sd=true"
drive_download(qurl,path="data/QA_lines.xlsx",overwrite=T)

# PI requests
purl="https://docs.google.com/spreadsheets/d/1BnejyLTEeGbFO3TmF3nmddxclUnJiLBu/edit?usp=sharing&ouid=100268570982256677112&rtpof=true&sd=true"
drive_download(purl,path="data/TeamRequirements.xlsx",overwrite=T)

#boxes <- st_read("data/20231026_combinedboxes.gpkg") 



# visions url
vurl="https://popo.jpl.nasa.gov/mmgis-aviris/Missions/BIOSCAPE/Layers/"

# get boxes
box_g5= st_read(paste0(vurl,"flightboxes/20231018_G5_LVIS_boxes_metadata.json")) %>% 
  mutate(aircraft="G5")

box_g3 = st_read(paste0(vurl,"flightboxes/20231024_G3_AVNG_PRISM_boxes_am.geojson")) %>% 
  mutate(aircraft="G3")
  
boxes=bind_rows(
  mutate(box_g5,aircraft,box_nr=as.character(box_nr),instrument,target,geometry),
  select(box_g3,aircraft,box_nr,instrument,target,geometry))%>%
  st_make_valid() 

rois <- st_read(paste0(vurl,"flightboxes/20230907_Team_ROIs.json"))

rois <- st_read("data/20231026_Team_ROIs_addedPIs.gpkg")



## clip ROIs to flight boxes, dissolve, and calculate areas (in m^2)
instruments=readxl::read_xlsx("data/TeamRequirements.xlsx")

# download lines from Visions
g5lines="data/g5lines.json"
download.file(paste0(vurl,"flightplans/Bioscape_101023_GV_lines.json"),destfile=g5lines)
g3lines="data/g3lines.json"
download.file(paste0(vurl,"flightplans/G3_plans_20231024_am.json"),destfile=g3lines)

g5_lines =  st_read(g5lines) %>% 
  st_transform(st_crs(rois_target)) %>% 
  mutate(box=substr(Name,1,3))

g3_lines = st_read(g3lines) %>% 
  st_transform(st_crs(rois_target)) 

# QA link

QA_lines_G3 = read_xlsx("data/QA_Lines.xlsx", sheet = "G3") %>% 
  left_join(g3_lines,by=c("Line"="Name"))
QA_lines_G5 = read_xlsx("data/QA_Lines.xlsx", sheet = "G5") %>% 
  left_join(g5_lines,by=c("Line"="Name"))

lines=bind_rows(
  select(mutate(QA_lines_G3,aircraft="G3"),box=Box,aircraft,line=Line,prism=Status_PRISM,avirisng=Status_ANG,date=Date_Flown,geometry),
  select(mutate(QA_lines_G5,aircraft="G5"),box=Box_Number,aircraft, line=Line,hytes=Status_HyTES,lvis=Status_LVIS,date=Date_Flown,geometry)
  ) %>% 
  st_as_sf() %>% 
  gather("instrument","status",-date,-box,-aircraft,-line,-geometry)

# box level summaries

box_summary <- lines %>% 
  st_set_geometry(NULL) %>% 
  group_by(box,instrument) %>% 
  summarize(lines=paste0(sum(status>0.5,na.rm=T),"/",sum(!is.na(status)))) %>% 
  spread(key=instrument,val=lines)

lines %>% 
    ggplot()+geom_sf( mapping=aes(col=status,order=desc(status)))+
    facet_wrap(~instrument)

  
## sum up area each PI requested (total)
roi_areas <- rois %>% 
  mutate(area = st_area(.)) %>% 
  group_by(team_PI) %>% 
  summarize(pi_total_area = sum(area)) %>% 
  mutate(pi_total_area=set_units(pi_total_area,"km^2"))


## Get box-level PI information
rois_target=rois#left_join(rois,instruments,by=c("team_PI"="PI"))


swaths <- lines %>% 
  mutate(
    swathgeom=case_when(
      instrument == "lvis" ~ st_buffer(lines, dist=1000)$geometry,
      instrument == "hytes" ~ st_buffer(., dist=6500)$geometry,
      instrument == "prism" ~ st_buffer(., dist=1000)$geometry,
      instrument == "aviris" ~ st_buffer(., dist=1000)$geometry,
      TRUE ~ NA
      )
  ) %>% 
  separate(line,c("aircraft","box","line","fl"),sep="_",extra="drop",remove = F) #%>% 
#  select(-Orientation,-lat1,-lon1,lat2,-lon2,-az12, -az21,-'Flight _ltitude_(ASL)')


  swaths %>% 
  st_intersection(filter(st_transform(rois_target,st_crs(swaths)),target==c("aquatic"))) %>% 
  mutate(polygon_area = st_area(.)) %>% 
  group_by(box, team_PI,target,instrument) %>% 
  summarize(nlines=n(),
            status=sum(status>0,na.rm=T),
            proportion=100*status/nlines) %>% 
    st_set_geometry(NULL) %>% 
    View()

  
  filter(swaths,instrument=="avirisng") %>% plot()#st_area() %>% sum() %>% set_units("km^2")
  ggplot(select(t_pi_areas,team_PI))+geom_sf(aes(fill=team_PI))

t_pi_areas <-
  filter(swaths,status>0) %>% 
  st_intersection(filter(rois_target,target=="terrestrial")) %>% 
  filter(target%in%c("terrestrial")) %>% 
  mutate(area=set_units(st_area(.),"km^2")) %>% 
  group_by(team_PI) %>% 
  summarize(pi_area_aquired=sum(area,na.rm=T)) %>% 
  left_join(st_set_geometry(roi_areas,NULL)) %>% 
  mutate(pi_total_area=set_units(pi_total_area,NULL),
         pi_area_aquired=set_units(pi_area_aquired,NULL),
         pi_proportion=100*pi_area_aquired/pi_total_area)

a_pi_areas <-
  filter(line_swaths,Status_ANG>0) %>% 
  st_intersection(filter(rois_target,target=="aquatic")) %>% 
  filter(target%in%c("aquatic")) %>% 
  mutate(area=st_area(.)) %>% 
  group_by(team_PI) %>% 
  summarize(total_area=set_units(sum(area,na.rm=T),"km^2"))

  
   


    line_swaths %>% 
      ggplot(aes(fill=Status_ANG>0,col=Status_ANG>0)) + 
      geom_sf()
    
    
## add column for area already acquired (in m^2)

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


areas_pi_flightbox <-
  rois_target %>% 
  group_by(target) %>% 
  st_intersection(boxes) %>% 
  mutate(polygon_area = st_area(.)) %>% 
  group_by(box_nr, team_PI, priority,target) %>% 
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
  mutate(
    # !!paste0(format(lubridate::ymd(today+1), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+1), "%b%d"),"_G5"):="",
    # !!paste0(format(lubridate::ymd(today+2), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+2), "%b%d"),"_G5"):="",
    # !!paste0(format(lubridate::ymd(today+3), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+3), "%b%d"),"_G5"):="",
    # !!paste0(format(lubridate::ymd(today+4), "%b%d"),"_G3"):="",
    # !!paste0(format(lubridate::ymd(today+4), "%b%d"),"_G5"):="",
  ) %>% 
  select(starts_with("Oct"),starts_with("Nov"),box_nr,target,
         priority=priority_area_cloud,priority_cloud=cloudmean.x,priority_team=area_based_box_priority,Sync,
         AVIRIS,PRISM,HyTES,LVIS,PIs) %>% 
  write_sheet(ss = psheet,
              sheet = as.character(today))

