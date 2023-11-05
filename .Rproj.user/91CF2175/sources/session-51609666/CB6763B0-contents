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
download.file(qurl,destfile="data/QA_lines.xlsx")

# PI requests
#purl="https://docs.google.com/spreadsheets/d/1BnejyLTEeGbFO3TmF3nmddxclUnJiLBu/edit?usp=sharing&ouid=100268570982256677112&rtpof=true&sd=true"
#drive_download(purl,path="data/TeamRequirements.xlsx",overwrite=T)
purl="https://drive.google.com/uc?export=download&id=1BnejyLTEeGbFO3TmF3nmddxclUnJiLBu"
download.file(purl,destfile="data/TeamRequirements.xlsx")

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
instruments=readxl::read_xlsx("data/TeamRequirements.xlsx")

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
message(print(paste("data/QA_Lines.xlsx:", file.exists ("data/QA_Lines.xlsx"))))
message(paste(list.files("data/"),collapse="\n"))

QA_lines_G3 = read_xlsx("data/QA_Lines.xlsx", sheet = "G3") %>% 
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


# Get total area requested by each PI
pi_total_area<-swath_rois %>% 
  group_by(team_PI) %>% 
  summarize(pi_total_area=sum(pi_area))


# Intersect swaths and ROIs
pi_swaths <- swath_rois %>% 
  mutate(status=ifelse(is.na(status),0,status), # separate out if it's good
         pi_area_acquired=ifelse(status>0,pi_area,0)) %>%  # area of flown lines
  group_by(box, team_PI,target,instrument) %>% 
  left_join(st_set_geometry(pi_total_area,NULL)) %>% 
  mutate(
    pi_proportion_totalarea_in_swath = 100 * pi_area/pi_total_area,
    pi_proportion_totalarea_in_swath_acquired = 100 * pi_area_acquired/pi_total_area,
    pi_area_remaining = pi_area-pi_area_acquired,
    pi_proportion_area_remaining=100*pi_area_remaining/pi_total_area
  )

# summary of lines flown and acquired
line_summary <-
  lines %>% 
  group_by(aircraft,box,target,instrument) %>% 
  summarize(
    nlines_total=n(),
    nlines_flown = sum(status>=0,na.rm=T),
    nlines_acquired = sum(status>0.5,na.rm=T),
    line_proportion = ifelse(nlines_acquired==0,0,100*nlines_acquired/nlines_flown)
  ) %>% 
      filter(aircraft=="G3"&target=="Terrestrial"&instrument=="avirisng"| #filter priorities
         aircraft=="G3"&target=="Aquatic"&instrument=="prism"|
         aircraft=="G5"&target=="Terrestrial"&instrument=="lvis"|
         aircraft=="G5"&target=="Aquatic"&instrument=="hytes"  
           )


swath_summary <-
  pi_swaths %>% 
  group_by(aircraft, box, target, line, instrument) %>% 
  summarize(
    nPI=length(unique(team_PI)),
    PIs=paste(unique(team_PI),collapse=","),
    roi_total_area=sum(pi_area,na.rm=T),
    status=sum(status>0.5,na.rm=T),
    priority_updated = 
      sum(pi_proportion_totalarea_in_swath * pi_proportion_area_remaining,na.rm=T),
    priority_original = 
      sum(pi_proportion_totalarea_in_swath * pi_proportion_totalarea_in_swath,na.rm=T)) %>% 
    st_set_geometry(NULL) %>% 
    filter(aircraft=="G3"&target=="Terrestrial"&instrument=="avirisng"| #filter priorities
         aircraft=="G3"&target=="Aquatic"&instrument=="prism"|
         aircraft=="G5"&target=="Terrestrial"&instrument=="lvis"|
         aircraft=="G5"&target=="Aquatic"&instrument=="hytes"  
           ) %>% 
  left_join(select(lines,line,geometry),by="line")




box_summary <- 
  swath_summary %>% 
   group_by(aircraft, box, target, instrument) %>% 
    summarize(
              #pi_area_total = sum(pi_total_area,na.rm=T),
              # pi_area_acquired = sum(pi_area_acquired,na.rm=T),
              # pi_proportion_totalarea_in_swath = sum(pi_proportion_totalarea_in_swath,na.rm=T),
              # pi_proportion_totalarea_in_swath_acquired = 
              #   sum(pi_proportion_totalarea_in_swath_acquired,na.rm=T),
      priority_updated = mean(priority_updated,na.rm=T),
              priority_original = mean(priority_original,na.rm=T),
      nPIs=length(unique(unlist(strsplit(PIs,",")))),
      PIs=paste(unique(unlist(strsplit(PIs,","))),collapse=",")
              ) %>% 
  mutate(box_nr=paste0(aircraft,"_",box)) %>% 
  left_join(st_set_geometry(line_summary, NULL),
            by=c("aircraft","box","target","instrument")) %>% 
  filter(aircraft=="G3"&target=="Terrestrial"&instrument=="avirisng"| #filter priorities
         aircraft=="G3"&target=="Aquatic"&instrument=="prism"|
         aircraft=="G5"&target=="Terrestrial"&instrument=="lvis"|
         aircraft=="G5"&target=="Aquatic"&instrument=="hytes"  
           ) %>% 
  left_join(select(boxes,box_nr,geometry),by="box_nr") %>% 
  group_by(aircraft) %>% 
  mutate(priority_updated_rank=rank(desc(priority_updated)),
         priority_original_rank=rank(desc(priority_original)))


## Write the geojson and push to release

linefile=file.path("data",paste0("bioscape_line_summary_",tag,".geojson"))
boxfile=file.path("data",paste0("bioscape_box_summary_",tag,".geojson"))



if(file.exists(linefile)) file.remove(linefile)
swath_summary  %>% 
#  geojson_style(var="priority_updated",
#                fill = 
#                  colorQuantile(domain = swath_summary$priority_updated,
#                palette=viridis::viridis_pal(direction = -1))) %>% 
  st_write(linefile,append=F)

if(file.exists(boxfile)) file.remove(boxfile)
box_summary %>% 
  st_write(boxfile, overwrite=T)


# if release doesn't exist for this tag - create it
if(!any(tag%in%pb_releases(repo)$tag_name))  pb_new_release(repo = repo,tag=tag)

pb_upload(file = linefile,repo=repo,tag=tag)
pb_upload(file = boxfile,repo=repo,tag=tag)



# Upload to google drive
today=lubridate::today() #set sheet name
psheet="https://docs.google.com/spreadsheets/d/1D4Xba_yucp1o9eHkRmvrHdxQgRG4HbHVOu9U8ajDIY8/edit#gid=0" #separate sheet


box_summary %>% 
  group_by(aircraft) %>% 
  mutate(priority_updated=rank(desc(priority_updated)),
         priority_original=rank(desc(priority_original))) %>% 
  select(aircraft, box, target, priority_updated, priority_original) %>% 
  arrange(aircraft, priority_updated) %>% 
  write_sheet(ss = psheet,
              sheet = as.character(today))

print("Saving Objects")

save(boxes, lines, rois, swaths, 
     swath_summary, line_summary, box_summary,
     file = "data/report_data.Rdata")

print("Workflow Complete!")


