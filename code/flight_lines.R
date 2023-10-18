# Generate Lines
library(reticulate)

# path to hyplan
hyplan_path="~/repos/hyplan/"



virtualenv_create("hyplan")
virtualenv_install("hyplan", scan(file.path(hyplan_path,"requirements.txt"),what = "char"))
virtualenv_install("hyplan", "qgis")



## read in gpkg of flight boxes 
boxfile= "data/20230919_G3_AVIRIS_PRISM_boxes.gpkg" # UPDATE THIS FILENAME AS YOU UPDATE BOXES

## reproject flight boxes into crs of cloud layers
boxes1= st_read(boxfile) %>% #st_read(file.path(dir,boxfile)) %>% 
  st_transform(st_crs(cloud10))


call_python_function <- function() {
  # Load the Python script
  source_python(file.path(hyplan_path,"qgis/Polygon2Box.py"))
  
  # Call the Python function
  python_result <- my_python_function()
  
  # Return the result
  return(python_result)
}




processing.run("script:Polygon2Box", {
  'INPUT_LAYER':QgsProcessingFeatureSourceDefinition(
    '/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/GIS files/20231004_GV_lines_1mbuffer.gpkg|layername=20231004_GV_lines_1mbuffer', 
    selectedFeaturesOnly=True, featureLimit=-1, geometryCheck=QgsFeatureRequest.GeometryAbortOnInvalid),
    'BOX_NOTE':'',
  'AIRCRAFT':[1],
  'AIRCRAFT_ATTRIBUTE':'',
  'PRIMARY_INSTRUMENT':7,
  'PRIMARY_INSTRUMENT_ATTRIBUTE':'',
  'SECONDARY_INSTRUMENT':[3],
  'SECONDARY_INSTRUMENT_ATTRIBUTE':'',
  'PIXEL_SIZE':8,
  'PIXEL_SIZE_ATTRIBUTE':'',
  'ALT_OVERRIDE':None,
  'ALT_OVERRIDE_ATTRIBUTE':'',
  'GROUND_SPEED':180,'OVERLAP':5,
  'OVERLAP_ATTRIBUTE':'',
  'SOLAR_POSITION':False,
  'TARGET_ATTRIBUTE':'',
  'DATE_TIME':None,'OUTPUT_FILE':'TEMPORARY_OUTPUT',
  'DOCKER_CONTAINER_NAME':'hyplan2','MULTI_THREADING':True,
  'LOAD_LAYERS':True,'DOCKER_PATH':'/usr/local/bin/docker\n',
  'PYTHON_PATH':'/Applications/QGIS.app/Contents/MacOS/bin/python3'})



processing.run("script:Polygon2Box", {'INPUT_LAYER':QgsProcessingFeatureSourceDefinition('/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/GIS files/20230928_G5_LVIS_boxes.gpkg|layername=20230928_G5_LVIS_boxes', selectedFeaturesOnly=True, featureLimit=-1, geometryCheck=QgsFeatureRequest.GeometryAbortOnInvalid),'BOX_NOTE':'','AIRCRAFT':[1],'AIRCRAFT_ATTRIBUTE':'','PRIMARY_INSTRUMENT':7,'PRIMARY_INSTRUMENT_ATTRIBUTE':'','SECONDARY_INSTRUMENT':[3],'SECONDARY_INSTRUMENT_ATTRIBUTE':'','PIXEL_SIZE':8,'PIXEL_SIZE_ATTRIBUTE':'','ALT_OVERRIDE':None,'ALT_OVERRIDE_ATTRIBUTE':'','GROUND_SPEED':180,'OVERLAP':5,'OVERLAP_ATTRIBUTE':'','SOLAR_POSITION':False,'TARGET_ATTRIBUTE':'','DATE_TIME':None,'OUTPUT_FILE':'TEMPORARY_OUTPUT','DOCKER_CONTAINER_NAME':'hyplan2','MULTI_THREADING':True,'LOAD_LAYERS':True,'DOCKER_PATH':'/usr/local/bin/docker','PYTHON_PATH':'/Applications/QGIS.app/Contents/MacOS/bin/python3'})