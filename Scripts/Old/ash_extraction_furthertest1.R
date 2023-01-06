
### Extracting from HDR image using a shapefile of polygons, outputs each polygon as a separate tif file
### Below, extracting buffer samples

library(maptools)  ## For wrld_simpl
library(raster)
library(sf)
library(caTools)
library(rgdal)
library(sp)
library(rgeos)

setwd("E:/") 


## Call & read in polygon shapefile 
poly <- st_read("Thesis Data/Ash Trees/Imagesamples/Atkinson/atkinson_3983_samples.shp") 

## Call & read in image to extract from
raster <- brick("Chan_Thesis_Missions/Ash_07262019/100131_ash_atkinson1_2019_07_27_13_58_10/raw_3983_rd_rf_or")
#atkin_16548_sub_ENVI <- read.ENVI("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//TestData//atkin_16548_sub") # using this as the image only gives 1 band

## Plot the two read-ins, if desired
#plot <- plot(poly)
#plot <- plot(atkin_16548_sub)

## Loop that is timed with system.time
system.time(
  for (i in 1:nrow(poly)) { #this is the number of polygons to iterate through
    single <- poly[i,] #selects a single polygon
    clip1 <- crop(raster, extent(single)) #crops the raster to the extent of the polygon, code author did this because it speeds the mask up
    clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
    
    ## Convert the clip2 rasters to WGS84
    newproj <- "+proj=longlat +datum=WGS84 +no_defs"
    clip22 <- projectRaster(clip2, crs=newproj)
    ## Need to change crs to NAD83 and then transform to WGS84
    #export <- st_as_sf(clip2,coords=c("dd_lon","dd_lat"), crs=4326, agr="constant")
    #st_write(export, dsn=paste0("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Ash_nelsopet//ROutputs//atkin_0_poly_RCS_samps", format(Sys.time(),"%Y%m%d"),".shp",sep=""),delete_layer = TRUE)
    ## Project shapefile
    #arcn_fia_gis_pts_prj <- arcn_fia_gis_pts %>% st_transform(102001)
    
    export <- stack(clip22, bands=NULL, native=FALSE, RAT=TRUE)
    #export2 <- as.array(export)

    #write.ENVI(export2, filename = paste(file.path("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Ash_nelsopet//ROutputs//atkin_16548_sub_RCS_samps"), i, sep=""))
    writeRaster(export, filename = paste(file.path("Thesis Data/Ash Trees/Ash_nelsopet/ROutputs/TIFFExtractedsamples/Atkinson/atkin_3983_samps"), i,".TIFF", sep=""))
    
    #ext<-extract(clip2,single) #extracts data from the raster based on the polygon bound
    #tab<-lapply(ext,table) #makes a table of the extract output
    #s<-sum(tab[[1]])  #sums the table for percentage calculation
    #mat<- as.data.frame(tab) 
    #mat2<- as.data.frame(tab[[1]]/s) #calculates percent
    #final<-cbind(single,mat,mat2$Freq) #combines into single dataframe
    #result<-rbind(final,result)
    
  })

# Outputs will overwrite in write.ENVI function

#raster2 <- brick("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//TestData//atkin_16548_sub_RCS_samps2.tif")



### Creating shapefile of buffers for assistance in creating buffer samples

setwd("E:/") # Set path for Atkinson samples

## Call & read in image to extract from
image <- brick("Chan_Thesis_Missions/Ash_07262019/100131_ash_atkinson1_2019_07_27_13_58_10/raw_15366_rd_rf_or")
#image <- brick("Chan_Thesis_Missions/Ash_07262019/100145_ash_nhti_2019_07_28_18_03_33/raw_2489_rd_rf_or")
#image <- brick("Chan_Thesis_Missions/Ash_07262019/100145_ash_nhti_2019_07_28_18_03_33/raw_4489_rd_rf_or")
#atkin_16548_sub_ENVI <- read.ENVI("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//TestData//atkin_16548_sub") # using this as the image only gives 1 band

# Read in point shapefile with readOGR
shp <- readOGR("Thesis Data/Ash Trees/Shapefiles/EAB_NH_Chan_Ash UAS NH000.shp")  

# Convert shapefile to simple feature and transforming the CRS so that units will be in feet
shp_nh <- st_as_sf(shp) %>% st_transform(3437)

# Buffer the transormed shapefile by each respective point's crown width divided by 2 (buffer radius) 
buffer <- st_buffer(shp_nh, shp$Crown_widt/2)

#buffer_test <- gBuffer(shp_2, width=(shp_2$Crown_widt/2))

# Reproject
st_crs(buffer, crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

# Convert to spatial object
buffer_spat <- as(buffer, "Spatial")

# st_transform(buffer_shp, CRS( "+proj=utm +zone=18N +datum=WGS84 +units=km"))
#  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +units=km"))
# "+init=epsg:3437"))#proj=merc +ellps=GRS1980 +units=m) )

# Write out shapefile using shapefile
shapefile(buffer_spat, filename= "Thesis Data/Ash Trees/treebuffer.shp")

# Write out shapefile using st_write
#st_write(buffer, dsn= "Thesis Data/Ash Trees/treebuffer2.shp")

# Write out shapefile using writeOGR (will overwrite the original shapefile)
#writeOGR(obj = buffer, dsn = "Thesis Data/Ash Trees/EAB_NH_Chan_Ash UAS NH000.shp", layer="treebuffer", driver = "ESRI Shapefile")

#Remove/mask out tree values so that only the ones we don't want are out of the image:
#buffer[buffer >1 ] <- NA

# Re-read in shapefile that was just written out above using st_read
buffer_shp_pre2<-st_read("Thesis Data/Ash Trees/treebuffer.shp")

# Re-read in shapefile that was just written out above using readOGR
buffer_shp_pre3<-readOGR("Thesis Data/Ash Trees/treebuffer.shp")

#geo_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#"+proj=tmerc +lat_0=42.5 +lon_0=-71.66666666666667 +k=0.999966667 +x_0=300000.0000000001 +y_0=0
#+ellps=GRS80 +units=us-ft +no_defs"

# Assigning the same coordinate reference system of the image to the buffered shapefile
crs(buffer_shp_pre3)=crs(image)
#buffer_shp_pre1 <- spTransform(buffer_shp_pre, geo_proj) #spatial polygon dataframe
#test_transformed2 <-st_as_sf(test_transformed)    # simple feature 

#test_transformed@bbox
#bb <- extent(-71.532171, -71.136296, 42.817516, 43.219969)
#test_transformed@bbox <- as.matrix(c(-71.532171, -71.136296, 42.817516, 43.219969)) #this kind of works...
#buffer_shp_pre3 <- extent(-71.532171, -71.136296, 42.817516, 43.219969) #THIS DOES SEEM TO WORK!
#test_transformedd <- replace(test_transformed@bbox, c(1020085.3, 1126554.6, 116145.1, 262440.8), c(-71.532171, -71.136296, 42.817516, 43.219969))
#extent(test_transformed) <- bb

#buffer_proj <- projectRaster(buffer, crs=newproj)
buffer_crop <- crop(x=buffer_shp_pre3, y=extent(image))
buffer_crop <- crop(x=buffer_shp_pre3, y=extent(-71.53196, -71.53107, 43.21874, 43.21936))

CP <- as(extent(-71.53196, -71.53107, 43.21874, 43.21936), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(image))

#out <- gIntersection(image, CP, byid = T)
out <- crop(bufferextent(buffer_shp_pre3))
out_test <- crop(image, shp_test)
mask_test <- mask(image, shp_test, inverse = T)

buffer_crop <- gIntersection(buffer_shp_pre3, extent(image))

buffer_crop1_spat <- as(buffer_crop1, "Spatial")

# testing if the image and buffer shapefile overlaps
cell <- extract(image, buffer_shp_pre3)



#buffer_crop <- crop(x=buffer_shp_pre, y=extent(image))

cube_mask <- raster::mask(image, buffer_shp_pre3) #filename = "Thesis Data/Ash Trees/Imagesamples/nhti_2489.tif")


## Plot the two read-ins, if desired
#plot <- plot(poly)
#plot <- plot(atkin_16548_sub)


## Call & read in polygon shapefile 
polybuffer <- st_read("Thesis Data/Ash Trees/Imagesamples/ImageBuffers/NHTI/nhti_2489_fullcrown.shp") 

## Call & read in image to extract from
rasterbuffer <- brick("Chan_Thesis_Missions/Ash_07262019/100145_ash_nhti_2019_07_28_18_03_33/raw_2489_rd_rf_or")

## Loop that is timed with system.time
system.time(
  for (i in 1:nrow(polybuffer)) { #this is the number of polygons to iterate through
    single <- polybuffer[i,] #selects a single polygon
    clip1 <- crop(rasterbuffer, extent(single)) #crops the raster to the extent of the polygon, code author did this because it speeds the mask up
    clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
    
    ## Convert the clip2 rasters to WGS84
    newproj <- "+proj=longlat +datum=WGS84 +no_defs"
    clip22 <- projectRaster(clip2, crs=newproj)
    ## Need to change crs to NAD83 and then transform to WGS84
    #export <- st_as_sf(clip2,coords=c("dd_lon","dd_lat"), crs=4326, agr="constant")
    #st_write(export, dsn=paste0("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Ash_nelsopet//ROutputs//atkin_0_poly_RCS_samps", format(Sys.time(),"%Y%m%d"),".shp",sep=""),delete_layer = TRUE)
    ## Project shapefile
    #arcn_fia_gis_pts_prj <- arcn_fia_gis_pts %>% st_transform(102001)
    
    export <- stack(clip22, bands=NULL, native=FALSE, RAT=TRUE)
    #export2 <- as.array(export)
    
    #write.ENVI(export2, filename = paste(file.path("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Ash_nelsopet//ROutputs//atkin_16548_sub_RCS_samps"), i, sep=""))
    writeRaster(export, filename = paste(file.path("Thesis Data/Ash Trees/Imagesamples/ImageBuffers/nhti_2489_samps"), i,".envi", sep=""))
    
    #ext<-extract(clip2,single) #extracts data from the raster based on the polygon bound
    #tab<-lapply(ext,table) #makes a table of the extract output
    #s<-sum(tab[[1]])  #sums the table for percentage calculation
    #mat<- as.data.frame(tab) 
    #mat2<- as.data.frame(tab[[1]]/s) #calculates percent
    #final<-cbind(single,mat,mat2$Freq) #combines into single dataframe
    #result<-rbind(final,result)
    
  })
