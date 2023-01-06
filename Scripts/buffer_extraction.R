
# Test Scripts to avoid using same project environment while Forest/Blueberry 
# scripts are running

# Working to extract buffers from images (Forests)

library(maptools)  ## For wrld_simpl
library(raster)
library(sf)
library(caTools)
library(rgdal)
library(sp)
library(rgeos)

setwd("E:/") 


# Call in buffer shapefile

buffer_poly <- st_read("/Thesis Data/Ash Trees/treebuffer.shp")

buffer_poly2 <- readOGR("/Thesis Data/Ash Trees/treebuffer.shp")

# Call in twice classified image

health_raster <- brick("/Forests/ROutputs/MaskedImages/nhti_0_health2nhti_0_crop.tif/nhti_0_crop.tif_PredLayer.tif")

# EXTRACT! 

buffer_poly2 <- spTransform(x = buffer_poly2, CRSobj = "+proj=longlat +datum=WGS84 +no_defs")

cropped <- mask(x=health_raster, mask = buffer_poly2)

cropped <- crop(buffer_poly2, extent(health_raster))

cropped_sf <- st_as_sf(cropped)



system.time(
  for (i in 1:nrow(cropped_sf)) { #this is the number of polygons to iterate through
    single <- cropped_sf[i,] #selects a single polygon
    clip1 <- crop(health_raster, extent(single)) #crops the raster to the extent of the polygon, code author did this because it speeds the mask up
    clip2 <- mask(clip1,single) #crops the raster to the polygon boundary
    
    ## Convert the clip2 rasters to WGS84
    newproj <- "+proj=longlat +datum=WGS84 +no_defs"
    clip22 <- projectRaster(clip2, crs=newproj, method = "ngb")
    ## Need to change crs to NAD83 and then transform to WGS84
    #export <- st_as_sf(clip2,coords=c("dd_lon","dd_lat"), crs=4326, agr="constant")
    #st_write(export, dsn=paste0("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Ash_nelsopet//ROutputs//atkin_0_poly_RCS_samps", format(Sys.time(),"%Y%m%d"),".shp",sep=""),delete_layer = TRUE)
    ## Project shapefile
    #arcn_fia_gis_pts_prj <- arcn_fia_gis_pts %>% st_transform(102001)
    
    #export <- stack(clip22, bands=NULL, native=FALSE, RAT=TRUE)
    #export2 <- as.array(export)
    
    #write.ENVI(export2, filename = paste(file.path("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Ash_nelsopet//ROutputs//atkin_16548_sub_RCS_samps"), i, sep=""))
    writeRaster(clip22, filename = paste(file.path("Forests/FinalOutputs/0_image_samps"), i,".TIFF", sep=""))
    
    #ext<-extract(clip2,single) #extracts data from the raster based on the polygon bound
    #tab<-lapply(ext,table) #makes a table of the extract output
    #s<-sum(tab[[1]])  #sums the table for percentage calculation
    #mat<- as.data.frame(tab) 
    #mat2<- as.data.frame(tab[[1]]/s) #calculates percent
    #final<-cbind(single,mat,mat2$Freq) #combines into single dataframe
    #result<-rbind(final,result)
    
  })


