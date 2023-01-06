
### Extracting from HDR image (subsetted is best) using a shapefile of polygons

library(maptools)  ## For wrld_simpl
library(raster)
library(sf)
library(caTools)
library(rgdal)
library(sp)

setwd("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees")

## Call & read in polygon shapefile 

poly <- st_read("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Imagesamples//NHTI//nhti_2489_samples.shp")

#poly <- data.frame(unlist(pre_poly), nrow=length(pre_poly), byrow=T)

## Call & read in image to extract from
raster <- brick("E://Chan_Thesis_Missions//Ash_07262019//100145_ash_nhti_2019_07_28_18_03_33//raw_2489_rd_rf_or")
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
    writeRaster(export, filename = paste(file.path("C://Users//catherine.chan//Desktop//Thesis Data//Ash Trees//Ash_nelsopet//ROutputs//TIFFExtractedsamples//NHTI//nhti_2489_samps"), i,".TIFF", sep=""))
    
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
