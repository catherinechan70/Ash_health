

# MASKING OUT UNWANTED CATEGORIES
library(RStoolbox)
library(raster)
library(rgdal)
library(sf)

library(tidyverse)
library(sp)
library(raster)
library(readxl)

Headwall_wv<-scan("/Forests/OriginalData/Headwall_wv",numeric())

# Call in classified/prediction tif layer that acts as mask 
class_mask <- brick("E:/Forests/ROutputs/nhti_0_ashshadraw_0_rd_rf_or/raw_0_rd_rf_or_PredLayer.tif")

# Call in image to be masked (hyperspectral data cube)
image <- brick("/Chan_Thesis_Missions/Ash_07262019/100145_ash_nhti_2019_07_28_18_03_33/raw_0_rd_rf_or")

#"/Forests/Data_Sub_Images/Atkinson_15366_Sub"

#Remove/mask out tree values so that only the ones we don't want are out of the image:
class_mask[class_mask >1 ] <- NA


masked <- raster::mask(image, class_mask, filename = "/Forests/ROutputs/MaskedImages/nhti_0_shadmasked.envi")




### Subsetting large areas with ROI shapfiles (NHTI 0 & 2000 troubles)


library(maptools)  ## For wrld_simpl
library(raster)
library(sf)
library(caTools)
library(rgdal)
library(sp)
library(rgeos)

setwd("E:/") 


## Call & read in polygon shapefile 
poly <- st_read("Forests/OriginalData/nhti_0_crop.shp") 

## Call & read in image to extract from
raster <- brick("Forests/ROutputs/MaskedImages/nhti_0_shadmasked.envi")
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
    writeRaster(export, filename = paste(file.path("Forests/OriginalData/nhti_0_crop"), i,".TIFF", sep=""))
    
  
    
  })




# DISCRIMINATING TREE SPECIES AND MASKING OUT NON-ASH

image <- brick("E:/Chan_Thesis_Missions/Ash_07262019/100132_ash_atkinson2_2019_07_27_14_30_59/raw_16548_rd_rf_or")
s_mask <- brick("E:/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShadows/atkin_16548_7.tif")

s_mask_clip1 <- crop(image, extent(s_mask), inverse = TRUE)
s_mask_clip2 <- crop(image, extent(s_mask))

s_mask <- setValues(s_mask, values = values(s_mask),ext=extent(image),crs=crs(image),
                 nrows=dim(image)[1],ncols=dim(image)[2])
  
  #setExtent(s_mask, ext = extent(image))

cropped <- crop(image, poly, inverse = TRUE)


# load rasters
image <- stack("E:/Chan_Thesis_Missions/Ash_07262019/100132_ash_atkinson2_2019_07_27_14_30_59/raw_16548_rd_rf_or") 
# hyperspectral imagery

poly <- st_read("/Thesis Data/Ash Trees/Imagesamples/MaskShadows/atkin_16548_shad.shp")

s_mask <- raster("E:/Thesis Data/Ash Trees/ExtractedMaskSamples/ExtractedShadows/atkin_16548_7.tif")
s_mask <- raster(s_mask, ext = extent(image))
s_mask <- raster(vals=values(s_mask),ext=extent(image),crs=crs(image),
              nrows=dim(image)[1],ncols=dim(image)[2])

#s_mask <- raster(vals=values(s_mask), ext = extent(image), crs = crs(image), nrows = dim(image) [1], ncols = dim(image) [2])

# mask for cloud shadow
#extract shadowed area
m_cloud <- s_mask
m_cloud[m_cloud < 1] <- NA

newproj <- "+proj=longlat +datum=WGS84 +no_defs"
m_cloud_proj <- projectRaster(m_cloud, crs=newproj)

shadowed <- raster::mask(image, m_cloud_proj)
# calculate zonal statistics
 zd <- zonal(image, s_mask, fun = 'mean', na.rm = T)
### zd <- as.data.frame(zd)
# calculate ratio between lighted up and shadowed zones
### zd[3,] <- zd[1,] / zd[2,]

 