### SCRIPTS TO PLOT SAMPLE SPECTRA 
## to easily plot portions of spectrum and spectra of interest 
library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)
library(spectrolab)

setwd("D:/") 
#Set working drive
sheet_data <- read_excel(path = "Forests/OriginalData/EAB_NH_Chan_OnlyAsh.xlsx")
sheet_data$Tree_numbe<-gsub(".tif","",sheet_data$Tree_numbe)

##Read in headwall wavelength
Headwall_wv<-scan("/Forests/OriginalData/Headwall_wv",numeric())


##All functions above worked now you want to integrate those into a loop or mapped function
##For now we'll apply each function above using the lines of code below

#ATKINSON
setwd("/Forests/OriginalData/Atkinson") # Set path for Atkinson samples
mypath_atkin = "/Forests/OriginalData/Atkinson"
# Import names of Atkinson sample tif files into character list
atkin_samples = list.files(mypath_atkin, pattern="\\.tif$")

atkin_raster_list<-lapply(atkin_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",atkin_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
atkin_raster_list <- Map(cbind, atkin_raster_list, Tree_numbe = names(atkin_raster_list))

#Renames columns to headwall bandpasses
atkin_raster_list <- lapply(atkin_raster_list, function(atkin_df) {
  names(atkin_df)[3:328] <- Headwall_wv
  atkin_df
})



#HOOKSETT
setwd("/Forests/OriginalData/Hooksett/") # Set path for Atkinson samples
mypath_hook = "/Forests/OriginalData/Hooksett"
# Import names of Atkinson sample tif files into character list
hook_samples = list.files(mypath_hook, pattern="\\.tif$")

hook_raster_list<-lapply(hook_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",hook_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
hook_raster_list <- Map(cbind, hook_raster_list, Tree_numbe = names(hook_raster_list))

#Renames columns to headwall bandpasses
hook_raster_list <- lapply(hook_raster_list, function(df) {
  names(df)[3:328] <- Headwall_wv
  df
})



#NHTI
setwd("/Forests/OriginalData/NHTI/") # Set path for Atkinson samples
mypath_nhti = "/Forests/OriginalData/NHTI"
# Import names of Atkinson sample tif files into character list
nhti_samples = list.files(mypath_nhti, pattern="\\.tif$")

nhti_raster_list<-lapply(nhti_samples,function(x){
  brick(x)%>% # Reads in the hypersectral raster as a brick object
    rasterToPoints()%>% # Converts Ratser to points data, this step is needed before raster can be converted to a dataframe
    as.data.frame()})%>% # Convert raster to dataframe
  setNames(gsub(".tif","",nhti_samples))  # Substitutes file name and removes .tif from the name

#Add column Tree_numbe to dataframe
nhti_raster_list <- Map(cbind, nhti_raster_list, Tree_numbe = names(nhti_raster_list))

#Renames columns to headwall bandpasses
nhti_raster_list <- lapply(nhti_raster_list, function(nhti_df) {
  names(nhti_df)[3:328] <- Headwall_wv
  nhti_df
})

# Lines 90-565: Taking individual spectral signatures of different sites and 
# classes to plot, with ggplots...
# All samples used, starts at line 567
#Category 1 (235_WA / 237_WA / 237D_WA)

#235_WA
atkin_235 <- atkin_raster_list$"235"[-c(1,2,329)]

atkin_235 <- rbind(colMeans = colMeans(atkin_235, na.rm = TRUE),
                atkin_235) 

atkin_235 <- rbind(colnames(atkin_235),
                atkin_235)

atkin_235 <- atkin_235[-c(3:nrow(atkin_235)), ]

atkin_235_long <- as.data.frame(t(atkin_235))

names(atkin_235_long)[names(atkin_235_long) == "1"] <- "Wavelength"
names(atkin_235_long)[names(atkin_235_long) == "2"] <- "Reflectance"

atkin_235_long[ ,2] <- as.numeric(as.character( atkin_235_long[ ,2]))

atkin_235_long$Wavelength = as.numeric(as.character(atkin_235_long$Wavelength))


#237_WA
atkin_237 <- atkin_raster_list$"237"[-c(1,2,329)]

atkin_237 <- rbind(colMeans = colMeans(atkin_237, na.rm = TRUE),
                atkin_237) 

atkin_237 <- rbind(colnames(atkin_237),
                atkin_237)

atkin_237 <- atkin_237[-c(3:nrow(atkin_237)), ]

atkin_237_long <- as.data.frame(t(atkin_237))

names(atkin_237_long)[names(atkin_237_long) == "1"] <- "Wavelength"
names(atkin_237_long)[names(atkin_237_long) == "2"] <- "Reflectance"

atkin_237_long[ ,2] <- as.numeric(as.character( atkin_237_long[ ,2]))

atkin_237_long$Wavelength = as.numeric(as.character(atkin_237_long$Wavelength))


#202_WA
atkin_202 <- atkin_raster_list$"202"[-c(1,2,329)]

atkin_202 <- rbind(colMeans = colMeans(atkin_202, na.rm = TRUE),
                   atkin_202) 

atkin_202 <- rbind(colnames(atkin_202),
                   atkin_202)

atkin_202 <- atkin_202[-c(3:nrow(atkin_202)), ]

atkin_202_long <- as.data.frame(t(atkin_202))

names(atkin_202_long)[names(atkin_202_long) == "1"] <- "Wavelength"
names(atkin_202_long)[names(atkin_202_long) == "2"] <- "Reflectance"

atkin_202_long[ ,2] <- as.numeric(as.character( atkin_202_long[ ,2]))

atkin_202_long$Wavelength = as.numeric(as.character(atkin_202_long$Wavelength))




#237D_WA

atkin_237D <- atkin_raster_list$"237D"[-c(1,2,329)]

atkin_237D <- rbind(colMeans = colMeans(atkin_237D, na.rm = TRUE),
                atkin_237D) 

atkin_237D <- rbind(colnames(atkin_237D),
                atkin_237D)

atkin_237D <- atkin_237D[-c(3:nrow(atkin_237D)), ]

atkin_237D_long <- as.data.frame(t(atkin_237D))

names(atkin_237D_long)[names(atkin_237D_long) == "1"] <- "Wavelength"
names(atkin_237D_long)[names(atkin_237D_long) == "2"] <- "Reflectance"

atkin_237D_long[ ,2] <- as.numeric(as.character( atkin_237D_long[ ,2]))

atkin_237D_long$Wavelength = as.numeric(as.character(atkin_237D_long$Wavelength))


#Category 2 (102_GA / 110_GA / 108_GA)

#102_GA

hook_102 <- hook_raster_list$"102"[-c(1,2,329)]

hook_102 <- rbind(colMeans = colMeans(hook_102, na.rm = TRUE),
                    hook_102) 

hook_102 <- rbind(colnames(hook_102),
                    hook_102)

hook_102 <- hook_102[-c(3:nrow(hook_102)), ]

hook_102_long <- as.data.frame(t(hook_102))

names(hook_102_long)[names(hook_102_long) == "1"] <- "Wavelength"
names(hook_102_long)[names(hook_102_long) == "2"] <- "Reflectance"

hook_102_long[ ,2] <- as.numeric(as.character( hook_102_long[ ,2]))

hook_102_long$Wavelength = as.numeric(as.character(hook_102_long$Wavelength))

#110_GA

hook_110 <- hook_raster_list$"110"[-c(1,2,329)]

hook_110 <- rbind(colMeans = colMeans(hook_110, na.rm = TRUE),
                  hook_110) 

hook_110 <- rbind(colnames(hook_110),
                  hook_110)

hook_110 <- hook_110[-c(3:nrow(hook_110)), ]

hook_110_long <- as.data.frame(t(hook_110))

names(hook_110_long)[names(hook_110_long) == "1"] <- "Wavelength"
names(hook_110_long)[names(hook_110_long) == "2"] <- "Reflectance"

hook_110_long[ ,2] <- as.numeric(as.character( hook_110_long[ ,2]))

hook_110_long$Wavelength = as.numeric(as.character(hook_110_long$Wavelength))


#108_GA

hook_108 <- hook_raster_list$"108"[-c(1,2,329)]

hook_108 <- rbind(colMeans = colMeans(hook_108, na.rm = TRUE),
                  hook_108) 

hook_108 <- rbind(colnames(hook_108),
                  hook_108)

hook_108 <- hook_108[-c(3:nrow(hook_108)), ]

hook_108_long <- as.data.frame(t(hook_108))

names(hook_108_long)[names(hook_108_long) == "1"] <- "Wavelength"
names(hook_108_long)[names(hook_108_long) == "2"] <- "Reflectance"

hook_108_long[ ,2] <- as.numeric(as.character( hook_108_long[ ,2]))

hook_108_long$Wavelength = as.numeric(as.character(hook_108_long$Wavelength))



#Category 3 (1_WA / 31_WA / 226_WA)

#1_WA

nhti_1 <- nhti_raster_list$"1"[-c(1,2,329)]

nhti_1 <- rbind(colMeans = colMeans(nhti_1, na.rm = TRUE),
                  nhti_1) 

nhti_1 <- rbind(colnames(nhti_1),
                  nhti_1)

nhti_1 <- nhti_1[-c(3:nrow(nhti_1)), ]

nhti_1_long <- as.data.frame(t(nhti_1))

names(nhti_1_long)[names(nhti_1_long) == "1"] <- "Wavelength"
names(nhti_1_long)[names(nhti_1_long) == "2"] <- "Reflectance"

nhti_1_long[ ,2] <- as.numeric(as.character( nhti_1_long[ ,2]))

nhti_1_long$Wavelength = as.numeric(as.character(nhti_1_long$Wavelength))



#31_WA

nhti_31 <- nhti_raster_list$"31"[-c(1,2,329)]

nhti_31 <- rbind(colMeans = colMeans(nhti_31, na.rm = TRUE),
                nhti_31) 

nhti_31 <- rbind(colnames(nhti_31),
                nhti_31)

nhti_31 <- nhti_31[-c(3:nrow(nhti_31)), ]

nhti_31_long <- as.data.frame(t(nhti_31))

names(nhti_31_long)[names(nhti_31_long) == "1"] <- "Wavelength"
names(nhti_31_long)[names(nhti_31_long) == "2"] <- "Reflectance"

nhti_31_long[ ,2] <- as.numeric(as.character( nhti_31_long[ ,2]))

nhti_31_long$Wavelength = as.numeric(as.character(nhti_31_long$Wavelength))


#226_WA

atkin_226 <- atkin_raster_list$"226"[-c(1,2,329)]

atkin_226 <- rbind(colMeans = colMeans(atkin_226, na.rm = TRUE),
                   atkin_226) 

atkin_226 <- rbind(colnames(atkin_226),
                   atkin_226)

atkin_226 <- atkin_226[-c(3:nrow(atkin_226)), ]

atkin_226_long <- as.data.frame(t(atkin_226))

names(atkin_226_long)[names(atkin_226_long) == "1"] <- "Wavelength"
names(atkin_226_long)[names(atkin_226_long) == "2"] <- "Reflectance"

atkin_226_long[ ,2] <- as.numeric(as.character( atkin_226_long[ ,2]))

atkin_226_long$Wavelength = as.numeric(as.character(atkin_226_long$Wavelength))


#Category 4 (11_GA / 18_WA / 208.5D_WA)

#11_GA

nhti_11 <- nhti_raster_list$"11"[-c(1,2,329)]

nhti_11 <- rbind(colMeans = colMeans(nhti_11, na.rm = TRUE),
                 nhti_11) 

nhti_11 <- rbind(colnames(nhti_11),
                 nhti_11)

nhti_11 <- nhti_11[-c(3:nrow(nhti_11)), ]

nhti_11_long <- as.data.frame(t(nhti_11))

names(nhti_11_long)[names(nhti_11_long) == "1"] <- "Wavelength"
names(nhti_11_long)[names(nhti_11_long) == "2"] <- "Reflectance"

nhti_11_long[ ,2] <- as.numeric(as.character( nhti_11_long[ ,2]))

nhti_11_long$Wavelength = as.numeric(as.character(nhti_11_long$Wavelength))

#18_WA

nhti_18 <- nhti_raster_list$"18"[-c(1,2,329)]

nhti_18 <- rbind(colMeans = colMeans(nhti_18, na.rm = TRUE),
                  nhti_18) 

nhti_18 <- rbind(colnames(nhti_18),
                  nhti_18)

nhti_18 <- nhti_18[-c(3:nrow(nhti_18)), ]

nhti_18_long <- as.data.frame(t(nhti_18))

names(nhti_18_long)[names(nhti_18_long) == "1"] <- "Wavelength"
names(nhti_18_long)[names(nhti_18_long) == "2"] <- "Reflectance"

nhti_18_long[ ,2] <- as.numeric(as.character( nhti_18_long[ ,2]))

nhti_18_long$Wavelength = as.numeric(as.character(nhti_18_long$Wavelength))


#208.5D_WA

atkin_208.5D <- atkin_raster_list$"208.5D"[-c(1,2,329)]

atkin_208.5D <- rbind(colMeans = colMeans(atkin_208.5D, na.rm = TRUE),
                   atkin_208.5D) 

atkin_208.5D <- rbind(colnames(atkin_208.5D),
                   atkin_208.5D)

atkin_208.5D <- atkin_208.5D[-c(3:nrow(atkin_208.5D)), ]

atkin_208.5D_long <- as.data.frame(t(atkin_208.5D))

names(atkin_208.5D_long)[names(atkin_208.5D_long) == "1"] <- "Wavelength"
names(atkin_208.5D_long)[names(atkin_208.5D_long) == "2"] <- "Reflectance"

atkin_208.5D_long[ ,2] <- as.numeric(as.character( atkin_208.5D_long[ ,2]))

atkin_208.5D_long$Wavelength = as.numeric(as.character(atkin_208.5D_long$Wavelength))



#Test

hook_108 <- hook_raster_list$"108"[-c(1,2,329)]

hook_108 <- rbind(colMeans = colMeans(hook_108, na.rm = TRUE),
                      hook_108) 

hook_108 <- rbind(colnames(hook_108),
                      hook_108)

hook_108 <- hook_108[-c(3:nrow(hook_108)), ]

hook_108_long <- as.data.frame(t(hook_108))

names(hook_108_long)[names(hook_108_long) == "1"] <- "Wavelength"
names(hook_108_long)[names(hook_108_long) == "2"] <- "Reflectance"

hook_108_long[ ,2] <- as.numeric(as.character( hook_108_long[ ,2]))

hook_108_long$Wavelength = as.numeric(as.character(hook_108_long$Wavelength))



# Plotting samples for each date on one plot 

ggplot() + 
  geom_line(data = atkin_235_long, aes(x = Wavelength, y = Reflectance), color = "firebrick", size=.75) +
  geom_line(data = atkin_237_long, aes(x = Wavelength, y = Reflectance), color = "chartreuse4", size=.75) +
  geom_line(data = atkin_237D_long, aes(x = Wavelength, y = Reflectance), color = "deepskyblue2", size=.75) +
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(390,1001)+
  ylim(0, 1) +
  theme(panel.background= element_rect(fill="snow2"),
        panel.grid.major = element_line(colour = "gray", size=0.6),
        panel.grid.minor = element_line(colour = "deepskyblue3",
                                        size=.03,
                                        linetype = "solid")) +
  
  ggtitle("Category 1")


#setwd("E:/Blueberries/ROutputs/Spectra_exports")
#ggsave("B4_signature.jpeg")



# All 4 categories NEW


all_plotdf <- cbind(atkin_235_long$Wavelength, atkin_235_long$Reflectance, atkin_237D_long$Reflectance, atkin_202_long$Reflectance, hook_108_long$Reflectance, 
                    nhti_1_long$Reflectance, nhti_31_long$Reflectance, nhti_11_long$Reflectance, nhti_18_long$Reflectance)%>%
#`rownames<-`(atkin_235_long$Wavelength) %>% 
  t() %>%
  row_to_names(row_number = 1)%>%
  as.data.frame()

all_plotdf_spec = as_spectra(all_plotdf)

all_plotdf_resamp <- spectrolab::resample(all_plotdf_spec, new_bands=seq(400, 999, 0.5), parallel=FALSE)
                    
all_plotdf_resamp_df <- as.data.frame(all_plotdf_resamp) %>% 
  t() %>% as.data.frame() %>% slice(-1) %>% rownames_to_column()

colnames(all_plotdf_resamp_df) <- c("Wavelength", "Class1", "Class12", "Class2", "Class22","Class3","Class32","Class4","Class42")

#d <- melt(all_plotdf_resamp_df, id.vars="Wavelength")

#ggplot(d, aes(Wavelength,value, col=variable, group=1)) + 
 # geom_line() 

ggplot(data=all_plotdf_resamp_df)+
  geom_line(mapping = aes(x=Wavelength, y = Class1, color = "Class 1", group=1), size = 0.75) +
  #geom_line(mapping = aes(x=Wavelength, y = Class12, color = "Class12"), size = 0.75) +
  
  #geom_line(mapping = aes(x=Wavelength, y = Class2, color = "Class 2"), size = 0.75) +
  #geom_line(mapping = aes(x=Wavelength, y = Class22, color = "Class22"), size = 0.75) +
  #
  #geom_line(mapping = aes(x=Wavelength, y = Class3, color = "Class 3"), size = 0.75) +
  #geom_line(mapping = aes(x=Wavelength, y = Class32, color = "Class32"), size = 0.75) +
  #
  #geom_line(mapping = aes(x=Wavelength, y = Class4, color = "Class 4"), size = 0.75) +
  #geom_line(mapping = aes(x=Wavelength, y = Class42, color = "Class42"), size = 0.75) +
  #
  #scale_color_manual(values = c('Class 1'="deepskyblue2", 'Reflectance2'="deepskyblue2", 'Class 2'="chartreuse4",
                               # 'Reflectance4'="chartreuse4", 'Class 3'="darkorange1", 'Reflectance6'="darkorange1", 
                                #'Class 4'="firebrick", 'Reflectance8'="firebrick"), breaks = c('Class 1', 'Class 2', 'Class 3', 'Class 4')) +

  
#'Reflectance1'="deepskyblue2", 'Reflectance2'="deepskyblue2", 'Reflectance3'="chartreuse4",
#'Reflectance4'="chartreuse4", 'Reflectance5'="darkorange1", 'Reflectance6'="darkorange1", 
#'Reflectance7'="firebrick", 'Reflectance8'="firebrick"
  
  labs(color='Health Class') +
  
  xlab('Wavelength nm') +
  ylab('Reflectance %') 
  #xlim(550,800)+
  #ylim(0, 0.75) +
  
  ggtitle("Ash Health Classes 550-800nm ")


setwd("E:/Forests/ROutputs/Spectra_exports/")
#ggsave("all_class_spectra550_800_2.jpeg")



ggplot() + 
  geom_line(data = all_plotdf_resamp_df, aes(x = Wavelength, y = Class1, group=1), color = "darkblue", size=.75) +
  #geom_line(data = atkin_237_long, aes(x = Wavelength, y = Reflectance), color = "deepskyblue2", size=.75) +
  geom_line(data = all_plotdf_resamp_df, aes(x = Wavelength, y = Class2, group=1), color = "deepskyblue3", size=.75) +
  
  #geom_line(data = hook_102_long, aes(x = Wavelength, y = Reflectance), color = "chartreuse1", size=.75) +
  geom_line(data = all_plotdf_resamp_df, aes(x = Wavelength, y = Class3, group=1), color = "chartreuse4", size=.75) +
  geom_line(data = all_plotdf_resamp_df, aes(x = Wavelength, y = Class4, group=1), color = "aquamarine", size=.75) +
  geom_line(data = all_plotdf_resamp_df, aes(x = Wavelength, y = Class12, group=1), color = "yellow", size=.75) +
  
  
  #geom_line(data = nhti_1_long, aes(x = Wavelength, y = Reflectance), color = "darkorange1", size=.75) +
  #geom_line(data = nhti_31_long, aes(x = Wavelength, y = Reflectance), color = "darkorange4", size=.75) +
  #geom_line(data = atkin_226_long, aes(x = Wavelength, y = Reflectance), color = "darkorange1", size=.75) +
  
  #geom_line(data = nhti_11_long, aes(x = Wavelength, y = Reflectance), color = "firebrick", size=.75) +
  #geom_line(data = nhti_18_long, aes(x = Wavelength, y = Reflectance), color = "darkmagenta", size=.75) +
  #geom_line(data = atkin_219_long, aes(x = Wavelength, y = Reflectance), color = "deeppink2", size=.75) +
  
  #scale_color_discrete(name="Health Class", labels=c("Class 1", "Class 2" , "Class 3", "Class 4") ) +
scale_x_discrete()+
#scale_color_manual(values = c("darkblue"="deepskyblue2")) +
  
  
  xlab('Wavelength') +
  ylab('Reflectance') #+ 
  #xlim(390,1001)+
  #ylim(0, 1)


# All 4 categories
ggplot() + 
  geom_line(data = atkin_235_long, aes(x = Wavelength, y = Reflectance, color = "darkblue"), size=.75) +
  #geom_line(data = atkin_237_long, aes(x = Wavelength, y = Reflectance), color = "deepskyblue2", size=.75) +
  geom_line(data = atkin_237D_long, aes(x = Wavelength, y = Reflectance), color = "deepskyblue3", size=.75) +
  
  #geom_line(data = hook_102_long, aes(x = Wavelength, y = Reflectance), color = "chartreuse1", size=.75) +
  geom_line(data = hook_110_long, aes(x = Wavelength, y = Reflectance), color = "chartreuse4", size=.75) +
  geom_line(data = atkin_202_long, aes(x = Wavelength, y = Reflectance), color = "aquamarine", size=.75) +
  geom_line(data = hook_108_long, aes(x = Wavelength, y = Reflectance), color = "yellow", size=.75) +
  
  
  #geom_line(data = nhti_1_long, aes(x = Wavelength, y = Reflectance), color = "darkorange1", size=.75) +
  #geom_line(data = nhti_31_long, aes(x = Wavelength, y = Reflectance), color = "darkorange4", size=.75) +
  #geom_line(data = atkin_226_long, aes(x = Wavelength, y = Reflectance), color = "darkorange1", size=.75) +
  
  #geom_line(data = nhti_11_long, aes(x = Wavelength, y = Reflectance), color = "firebrick", size=.75) +
  #geom_line(data = nhti_18_long, aes(x = Wavelength, y = Reflectance), color = "darkmagenta", size=.75) +
  #geom_line(data = atkin_219_long, aes(x = Wavelength, y = Reflectance), color = "deeppink2", size=.75) +
  
  #scale_color_discrete(name="Health Class", labels=c("Class 1", "Class 2" , "Class 3", "Class 4") ) +
  scale_color_manual(values = c("darkblue"="deepskyblue2")) +
  
  
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(390,1001)+
  ylim(0, 1) +
  #theme(#panel.background= element_rect(fill="snow2"),
        #panel.grid.major = element_line(colour = "gray", size=0.6),
        #panel.grid.minor = element_line(colour = "deepskyblue3",
                                        #size=.03,
                                        #linetype = "solid")) +
  
  ggtitle("All Categories")

setwd("E:/Forests/ROutputs/Spectra_exports/")
#ggsave("all_class_spectra.jpeg")







# Wrangling all data to plot median of all samples

atkin_alldf <- bind_rows(atkin_raster_list, .id = "column_label")


# Find median reflectance of all samples, using vigor as grouping category

dplyr::summarise(Median_Reflectance = median(Reflectance),
                 Max_Reflectance = max(Reflectance),
                 Min_Reflectance = min(Reflectance),
                 Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                 Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                 Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                 Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength))  %>%
  as.data.frame() %>%
  dplyr::filter(Wavelength>419 & Wavelength<850)






