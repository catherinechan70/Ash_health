### SCRIPTS TO PLOT SAMPLE SPECTRA 
### to easily plot portions of spectrum and spectra of interest 
library(tidyverse)
library(sp)
library(rgdal)
library(raster)
library(readxl)
library(spectrolab)

setwd("D:/") 
#Set working drive

# Extension of 'Forests_spectra_plot.R', use environment from that script


# Wrangling all data pixels together to later plot median of all samples

sheet_dat_class <- sheet_data[,c(1,12)]

# Atkinson combine

atkin_bound <- bind_rows(atkin_raster_list, .id = "column_label")

atkin_alldf <- left_join(atkin_bound, sheet_dat_class, by = 'Tree_numbe')

# Hooksett combine

hook_bound <- bind_rows(hook_raster_list, .id = "column_label")

hook_alldf <- left_join(hook_bound, sheet_dat_class, by = 'Tree_numbe')

# NHTI combine

nhti_bound <- bind_rows(nhti_raster_list, .id = "column_label")

nhti_alldf <- left_join(nhti_bound, sheet_dat_class, by = 'Tree_numbe')

# Combine all location data frames

all_df <- bind_rows(atkin_alldf, hook_alldf, nhti_alldf)

# Remove rows and columns w/ NAs

# remove rows (NHTI tree numbers 3, 4, 5, 6 included but omitted from study-need to get rid of these rows)
all_df_naomit <- drop_na(all_df)

# all_df_naomit <- all_df_naomit_row[ , colSums(is.na(all_df_naomit_row))==0] 

# Split into smaller data frames by condition (results in 4 data frames)

list_byclass <- split(all_df_naomit, f=all_df_naomit$Condition_)

# Create column names:
class_names <- c('Class1', 'Class2', 'Class3', 'Class4')

# Create quantile values

quants <- c(0.875, 0.125, 0.95, 0.05)

# Create functions to find median, mean, max, min, quantiles
# Could not combine to create in one function...

calc_mean <- function(class_df)
{
  (apply(class_df[,4:329], 2, mean))
}

calc_med <- function(class_df)
{
  (apply(class_df[,4:329], 2, median))
}

calc_range <- function(class_df)
{
  (apply(class_df[,4:329], 2, range))
}

calc_quant <- function(class_df)
{
  (apply(class_df[,4:329], 2, quantile, probs = quants))
}

# Applying those functions to the list of 4 vigor class data frames

df_mean <- lapply(list_byclass, calc_mean) %>%  
   as.data.frame() %>% set_colnames(c("C1mean", "C2mean", "C3mean", "C4mean"))

df_med <- lapply(list_byclass, calc_med) %>%  
  as.data.frame() %>% set_colnames(c("C1med", "C2med", "C3med", "C4med"))

range <- lapply(list_byclass, calc_range)  
df_range <- do.call(rbind.data.frame, range) %>% 
  set_rownames(c("C1min", "C1max", "C2min", "C2max", "C3min", "C3max", "C4min", "C4max"))

quants <- lapply(list_byclass, calc_quant) 
df_quants <- do.call(rbind.data.frame, quants) 

# merge the stats data frames into one

allstats_df1 <- cbind(df_mean, df_med) %>% t()

allstats_df <- rbind(allstats_df1, df_range, df_quants)


# Spectra resampling using 'Spectrolab'

allstats_spec= as_spectra(allstats_df)

#class_spec_med_spec= as_spectra(class_spec_med_naomit)

#class_spec_mean_spec= as_spectra(class_spec_mean_naomit)

allstats_spec_resamp <- spectrolab::resample(allstats_spec, new_bands=seq(400, 999), parallel=FALSE)

#med_spec_resamp <- spectrolab::resample(class_spec_med_spec, new_bands=seq(404, 999), parallel=FALSE)

#mean_spec_resamp <- spectrolab::resample(class_spec_mean_spec, new_bands=seq(404, 999), parallel=FALSE)



allstats_spec_resamp_df <- as.data.frame(allstats_spec_resamp)  %>% 
  t() %>% as.data.frame() %>%   row_to_names(row_number = 1) %>% rownames_to_column()

allstats_spec_resamp_df <- lapply(allstats_spec_resamp_df,as.numeric) %>% as.data.frame()


#med_resamp_df <- as.data.frame(med_spec_resamp) %>% 
 # t() %>% as.data.frame() %>%   row_to_names(row_number = 1)

#mean_resamp_df <- as.data.frame(mean_spec_resamp) %>% 
 # t() %>% as.data.frame() %>%   row_to_names(row_number = 1)


# Plot the spectra

plot(rownames(allstats_spec_resamp_df), allstats_spec_resamp_df$C1mean, type = "l", col = 1)  # Plot with Base R
lines(rownames(allstats_spec_resamp_df), allstats_spec_resamp_df$C2mean, type = "l", col = 2)
lines(rownames(allstats_spec_resamp_df), allstats_spec_resamp_df$C3mean, type = "l", col = 3)
lines(rownames(allstats_spec_resamp_df), allstats_spec_resamp_df$C4mean, type = "l", col = 4)
#legend(x="topleft", legend=levels(allstats_spec_resamp_df[,1:4]), pch=16, col=unique(group))
      # legend=c("Class1", "Class2", "Class3", "Class4"))

# plot mean

ggplot() + 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C1mean, color = "class 1", group=1), size=.75)+
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C2mean, color = "class 2"), size=.75)+ 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C3mean, color = "class 3"), size=.75)+ 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C4mean, color = "class 4"), size=.75)+ 
  
  scale_color_discrete(name="Health Class", labels=c("Class 1", "Class 2" , "Class 3", "Class 4")) +

  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(400,900)+
  ylim(0, 1) +
  theme_bw()+
  ggtitle("Mean values")


# plot median

ggplot() + 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C1med, color = "class 1", group=1), size=.75)+
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C2med, color = "class 2"), size=.75)+ 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C3med, color = "class 3"), size=.75)+ 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C4med, color = "class 4"), size=.75)+ 
  
  scale_color_discrete(name="Health Class", labels=c("Class 1", "Class 2" , "Class 3", "Class 4")) +
  
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(400,900)+
  ylim(0, 1) +
  theme_bw()+
  ggtitle("Med values")

# plot median of vigor class 1 w/ interquartile ranges (original)

ggplot(allstats_spec_resamp_df) + 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C1med, color = "class 1", group=1), size=.75)+
  geom_ribbon(aes(x=rowname, ymin = X1.12.5., ymax = X1.87.5.), alpha = 0.3) +
  geom_ribbon(aes(x=rowname, ymin = C1min, ymax = C1max), alpha = 0.2)+ 
  
  scale_color_discrete(name="Health Class", labels=c("Class 1")) +
  
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(400,900)+
  ylim(0, 1) +
  theme_bw()+
  ggtitle("C1 Median at 87.5%")


# plot median of vigor classes w/ interquartile ranges, then plot as 4-panel figure

C1med <- ggplot(allstats_spec_resamp_df) + 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C1med, color = "class 1", group=1), show.legend = F, size=.75)+
  geom_ribbon(aes(x=rowname, ymin = X1.12.5., ymax = X1.87.5.), alpha = 0.3) +
  geom_ribbon(aes(x=rowname, ymin = C1min, ymax = C1max), alpha = 0.2)+ 
  
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(400,900)+
  ylim(0, 1) +
  theme_bw()+
  ggtitle("C1 Median at 87.5%")

C2med <- ggplot(allstats_spec_resamp_df) + 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C2med, color = "class 2", group=1), show.legend = F, size=.75)+
  geom_ribbon(aes(x=rowname, ymin = X2.12.5., ymax = X2.87.5.), alpha = 0.3) +
  geom_ribbon(aes(x=rowname, ymin = C2min, ymax = C2max), alpha = 0.2)+ 
  
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(400,900)+
  ylim(0, 1) +
  theme_bw()+
  ggtitle("C2 Median at 87.5%")

C3med <- ggplot(allstats_spec_resamp_df) + 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C3med, color = "class 3", group=3), show.legend = F, size=.75)+
  geom_ribbon(aes(x=rowname, ymin = X3.12.5., ymax = X3.87.5.), alpha = 0.3) +
  geom_ribbon(aes(x=rowname, ymin = C3min, ymax = C3max), alpha = 0.2)+ 
  
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(400,900)+
  ylim(0, 1) +
  theme_bw()+
  ggtitle("C3 Median at 87.5%")

C4med <- ggplot(allstats_spec_resamp_df) + 
  geom_line(data = allstats_spec_resamp_df, aes(x = rowname, y = C4med, color = "class 4", group=4), show.legend = F, size=.75)+
  geom_ribbon(aes(x=rowname, ymin = X4.12.5., ymax = X4.87.5.), alpha = 0.3) +
  geom_ribbon(aes(x=rowname, ymin = C4min, ymax = C4max), alpha = 0.2)+ 
  
  xlab('Wavelength') +
  ylab('Reflectance') + 
  xlim(400,900)+
  ylim(0, 1) +
  theme_bw()+
  ggtitle("C4 Median at 87.5%")

plot_grid(C1med,C2med, C3med, C4med, labels="auto")


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






