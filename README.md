### Ash_health

This repository includes the workflows and scripts used to classify ash health and stress (via crown dieback ratings). The prediction or classification method entails a random forest algorithm developed by the Nelson Lab. The data and scripts were used in a master’s thesis. 

Data can be made available upon request. 

Polygons were first created in ENVI over the canopies of ground-sampled points using the region of interest (ROI) tool. Each image contains one polygon shapefile with multiple polygons (sometimes only one). Samples could be omitted (ground sample was excluded in the imagery) or duplicated (ground sample was captured in 2 different images) which is denoted with a ‘D’ at the end of the sample number. A ‘.5’ at the end of the sample number indicates a sample number duplicate (mistakenly naming a sample with an existing number). Digitized samples are labeled as its site name, image number, and ‘samples’ (i.e. *atkinson_3983_samples.shp*) and are stored within the Imagesamples file under each location. 

Sample numbers under 100 are associated with location ‘New Hampshire Technical Institute’ (NHTI), numbers 100 and above, but below 200 are associated with Hooksett, and 200 and above are associated with location Atkinson.  

1.	*ash_extraction_furthertest1.R* script contains a loop that extracts all samples from the remote sensing image. The loop must be run for each image. All polygon extractions are saved as their own .tif file. These are stored under **ImageExtractions** under **OriginalData** and were all manually renamed to their sample number. This process was also conducted for shadow samples which are under **OriginalData**. 

2.	Any of the following 2 scripts   
*ashhealth_data_arrange.R* (converting image data to points and arranging by category)
*ash_shadow_data_arrange.R* (converting image data to points and arranging by category)

3.	*Spectral_classifier_ranger.R*  
Creates model and generates variable importance information to differentiate between shadow and non-shadow

4.	*Image_classifier.R*  
Classifies the image as shadow or non-shadow
Requires function *HyperspecGenFunctionRanger.R* and *LandCoverEstimator.R* both in folder **Functions**.

5.	*TestingScript.R*  
Masks, or assigns NA values to the areas classified as shadow

6.	*Spectral_classifier_ranger.R* (Again)  
Creates model and generates variable importance information to differentiate between health categories
7.	*Image_classifier.R* (Again)  
Classifies the image by health class  
Requires function *HyperspecGenFunctionRanger.R* and *LandCoverEstimator.R* both in folder **Functions**. 

8.	*buffer_extraction.R*  
Extracts classified images to only buffers (representing tree samples). These are included under **FinalOutputs**

9.	*classedbuffers_error.R*  
Used to calculate classification accuracy for pixels within buffers. 

10.	*Forest_spectra_plot.R*  
*Forest_spectra_plot2.R*  
Used to create plots of spectral curves for visual interpretation.
