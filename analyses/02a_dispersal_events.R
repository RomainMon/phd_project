#------------------------------------------------#
# Author: Romain Monassier
# Objective: Extract dispersal events
#------------------------------------------------#

### Load packages ------
library(dplyr)
library(here)

### Load functions -----
source(here::here("R","APonchon","progress_bar.R"))

### Import datasets -----
load(here("data", "glt", "APonchon", "data_clean_long_final.RData"))

### Parameters ------
# Bad GLTs (e.g., unidentified individuals)
bad = c("?","T0","IN","?-1","?-2","?-3","FT","?-4","?-5","?-6", "FT")

### Identify dispersal events ------
# Filter out "bad" individuals, dead individuals, and already dispersed individuals
data.filter = data.clean.final %>%
  dplyr::filter(!GLT %in% bad & Disp != 1 & Death == 0) %>%
  dplyr::arrange(DateObs, GLT) %>%  # Sort by date and individual
  dplyr::group_by(GLT) # Group by individual


# Empty list
disp = NULL

# Unique group names
gp = sort(unique(data.filter$Group))

# Loop over groups
for (i in 1:length(gp)){
  
  # Subset the data for the current group 
  singlegp = data.filter %>% 
    dplyr::filter(Group==gp[i])
  
  # Get all unique observation dates for this group
  dateobs = sort(unique(singlegp$DateObs))

  # If there are at least 2 observations
  if(length(dateobs) > 1){
    # Compare two consecutive observations
    for (j in 2:length(dateobs)){
      
      # Observation BEFORE
      daygpbef = singlegp %>% 
        dplyr::filter(DateObs==dateobs[j-1])
      
      # Observation (CURRENT)
      daygp = singlegp %>% 
        dplyr::filter(DateObs==dateobs[j])
      
      # Flag missing individuals
      # i.e., individual present in the previous observation but missing in the current one = dispersal event
      miss = dplyr::anti_join(daygpbef,daygp,by="GLT")
      
      # Append to dispersal data frame
      if(nrow(miss) > 0){
        disp = rbind(disp,miss)
        
      }
      setTxtProgressBar(progress_bar(length(gp)),i)  
    }
  }
}

# Last observation for each individual that dispersed
emigr = disp %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::slice(max(ObsOrder)) %>% 
  as.data.frame(.)
