







### Transition rasters ----
# Selected rasters
rasters_selected = rasters_merged[idx_selected]
# Transition rasters
transition_rasters = list()

for(i in 1:(length(rasters_selected) - 1)) {
  from_r = rasters_selected[[i]]
  to_r   = rasters_selected[[i + 1]]
  
  # compute transition: from*10 + to
  tr_r = from_r * 10 + to_r
  
  # name and store
  names(tr_r) = paste0(years_selected[i], "_to_", years_selected[i+1])
  transition_rasters[[i]] = tr_r
}

# Now each cell contains a numeric code indicating the transition. For example: 31 → changed from class 3 → 1
plot(transition_rasters[[1]])
unique(transition_rasters[[1]])


