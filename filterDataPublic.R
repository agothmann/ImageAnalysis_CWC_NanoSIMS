# This function returns the MatLab file in a useable form for R for only the selected 
# isotope pair by analysis

filterData <- function(base_dir, iso1, iso2){
  coral_maps <- load_LANS_maps(    #converting MATLAB files into R
    base_dir = base_dir, 
    ion_data_only = FALSE,
    analysis = "TN_Aliz2_Pair4_Revised")
  
  filtered_coral_maps <- coral_maps %>%    #renaming data
    select(x.px, y.px, x.um, y.um, value, variable)%>%    #selecting variables of interest
    filter(variable %in% c(iso1, iso2))     #filtering by the two selected isotopes
  
  return(filtered_coral_maps)
}