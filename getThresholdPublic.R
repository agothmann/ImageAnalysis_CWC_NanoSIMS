# This function calculates and  returns the threshold for each respective isotope in order to 
# create datasets that highlight high intensity bands. It also returns a scatterplot of the
# data with the thresholds and regression line (also saves to pdf).

getThreshold <- function(data, iso1, iso2, path){
  
  fullData <- data %>% #creating a new clean dataset to use
    filter(value!=0)%>% #filtering out the outside region where the values=0
    mutate(variable=fct_recode(variable, "iso1"=iso1, "iso2"=iso2))%>% #changing data type of variable to a factor
    pivot_wider(names_from = variable, values_from = value) %>% #pivot data so that there is a row for each pixel,variable combination (makes dataset twice as long)
    filter(iso1 < quantile(iso1, 0.999, na.rm = TRUE),
           iso2 < quantile(iso2, 0.999, na.rm = TRUE))
  subsetData <- fullData%>% #sub-setting the data to speed up run time
    slice_sample(n=1000)
  
  model <- mblm(iso2~iso1, data=subsetData) #Theil Sen Regression
  summodel <- summary(model)
  print(model)

  iso1_max <- max(fullData$iso1, na.rm =TRUE)
  
  # This next part creates a table with three columns (iso1 cutoff, iso2 cutoff, 
  # and the associated pearson correlation coefficient for the data below those cutoffs), where
  # the cutoffs are 100 points along the regression line, starting at the highest value
  # When the coefficient < 0, break the loop and grab those cutoffs for the final threshold values
  n_steps <- 100
  iteration_steps <- tibble(iso1 = seq(iso1_max, 0, length = n_steps))
  
  iteration_steps <- iteration_steps %>%
    mutate(iso2 = predict(model, new = iteration_steps), cor = parse_number("."))
  
  for (i in 1:n_steps){
    iteration_data <- fullData %>%
      filter(iso1 < iteration_steps$iso1[i] | iso2 < iteration_steps$iso2[i])
    
    iteration_steps$cor[i] <- cor(iteration_data$iso1, iteration_data$iso2)
    
    if(iteration_steps$cor[i] < 0){
      break
    }
  }
  
  cutoffs <- iteration_steps[i, ]
  
  # store the iso1 and iso2 thresholds from Costes paper threshold methods
  iso1_thresh <- cutoffs$iso1
  iso2_thresh <- cutoffs$iso2
  
  print(list(iso1thresh=iso1_thresh))
  print(list(iso2thresh=iso2_thresh)) 
  
  # Creates the scatterplot of the data with regression line and threshold lines (in red)
  plot1 <- fullData %>%
    ggplot() +
    geom_point(aes(x = iso1, y = iso2),shape=".")+ #plots the values for iso1 and iso2 as a scatterplot
    geom_abline(slope=model$coefficients[[2]], intercept = model$coefficients[[1]])+ #adds a regression line with the slope from the Theil Sen regression model fit above
    geom_hline(yintercept = iso2_thresh, color="red")+ #adds a horizontal line at the iso2 threshold
    geom_vline(xintercept = iso1_thresh, color="red")+ #adds a vertical line at the iso1 threshold
    labs(x=iso1, y=iso2)
  print(model$coefficients[[2]])
  
  file_path = str_c(path, "/", iso1, "-", iso2, "_Threshold.pdf")
  pdf(file = file_path) # start saving output
  print(plot1) 
  dev.off() # stop saving output
  print(plot1)
  
  
  return(list(iso1_thresh=iso1_thresh, iso2_thresh=iso2_thresh, plot1))
}




