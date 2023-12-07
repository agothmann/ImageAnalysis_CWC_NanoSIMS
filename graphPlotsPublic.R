# This function graphs important plots regarding the two specified isotopes using matrices
# It saves the plots to pdf in a folder. It will save four graphs/plots to PDF in total. 
# Each will have a descriptive separate title. 

graphPlots <- function(data, iso1, iso2, threshold_iso1, threshold_iso2, path){
  
  # set frame size, this is used as an argument in the colored concentration graph
  frame_size.um <- 35
  data$value[is.na(data$value)] <- 0
  fill_max <- quantile(data$value, 0.999)
  
  # create first plot, colored coral graphs
  plot1 <- data %>%
    ggplot(aes(x.um, y.um)) + 
    geom_raster(aes(fill = value)) +
    facet_grid(~variable) + 
    coord_equal() +   
    # the lines up to this point are the essential part. Everything else is visual aesthetics!
    theme_bw() + 
    # Next three lines make image go to the edge of plot (eliminate white space)
    scale_y_continuous(expression("y [" * mu * "m]"), expand = c(0, 0)) +  
    scale_x_continuous(expression("x [" * mu * "m]"), expand = c(0, 0)) + 
    expand_limits(x = c(0, frame_size.um), y = c(0, frame_size.um)) +   #changing value of x limits to match micron region   
    scale_fill_viridis(option = "turbo", limits = c(0, fill_max), oob = scales::squish) + # color scale set to turbo
    theme(panel.background = element_rect(fill = "black"), # Background color (if images not all same size)               
          panel.spacing = unit(0, "mm"),   # space between graphs
          strip.text = element_text(size = 20), # size of panel label text
          strip.background = element_blank(),  # color of panel label background
          legend.position = "bottom") +       # put legend on bottom
    guides(color = guide_legend(override.aes = list(size = 8, shape = 15))) +  
    labs(x = expression("x [" * mu * "m]"), y = expression("y [" * mu * "m]"), color = "ROI") # add labels
  
  # Save Colored Concentration Plot
  file_path = str_c(path, "/", iso1, "-", iso2, "_Colored_Coral_Graphs.pdf")
  pdf(file = file_path) # start saving output
  print(plot1)
  dev.off() # stop saving output
  print(plot1)
  
  # create a cimg with normalized values for iso1
  cimg_iso1 <- data %>%
    filter(variable == iso1) %>%
    select(x.px, y.px, value) %>%
    rename(x = x.px,
           y = y.px) %>%
    as.cimg(dim=c(512,512,1,1))
  
  #create a cimg with normalized values for iso2
  cimg_iso2 <- data %>%
    filter(variable == iso2) %>%
    select(x.px, y.px, value) %>%
    rename(x = x.px,
           y = y.px) %>%
    as.cimg(dim=c(512,512,1,1))
  
  # ISO1 cimg plots
  file_path = str_c(path, "/", iso1, "-", iso2, "_Thresholded_", iso1, ".pdf")
  pdf(file_path) # start saving output
  par(mfrow = c(1,2))
  thresholded_iso1 <- cimg_iso1 > threshold_iso1 # cut off from threshold method
  plot(thresholded_iso1, main = str_c("thresholded ", iso1, " pre-fill"))
  thresholded_iso1 %>% fill(2) %>% plot(main = str_c("thresholded ", iso1, " fill=2")) # fill
  dev.off() # stop saving output
  
  plot2 <- thresholded_iso1 %>% fill(2) %>% plot(main = "thresholded iso1 fill=2") # fill
  
  # ISO2 cimg plots
  file_path = str_c(path, "/", iso1, "-", iso2, "_Thresholded_", iso2, ".pdf")
  pdf(file_path) # start saving output
  par(mfrow = c(1,2))
  thresholded_iso2 <- cimg_iso2 > threshold_iso2 # cut off from threshold method
  plot(thresholded_iso2, main = str_c("thresholded ", iso2, " pre-fill"))
  thresholded_iso2 %>% fill(2) %>% plot(main = str_c("thresholded ", iso2, " fill=2")) # fill
  dev.off() # stop saving output
  
  plot3 <- thresholded_iso2 %>% fill(2) %>% plot(main = "thresholded iso2 fill=2") # fill
  
  # create a cimg with normalized values for iso1
  cimg_iso1 <- data %>%
    filter(variable == iso1) %>%
    select(x.px, y.px, value) %>%
    rename(x = x.px,
           y = y.px) %>%
    as.cimg(dim=c(512,512,1,1))
  
  # create a cimg with normalized values for iso2
  cimg_iso2 <- data %>%
    filter(variable == iso2) %>%
    select(x.px, y.px, value) %>%
    rename(x = x.px,
           y = y.px) %>%
    as.cimg(dim=c(512,512,1,1))
  
  # iso1 with only above threshold pixels and fill of 2 (no clean)
  final_iso1 <- (cimg_iso1 > threshold_iso1) %>% fill(2) 
  
  # iso2 with only above threshold pixels and fill of 2 (no clean)
  final_iso2 <- (cimg_iso2 > threshold_iso2) %>% fill(2) 
  
  # Make multichannel image (yellow is the overlap of the two isotopes)
  file_path = str_c(path, "/", iso1, "-", iso2, "_Multichannel_Image.pdf")
  pdf(file_path) # start saving output
  par(mfrow = c(1,1))
  final_color <- add.color(plot2)
  channel(final_color,2) <- (plot3)
  channel(final_color,3) <- 0
  final_color %>% plot(main = "multichannel image")
  dev.off() # stop saving output
}
