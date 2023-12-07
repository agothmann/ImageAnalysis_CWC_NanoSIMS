# Calculates the M1, M2, MOC, Pearson, and Spearman coefficients using matrices of iso1 and
# iso2. Also returns the associated p-value for Pearson and Spearman correlation coefficients

calcCoef <- function(data, iso1, iso2, threshold_iso1, threshold_iso2){
  # create a cimg (class of object for storing image data) with normalized values for iso1
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
  
  # iso1 with only above threshold pixels and fill of 2 to smooth the image slightly - the fill can be removed if desired
  final_iso1 <- (cimg_iso1 > threshold_iso1) %>% fill(2) 
  
  # iso2 with only above threshold pixels and fill of 2 to smooth the image slightly
  final_iso2 <- (cimg_iso2 > threshold_iso2) %>% fill(2) 
  
  # Make multichannel image
  final_color <- add.color(final_iso1*cimg_iso1) # set color channels iso1 values
  channel(final_color,2) <- (final_iso2*cimg_iso2) # set 2nd color channel to iso2 values
  channel(final_color,3) <- 0 # set 3rd color channel to 0
  
  # Store images as matrices that will be used for calculating co-occurrence and correlation coefficients
  final_matrix_iso1 <- final_color[,,1,1] #final iso1 matrix!
  final_matrix_iso2 <- final_color[,,1,2] #final iso2 matrix!
  
  final_matrix_iso1[is.na(final_matrix_iso1)] <- 0
  final_matrix_iso2[is.na(final_matrix_iso2)] <- 0
  
  # Manders using matrix multiplication
  MOC = sum((final_matrix_iso2*final_matrix_iso1))/
    (sqrt(sum(final_matrix_iso2^2) * sum(final_matrix_iso1^2)))
  
  # M1 calculation
  M1 = sum(final_matrix_iso1[final_matrix_iso2>0])/sum(final_matrix_iso1)
  
  # M2 calculation
  M2 = sum(final_matrix_iso2[final_matrix_iso1>0])/sum(final_matrix_iso2)
  
  # Pearson and Spearman using vectors and built in cor function
  A = final_matrix_iso1[1:(512*512)] # making a vector out of iso1 matrix
  B = final_matrix_iso2[1:(512*512)] # making a vector out og iso2 matrix
  newA = A[(B>0)&(A>0)] # filtering to only values above threshold for both isotopes
  newB = B[(B>0)&(A>0)] # filtering to only values above threshold for both isotopes
  pearson = cor.test(x = newA,y =newB, method="pearson", conf.level = 0.95)
  spearman = cor.test(x = newA, y=newB, method="spearman",conf.level = 0.95)
  
  # return all 5 coefficients
  return(list(MOC=MOC, M1=M1, M2=M2, pearson=pearson, spearman=spearman))
}

