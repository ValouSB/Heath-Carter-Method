# Heath-Carter Somatograph
# Create a somatograph with subjects using the Heath-Carter method

## You can run the following two lines without "#" to install the PNG and Grid packages if you need
# install.packages("png")
# install.packages("grid")


library(png) # background img (somatograph-blank) (1)
library(grid) # background img (somatograph-blank) (2)

HCSoma <- function(data) {
  # Somatogrpah
  # 
  # Args:
  #   data: a numeric data-frame or matrix with 10 variables :
  #		  weight : weight of the subject in kg
  #       l_stature : stature in cm
  #		  c_arm : maximal circonference on the right arm 
  #		  c_calf : maximal circonference on the right calf
  #       w_femur : width of the distal epiphyse of the femur
  #		  w_humerus : width of the distal epiphyse of the humerus
  #       sf_subsca : subscapular skinfold
  #       sf_supraspi : supraspinal skinfold
  #       sf_triceps : brachial triceps skinfold
  #
  # Returns: 
  #   somatotype from Args
  HCS_endo_X <- (data$sf_triceps + data$sf_subsca + data$sf_supraspi) * (170.18 / data$l_stature)
  HCS_endo   <- (-0.7182) + (0.1451 * (HCS_endo_X)) - (0.00068 * ((HCS_endo_X)))^2 + (0.0000014 * ((HCS_endo_X)))^3
  HCS_meso   <- (0.858 * data$w_humerus)  + (0.601 * data$w_femur) + (0.188 * data$c_arm) + (0.161 * data$c_calf) - (0.131 * data$l_stature)  + 4.5
  HCS_ecto   <- c() # define an empty vector for ectomorphy
  for (i in 1:nrow(data)) {
    HWR <- ((data$l_stature[i]) / (data$weight[i]^(1/3))) # defining the height/weight ratio
    if (HWR >= 40.75) {
      HCS_ecto <- c(HCS_ecto, ((0.732 * HWR) - 28.58)) 
      }
    else if (HWR > 38.25) {
      HCS_ecto <- c(HCS_ecto, ((0.463 * HWR) - 17.63))
    }
    else {
      HCS_ecto <- c(HCS_ecto, 0.1)
    }
  }
  table_HCS <- as.data.frame(cbind(HCS_endo, HCS_meso, HCS_ecto)) #combine endo, meso and ecto
  x <- (table_HCS[,3] - table_HCS[,1])
  y <- ((2*table_HCS[,2]) - (table_HCS[,1] + table_HCS[,3]))
  return(cbind(x,y))
}


data <- read.csv2("PATH TO YOUR DATA")
df <- as.data.frame(HCS(data))
img <- readPNG("PATH TO SOMATOGRAPH BLANK") 
ggplot(df, aes(x, y)) +    
        xlim(-inf, inf) +
        ylim(-inf, inf) +
        theme(panel.background = element_rect(fill = 'white', colour = 'white'))