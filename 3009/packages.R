# The function p_load() from {pacman} checks to see if a package is installed, 
# if not it attempts to install the package and then loads it. 
# It can also be applied to several packages at once, 
# all this in a very condensed way:
  
install.packages("pacman")
pacman::p_load(tiff, raster, terra, jpeg, sm, RImageJROI, spatstat, stringr)
