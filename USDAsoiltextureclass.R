################################################################################
##########################  Soil Texture Calculator  ###########################
################################################################################
## 02/05/2023               ####################################################
## GouweGozer               ####################################################
################################################################################

# This function determines USDA soil texture class based on the Texture
# Definitions of the USDA Soil Texture Calculator

# The input for the function are 3 numeric values
# clay: % clay of the soil
# silt: % silt of the soil
# sand: % sand of the soil

# The output of the function is a character value with one of the 12
# USDA soil texture classes

# This code does not require any functions outside base R

SoilTextureClassUSDA <- Vectorize(function(clay, silt, sand) {
  if(sum(clay, silt, sand) != 100) {
    stop("The sum of clay, silt, and sand should amount to 100 percent. 
    
If you are using tibble columns as input you can identify the erroneous row(s) like this:
        
         'which(rowSums(select(df, clay, silt, sand)) != 100)'

where df is the name of your tibble and the other entries are column names")
  }else if (sand <= (90 + (-1/3 * silt)) && sand >= (85 + (-1/6 * silt))) {
    soil_texture <- "Loamy sand"
  } else if (clay <= 10 && silt >= 80) {
    soil_texture <- "Silt"
  } else if (clay >= 40 && silt < 40 && sand <= 45) {
    soil_texture <- "Clay"
  } else if (sand >= 85 && (silt + 2 * clay) < 30) {
    soil_texture <- "Sand"
  } else if (silt >= 80 && clay < 12) {
    soil_texture <- "Silt"
  } else if (clay >= 40 && silt >= 40) {
    soil_texture <- "Silty clay"
  } else if (clay >= 27 && clay <= 40 && sand <= 20) {
    soil_texture <- "Silty clay loam"
  } else if ((sand <= (85 + (-1/6 * silt)) && clay <= 20 && sand >= 52) ||
             (clay <= 8 && silt >= 40 && silt <= 50)) {
    soil_texture <- "Sandy loam"
  } else if (clay >= 10 && clay < 27 && sand <= ((-10/28) * silt + (10 + 800/28))) {
    soil_texture <- "Silty clay loam"
  } else if (clay <= 27 && silt >= 50 && silt <= 80 &&
             sand >= ((-10/28) * silt + (10 + 800/28))) {
    soil_texture <- "Silt loam"
  } else if (clay >= 27 && clay <= 39 && sand >= 20 && sand < 46) {
    soil_texture <- "Clay loam"
  } else if (clay >= 20 && clay < 35 && sand >= 45 && silt < 28) {
    soil_texture <- "Sandy clay loam"
  } else if (clay >= 35 && sand >= 45) {
    soil_texture <- "Sandy clay"
  } else if (clay >= 7 && clay <= 27 && silt >= 28 && silt <= 50 && sand <= 52) {
    soil_texture <- "Loam"
  } else {
    stop(sprintf("Unknown texture: clay = %.1f, silt = %.1f, sand = %.1f", clay, silt, sand))
  }

  return(soil_texture)
})
# The USDA soil calculator Excel file containing the Texture Definitions can be 
# downloaded from the USDA Natural Resources Conservation Service website:
# https://www.nrcs.usda.gov/sites/default/files/2022-11/MultiPointTriangle_v1.xlsm



