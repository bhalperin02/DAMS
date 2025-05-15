# This function recalls and loads all the needed packages to run OutputComplete
# Updated 11.14.24 BH based on code from CH

# Inputs: None
# Ouputs: Recalls required packages and loads all packages

IHAPackages <- function() {
  
  # FORM FOR INSTALLING PACKAGES
  # if(!require(package)){install.packages('package')} 
  # library(package)
  
  if(!require(tidyverse)){install.packages('tidyverse')} 
  library(tidyverse)

  if(!require(lubridate)){install.packages('lubridate')} 
  library(lubridate)
  
  if(!require(IHA)){install.packages('IHA')} 
  library(IHA)
  
  if(!require(dataRetrieval)){install.packages('dataRetrieval')} 
  library(dataRetrieval)
  
  if(!require(zoo)){install.packages('zoo')} 
  library(zoo)
  
  if(!require(plyr)){install.packages('plyr')} 
  library(plyr)
  
  if(!require(ggplot2)){install.packages('ggplot2')} 
  library(ggplot2)
  
  if(!require(tidyr)){install.packages('tidyr')} 
  library(tidyr)
  
  if(!require(dplyr)){install.packages('dplyr')} 
  library(dplyr)
  
  if(!require(stats)){install.packages('stats')} 
  library(stats)
  
  if(!require(exactRankTests)){install.packages('exactRankTests')} 
  library(exactRankTests)
  
  if(!require(eseis)){install.packages('eseis')} 
  library(eseis)
  
  if(!require(plot.matrix)){install.packages('plot.matrix')} 
  library(plot.matrix)
  
  if(!require(writexl)){install.packages('writexl')} 
  library(writexl)
  
}