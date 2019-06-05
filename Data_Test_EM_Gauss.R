# Statistical Projects

# Download Data
# Function that compares BIC/AIC with different number of components and returns best results

setwd("C:/Users/Michaela")

install.packages("rvest")
install.packages("stringr")


# DATA
  # EUCASTData Package
 #   install.packages("Dropbox/Project SS2019/Implementierung/Daten/EUCASTData_2019.04.15.tar.gz", repos = NULL, type = "source")
    library(EUCASTData)
  
  # Download Zone Data
    scrapeBacteria(mic = FALSE, dir = "Dropbox/Project SS2019/Implementierung/Daten")
    
  # Laod Data
    ZD <- read.csv("C:/Users/Michaela/Dropbox/Project SS2019/Implementierung/Daten/ZD.csv", sep=";")

 
  
  

head(ZD)



head(ZD)

library(antibioticR)


