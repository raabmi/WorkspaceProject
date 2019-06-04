# Statistical Projects SS2019

setwd("C:/Users/Nicole")

#install.packages("rvest")
#install.packages("stringr")


# DATA
  # EUCASTData Package
      #install.packages("Dropbox/Project SS2019/Implementierung/Daten/EUCASTData_2019.04.15.tar.gz", repos = NULL, type = "source")
      #library(EUCASTData)
  
  # Download Zone Data
    #scrapeBacteria(mic = FALSE, dir = "Dropbox/Project SS2019/Implementierung/Daten")
    
  # Laod Data
    ZD <- read.csv("Dropbox/Project SS2019/Implementierung/Daten/ZD.csv", sep=";")

    
# Summary
head(ZD)
str(ZD)


# EXAMPLE DATASETS

  # EXAMPLE 1: Antimicrobial == "Ampicillin" & Bacterium == "Escherichia coli"
  ZD1 <- subset(ZD, Antimicrobial == "Ampicillin" & Bacterium == "Escherichia coli",
                grepl("^Z", colnames(ZD)))
  example1 <- data.frame(ZD1 = as.integer(gsub("^Z", "", colnames(ZDs))),
                        Freq = unname(unlist(ZDs)))
  plot(Freq ~ ZD1, data = example1, type = "h")

  # EXAMPLE 2: Antimicrobial == "Piperacillin" & Bacterium == "Escherichia coli"
  ZD2 <- subset(ZD, Antimicrobial == "Piperacillin" & Bacterium == "Escherichia coli",
                grepl("^Z", colnames(ZD)))
  example2 <- data.frame(ZD = as.integer(gsub("^Z", "", colnames(ZD2))),
                        Freq = unname(unlist(ZD2)))
  plot(Freq ~ ZD, data = example2, type = "h")
  
  # EXAMPLE 3: Antimicrobial == "Mecillinam" & Bacterium == "Escherichia coli"
  ZD3 <- subset(ZD, Antimicrobial == "Mecillinam" & Bacterium == "Escherichia coli",
                grepl("^Z", colnames(ZD)))
  example3 <- data.frame(ZD = as.integer(gsub("^Z", "", colnames(ZD3))),
                        Freq = unname(unlist(ZD3)))
  plot(Freq ~ ZD, data = example3, type = "h")


# TESTS

