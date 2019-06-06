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
  example1 <- data.frame(ZD1 = as.integer(gsub("^Z", "", colnames(ZD1))),
                        Freq = unname(unlist(ZD1)))
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
  
  # EXAMPLE 4: Antimicrobial == "Amikacin" & Bacterium == "Acinetobacter spp"
  ZD4 <- subset(ZD, Antimicrobial == "Amikacin" & Bacterium == "Acinetobacter spp",
                grepl("^Z", colnames(ZD)))
  example4 <- data.frame(ZD = as.integer(gsub("^Z", "", colnames(ZD4))),
                         Freq = unname(unlist(ZD4)))
  plot(Freq ~ ZD, data = example4, type = "h")
  


# TESTS
e1Start <- createCluster(as.matrix(example4), 1)
e1Start
em.gauss(as.matrix(example4$Freq), 
         mu = c(22) , 
         sigma2 = c(16), 
         pi = c(1),          
         alpha = 1,
         beta = 3,
         epsilon = 0.0001) # PROBLEM: 1 component funkt nicht


e1Start <- createCluster(as.matrix(example1), 1)
e1Start
em.gauss(as.matrix(example1$Freq), 
         mu = c(15) , 
         sigma2 = c(38), 
         pi = c(1),          
         alpha = 1,
         beta = 3,
         epsilon = 0.0001) # PROBLEM: 1 component funkt nicht



e2Start <- createCluster(as.matrix(example2), 2)
e2Start
em.gauss(y = as.matrix(example2$Freq), 
         mu = as.vector(unlist(e2Start[1])) , 
         sigma2 = as.vector(unlist(e2Start[2])), 
         pi = c(1/2, 1/2),          
         alpha = 1,
         beta = 3,
         epsilon = 0.0001)

e2Start <- createCluster(as.matrix(example2), 3)
e2Start
em.gauss(y = as.matrix(example2$Freq), 
         mu = as.vector(unlist(e2Start[1])) , 
         sigma2 = as.vector(unlist(e2Start[2])), 
         pi = c(1/3, 1/3, 1/3),          
         alpha = 1,
         beta = 3,
         epsilon = 0.0001)






