getwd()
setwd("C:\\Users\\yogur\\Desktop\\Homework\\Evolution2\\BirdData")

library(stats)

BirdData_All <- read.csv("All_Orders_Data_2.csv")
BirdData_Anseriformes <- read.csv("Anseriformes_Data.csv")
BirdData_Columbiformes <- read.csv("Columbiformes_Data.csv")
BirdData_Coraciiformes <- read.csv("Coraciiformes_Data.csv")
BirdData_Galliformes <- read.csv("Galliformes_Data.csv")
BirdData_Passeriformes <- read.csv("Passeriformes_Data.csv")
BirdData_Psittaciformes <- read.csv("Psittaciformes_Data.csv")
BirdData_Strigiformes <- read.csv("Strigiformes_Data.csv")

WorkingFile <- BirdData_All

Color <- as.vector(WorkingFile[,4])
Fecundity <- as.vector(WorkingFile[,7])


plot(Color, Fecundity, pch=16, cex=0.2, col= "blue",
   #main = print(WorkingFile[1,1]),
   main = "All Orders", xlab = "Dichromatism", 
   ylab = "Number of Eggs Per Clutch", xlim = c(-4, 4), ylim = c(0,20))
abline(lm(WorkingFile[,4] ~ WorkingFile[,7]))

