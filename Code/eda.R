### información geografica de la CAR
 
 ## Subcuencas definidas en la CAR
link1 <- "https://datosgeograficos.car.gov.co/datasets/delimitaci%C3%B3n-de-subcuencas-a-escala-125-000-en-la-jurisdicci%C3%B3n-car?geometry=-83.133%2C3.350%2C-64.808%2C7.178"

 ## Red hidrologica / estaciones de muestreo 
link2 <- "https://datosgeograficos.car.gov.co/datasets/red-hidrometeorol%C3%B3gica-car/data?geometry=-74.135%2C4.847%2C-73.992%2C4.877&selectedAttribute=CATEGORIA_NOMBRE"

 ## Areas protegidas
link3 <- "https://datosgeograficos.car.gov.co/datasets/%C3%A1reas-protegidas-declaradas-car/data?geometry=-74.683%2C4.973%2C-73.538%2C5.212"

## Delimitación rondas de rios
link4 <- "https://datosgeograficos.car.gov.co/datasets/delimitaci%C3%B3n-de-rondas/data?geometry=-74.214%2C4.900%2C-73.927%2C4.960"

## Load packages 

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

## Load data

file2 <- "../Data/rio_frio.csv"
file1 <- "../Data/rio_neusa.csv"

load_data <- function(data1, data2){
    
    rio_neusa <- read.csv(data1, stringsAsFactors = FALSE)
    rio_frio <-  read.csv(data2, stringsAsFactors = FALSE) ## Join both tables
    return(list(rio_frio, rio_neusa))
    }
data <- load_data(file1, file2)
rio_frio <- data[[1]]
rio_neusa <- data[[2]]
### eda

## Summary statistics: 
 # boxplot by estation and by sample // histograms of the data 
 # table: sumary statistics (mean, median, quantiles, sd, skewness, kurtosis)
 # count: # of individuals and # of families  
