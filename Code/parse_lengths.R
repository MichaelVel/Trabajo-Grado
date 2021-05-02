library(tidyverse)

### 

join_logs <- function(){
    
    ## Join the logs returned by ImageJ
    rio_neusa <- read.csv("raw_data/logs_neusa.csv")
    rio_frio <- read.csv("raw_data/logs_frio.csv")
    join_logs <- rbind(rio_frio, rio_neusa)
    return(join_logs)
    
}

total_length <-  function(data) {
    
    ## Some lots have more than one log, this function
    ## sums the lengths
    data <- group_by(data, Lote)
    data <- summarise(data, length = sum(longitude) )
    return(data)
}

clasified_length <-  function(data) {
    
    ## Return a datasets with categories of length
    len = length(data$Lote)
    tamaño = rep(NA, len)
    data = cbind(data, tamaño)
    
    for (i in 1:len) {
        
        if (data$length[i] < 2.5){
        data$tamaño[i] <- "<2.5"
        }
        else if (data$length[i] >= 2.5 & data$length[i] < 5 ){
            data$tamaño[i] <- "2.5-5"
        }
        else if (data$length[i] >= 5 & data$length[i] < 10 ){
            data$tamaño[i] <- "5-10"
        }
        else if (data$length[i] >= 10 & data$length[i] < 20 ){
            data$tamaño[i] <- "10-20"
        }
        else if (data$length[i] >= 20 & data$length[i] < 40 ){
            data$tamaño[i] <- "20-40"
        }
        else if (data$length[i] >= 40 & data$length[i] < 80 ){
            data$tamaño[i] <- "40-80"
        }
        else if (data$length[i] >= 80 ){
            data$tamaño[i] <- ">80"
        }
    } 
    
    return(data)
        
} 

joined_to_inventory <- function(data) {
    
    ## Join the new variables to the dataset
    inventory <- read.csv("transform_data/inventario.csv")
    data <- left_join(inventory, data, by = 'Lote')
    return(data)
    
}

functional_length <- function() {
    
    join_logs() %>%
        total_length() %>%
        clasified_length() %>%
        joined_to_inventory() %>%
        write.csv(file = "transform_data/inventario_lenghts.csv")
       
} 

functional_length()
