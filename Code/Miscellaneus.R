## Miscellaneus script used to create the 'abundancia_orden.csv' file 
## Not intended to be run with the main program. 

library(tidyverse)
library(taxize)

complete <- function(file1, file2) {
    
    ## Takes as input the routes of the raw data and joiun the two data frames, 
    ## selecting the estations and samples to use
    
    join_data <- function(data1, data2){
        
        data_1 <- read.csv(data1, stringsAsFactors = FALSE)
        data_2 <- read.csv(data2, stringsAsFactors = FALSE) 
        data_2 <- data_2[names(data_2) %in% names(data_1)]
        data_joined <- bind_rows(data_1, data_2)
        return(data_joined)
    }
    
    data <- join_data(file1,file2) %>% subset(Estacion != 'Neu5') 
    data$tmp <- paste0(data$Estacion, '_', data$Muestra) ## temporal variable
    
    data <- data %>%     ## All stations with 6 samples
        subset(tmp != 'Neu1_11' & tmp != 'Neu1_4') %>%
        subset(tmp != 'Neu2_11' & tmp != 'Neu2_8' &
               tmp != 'Neu2_4') %>%
        subset(tmp != 'Neu3_11' & tmp != 'Neu3_10') %>%
        subset(tmp != 'Neu6_11' & tmp != 'Neu6_10' & 
               tmp != 'Neu2_8' & tmp != 'Neu2_1') %>%
        subset(tmp != 'Neu7_11' & tmp != 'Neu7_10' & 
               tmp != 'Neu7_1') %>%
        subset(select = -tmp)
    
        return(data)
        
}

make_RTable <- function(data) {
    
    RTable <- data %>%
                select(-c(Epoca, Fecha, Latitud, Longitud,
                          Altitud, Familia, Individuos)) %>%
                distinct()  %>%
                group_by( Estacion, Muestra) %>%
                summarise(across(.cols = everything() , mean)) 
     
    return(RTable)
}

make_abundance_by_order <- function(data, exceptions = NULL ) {

    data <- read.csv(data) %>% 
        unite(col = names, Estacion, Muestra) %>%
        column_to_rownames('names')  %>%
        as.matrix() %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column( var = "Familia")
    
    if (is.null(exceptions)) {
        
        exceptions <- data.frame(
            Familia = c("Glossiphoniidae", "Physidae", 
                        "Planorbidae", "Lymnaeidae", 
                        "Sphaeriidae", "Naididae"),
            Orden = c("Hirudinea", rep("Gastropoda", 3),
                      "Bivalvia", "Oligochaeta"),
            stringsAsFactors = FALSE )
    }
    
    family_to_order <- function(data, exceptions = NULL ) {
        
        order_names <- tax_name(sci = data$Familia, get = "order",
                                db = "ncbi", messages = FALSE)
        
        if (!is.null(exceptions)) {
            
            for (i in 1:nrow(exceptions)){
                
                index <- grep(pattern = exceptions$Familia[i], order_names$query)
                order_names$order[index] <- exceptions$Orden[[i]]
            }
        }
            
        data[["Familia"]] <- order_names$order
        names(data)[names(data) == 'Familia'] <- 'Orden'
        
        
        data %>%
            group_by(Orden) %>%
            summarise(across(.cols = everything(), sum)) %>%
            column_to_rownames("Orden") %>%
            as.matrix() %>%
            t() %>%
            as.data.frame() %>%
            rownames_to_column(var = "names") %>%
            separate(names, c("Estacion", "Muestra")) %>%
            return()
        
    }
    
    data <- family_to_order(data, exceptions)
    
    return(data)
    
}


main <- function() {
    
    file1 <- "Data/raw_data/rio_frio.csv"
    file2 <- "Data/raw_data/rio_neusa.csv"
    abundance <- "Data/rlq/LTable.csv"
    
    complete <- complete(file1, file2)
    RTable <- make_RTable(complete)
    abundance_order <- make_abundance_by_order(abundance)
    
    write.csv(complete, file = "Data/muestreo_completo.csv", row.names = FALSE )
    write.csv(RTable, file = "Data/rlq/RTable.csv", row.names = FALSE )
    write.csv(abundance_order, file = "Data/other/abundancia_orden.csv", 
              row.names = FALSE )
    
}
## Requiere and API Key from:
## https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/

## See the taxize-authentication documentation to load the API Key to R.

main()
