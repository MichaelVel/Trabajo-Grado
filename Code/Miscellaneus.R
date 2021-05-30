## Miscellaneus script used to create the 'abundancia_orden.csv' file 
## Not intended to be run with the main program. 

library(taxize)

main <- function() {

        data <- read.csv("Data/abundancia.csv") %>% 
        select(-c(Tipo_Estacion, Muestra)) %>%
        group_by(Estacion) %>%
        summarise(across(everything(), sum)) %>%
        column_to_rownames("Estacion") %>%
        as.matrix() %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column( var = "Familia")
    
    
    exceptions <- data.frame(
        Familia = c("Glossiphoniidae", "Physidae", 
                    "Planorbidae", "Lymnaeidae", 
                    "Sphaeriidae", "Naididae"),
        Orden = c("Hirudinea", rep("Gastropoda", 3),
                  "Bivalvia", "Oligochaeta"),
        stringsAsFactors = FALSE )
    
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
            return()
        
    }
    
    data <- family_to_order(data, exceptions)
    write.csv(data, file = "Data/abundancia_orden.csv")
}

## Requiere and API Key from:
## https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/

## See the taxize-authentication documentation to load the API Key to R.

main()
