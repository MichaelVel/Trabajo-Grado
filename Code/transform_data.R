## Packages required
library(dplyr)
library(tidyr)
library (taxize)
library(measurements)
library(stringr)
library(rgbif)

### Taxonomy 

# To all datasets

validate_families<- function(data, var = 'Familia'){
     
     ## Validate with 'Open Tree of Life' the families names  
     families = levels(factor(data[[var]]))
     x = tol_resolve(names = families)[,1:2]
     len = length(data[[var]])
     
     for (i in 1:len){
         index = which(tolower(data[[var]][[i]]) == x[[1]])
         data[[var]][[i]] <- x[[2]][[index]]  
     }
    
     return(data)
}

change_families <- function(data, old_family, new_family) {
    
    ## If necesary, allows to change a family by other
    x <- data$Familia == old_family
    data$Familia[x] <- new_family    
    return(data)
}

#To rio_frio dataset

pivot_families<- function(data, cols = NULL){
    
    ## Takes a matrix with families and individuals, and return the dataset
    ## with the tidy data.
    x = pivot_longer(data, cols, names_to = "Familia", values_to = "Individuos")
    x = filter(x, Individuos != 0)
    return(x)
}


genre_from_family <- function(data){
     
    ## If necessary, allows to split a variable with Family and Genre 
    ## into two variables
    x = separate(data, col = 'Familia', into = c('Familia', 'Genero') )
    return(x)
}

### Location 

conv_coord <- function( data , med = NULL, ...){
    
    ## Change the format of the coordinates
    if (med == 'lat') {
        if (any(str_detect(data$Latitud, ","))){
            data$Latitud <- gsub(",", ".", data$Latitud)}
        data$Latitud <- str_remove(data$Latitud, " N")
        data$Latitud <- trimws(data$Latitud)
        data$Latitud <- gsub("°", " ", data$Latitud) 
        data$Latitud <- gsub("'", " ", data$Latitud) 
        data$Latitud <- gsub("\"", "", data$Latitud)
        data$Latitud <- conv_unit(data$Latitud, from = 'deg_min_sec', to = 'dec_deg')
        data$Latitud <- as.numeric(data$Latitud)
    }
    
    else if (med == 'lon') {
        if (any(str_detect(data$Longitud, ","))){
            data$Longitud <- gsub(",", ".", data$Longitud)}
        data$Longitud <- str_remove(data$Longitud, " W")
        data$Longitud <- trimws(data$Longitud)
        data$Longitud <- paste0("-", data$Longitud) 
        data$Longitud <- gsub("°", " ", data$Longitud) 
        data$Longitud <- gsub("'", " ", data$Longitud) 
        data$Longitud <- gsub("\"", "", data$Longitud)
        data$Longitud <- conv_unit(data$Longitud, from = 'deg_min_sec', to = 'dec_deg')
        data$Longitud <- as.numeric(data$Longitud)
    }
    return(data)
}

conv_alt <- function(data, user, ...) {
    
    ## Obtain and replace the altitude using the coordinates 
    elevation <- elevation(latitude = data$Latitud, longitude = data$Longitud,
                           username = user)
    data$Altitud <- elevation$elevation_geonames
    return(data)
}


### Load and export data

load_data <- function(data, username, ...) {
    data <- read.csv(data, stringsAsFactors = FALSE)
    data <- conv_coord(data, med = 'lat')
    data <- conv_coord(data, med = 'lon')
    data <- conv_alt(data, user= username) 
    return(data)
}

export_csv<- function(data , filename){
    route = paste0("transform_data/", filename)
    write.csv(data, file = route)
}

### Running Script

load_data("raw_data/geografico.csv", "msvelandiag") %>% 
    change_families(old_family = 'Unionidae', new_family = 'Sphaeriidae' ) %>%
    change_families(old_family = 'Ancylidae', new_family = 'Sphaeriidae' ) %>%
    validate_families() %>%
    export_csv(filename = 'inventario.csv' )

load_data("raw_data/rio_frio.csv", "msvelandiag") %>%
   pivot_families(cols = 13:40) %>%
   genre_from_family() %>%
   change_families(old_family = 'Unionidae', new_family = 'Sphaeriidae' ) %>%
    change_families(old_family = 'Ancylidae', new_family = 'Sphaeriidae' ) %>%
    validate_families() %>%
    export_csv(filename = 'rio_frio.csv' )

load_data("raw_data/rio_neusa.csv", "msvelandiag")%>% 
    change_families(old_family = 'Unionidae', new_family = 'Sphaeriidae' ) %>%
    change_families(old_family = 'Ancylidae', new_family = 'Sphaeriidae' ) %>%
    validate_families() %>%
    export_csv(filename = 'rio_neusa.csv' )

# Testing     
inventario <- load_data("raw_data/geografico.csv", "msvelandiag") %>% 
    change_families(old_family = 'Unionidae', new_family = 'Sphaeriidae' ) %>%
    change_families(old_family = 'Ancylidae', new_family = 'Sphaeriidae' ) %>%
    validate_families() 

rio_frio <- load_data("raw_data/rio_frio.csv", "msvelandiag") %>%
    pivot_families(cols = 13:40) %>%
    genre_from_family() %>%
    change_families(old_family = 'Unionidae', new_family = 'Sphaeriidae' ) %>%
    change_families(old_family = 'Ancylidae', new_family = 'Sphaeriidae' ) %>%
    validate_families() 

rio_neusa <- load_data("raw_data/rio_neusa.csv", "msvelandiag")%>% 
    change_families(old_family = 'Unionidae', new_family = 'Sphaeriidae' ) %>%
    change_families(old_family = 'Ancylidae', new_family = 'Sphaeriidae' ) %>%
    validate_families() 

x <- levels(factor(inventario$Familia))
b <- levels(factor(rio_frio$Familia))
c <- levels(factor(rio_neusa$Familia))
b[! b %in% x]
c[! c %in% x]
