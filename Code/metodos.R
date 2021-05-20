## Packages required
library(measurements)
library(stringr)
library(rgbif)
library(prettymapr)
library(rosm)
library(knitr)
library(kableExtra)

## Preprocessing functions 


load_data_met <- function(data, username, ...) {
    
    conv_coord <- function(data , med = NULL, ...){ ## Degrees Minutes Seconds to Decimal 
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
    
    conv_alt <- function(data, user, ...) { ## Obtain elevation from coordinates 
        elevation <- elevation(latitude = data$Latitud, longitude = data$Longitud,
                               username = user)
        data$Altitud <- elevation$elevation_geonames
        return(data)
    }
    
    data <- read.csv(data)
    data <- conv_coord(data, med = 'lat')
    data <- conv_coord(data, med = 'lon')
    data <- conv_alt(data, user= username) 
    return(data)
}

estaciones_coord <- function(data, name = NULL){  ## Miscelaneous function
    estacion <- subset(data, Estacion == name )
    estacion$Latitud <- factor(as.character(estacion$Latitud))
    estacion$Longitud <- factor(as.character(estacion$Longitud))
    return(estacion)
}

levels_estaciones <- function(...){ ## Miscelaneous function
    est <- estaciones_coord(...)
    latitud <- levels(est$Latitud)
    longitud <- levels(est$Longitud)
    print(c(latitud, longitud))
}

## Plotting maps with the stations 

mk_plot <- function(data, data2, bbox = NULL, expand = FALSE, 
                    resolution = 200, ...){ ##return map with OpenStreetMap 

    if (expand == FALSE) { 
        return(prettymap({
        osm.plot(bbox, res = resolution, stoponlargerequest=FALSE);
        osm.points(data$Longitud, data$Latitud, pch = 18, cex = 1.5, 
                   col = "red");
        osm.text(data$Longitud, data$Latitud  , 
                 data$Estacion, cex = 1, col = "black", pos = 1);
        }, drawbox = TRUE, drawarrow = TRUE))
    }
    else {
        return(prettymap({
        osm.plot(bbox, res = resolution, stoponlargerequest=FALSE);
        osm.points(data$Longitud, data$Latitud, pch = 18, cex = 1.5, col = "red");
        osm.text(data$Longitud, data$Latitud, 
                 data$Estacion, cex = 1, col = "red", pos = 1);
        osm.points(data2$Longitud, data2$Latitud, pch = 18, cex = 1.5, col = "blue");
        osm.text(data2$Longitud, data2$Latitud  , 
                 data2$Estacion, cex = 1, col = "blue", pos = 3);
        }, drawbox = TRUE, drawarrow = TRUE))
    }
}

plot_est <-  function(device = 'rmarkdown' ) { ## Function to be passed to the document
    
    data <-  read.csv("Data/estaciones.csv")
    est_frio <- subset(data, Rio == "Frio")
    est_neusa <- subset(data, Rio == "Neusa")
    
    if (device == 'png'){ 
        
        png(filename = 'Results/rio_frio.png')
        mk_plot(est_frio, expand = FALSE, resolution = 150,
                bbox = makebbox(w = -74.1531, s =4.8389, e = -73.9787, n = 4.9346)) 
        dev.off()
        
        png(filename = 'Results/rio_neusa.png')
        mk_plot(est_neusa, expand = FALSE, resolution = 150,
                bbox = makebbox(w = -74.0060, s =5.0728, e = -73.9291, n = 5.1670)) 
        dev.off()
    } else if (device == 'rmarkdown') {
        par(fig=c(0,0.5,0,1))
        mk_plot(est_frio, expand = FALSE, resolution = 150,
                bbox = makebbox(w = -74.0863, s =4.8484, e = -74.0396, n = 4.9354)) 
        par(fig=c(0.5,1,0,1), new=TRUE)
        mk_plot(est_neusa, expand = FALSE, resolution = 150,
                bbox = makebbox(w = -74.0060, s =5.0728, e = -73.9291, n = 5.1670)) 
                
    } 
}




