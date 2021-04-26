## Packages required
library(measurements)
library(stringr)
library(rgbif)
library(prettymapr)
library(rosm)

conv_coord <- function( data , med = NULL, ...){
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
    elevation <- elevation(latitude = data$Latitud, longitude = data$Longitud,
                           username = user)
    data$Altitud <- elevation$elevation_geonames
    return(data)
}

load_data <- function(data, username, ...) {
    data <- read.csv(data)
    data <- conv_coord(data, med = 'lat')
    data <- conv_coord(data, med = 'lon')
    data <- conv_alt(data, user= username) 
    return(data)
}


mk_plot <- function(data, data2, muni = NULL, muni2 = NA,  bbox = NULL, expand = FALSE, 
                    resolution = 200, ...){

    data <- subset(data, Municipio == muni | Municipio == muni2)
    
    if (expand == FALSE) { 
        return(prettymap({
        osm.plot(bbox, res = resolution);
        osm.points(data$Longitud, data$Latitud, pch = 18, cex = 1.5, 
                   col = "red");
        osm.text(data$Longitud, data$Latitud  , 
                 data$Estacion, cex = 1, col = "black", pos = 1);
        }))
    }
    else {
        return(prettymap({
        osm.plot(bbox, res = resolution);
        osm.points(data$Longitud, data$Latitud, pch = 18, cex = 1.5, col = "red");
        osm.points(data2$Longitud, data2$Latitud, pch = 3, cex = 1.5, col = "blue");
        osm.text(data2$Longitud, data2$Latitud  , 
                 data2$Estacion, cex = 1, col = "blue", pos = 3);
        }))
    }
}


inventario <- load_data("raw_data/geografico.csv", user = "msvelandiag")
est_neusa <- load_data("rio_neusa.csv", user = "msvelandiag")
est_frio <- read.csv("transform_data/rio_frio.csv")

#png("calera.png")
mk_plot(inventario, muni = "La Calera", 
        bbox = makebbox(w = -73.9787, s =4.8523, e = -73.9617, n = 4.7252)) 
#dev.off()

#png("cajica.png")
mk_plot(inventario, est_frio, muni = "Cajica", muni2 = "Chia", expand = TRUE, resolution = 220,
        bbox = makebbox(w = -74.1531, s =4.8389, e = -73.9787, n = 4.9346) ) 
#dev.off()

#png("lenguezaque.png")
mk_plot(inventario, muni = "Lenguezaque",
        bbox = makebbox(w = -73.7241, s =5.2740, e = -73.7002, n = 5.3001)) 
#dev.off()
#
png("cogua.png", width= 1000, height = 1000)
mk_plot(data = inventario, data2 = est_neusa, 
       muni = "Cogua", expand = TRUE, resolution = 150,
       bbox = makebbox(w = -74.0060, s =5.0728, e = -73.9291, n = 5.1670)) 
dev.off()

