library(prettymapr)

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
            osm.text(data$Longitud, data$Latitud  , 
                     data$Estacion, cex = 1, col = "red", pos = 1);
            osm.points(data2$Longitud, data2$Latitud, pch = 3, cex = 1.5, col = "blue");
            osm.text(data2$Longitud, data2$Latitud  , 
                     data2$Estacion, cex = 1, col = "blue", pos = 3);
        }))
    }
}


inventario <- load_data("geografico.csv", user = "msvelandiag")
est_neusa <- load_data("rio_neusa.csv", user = "msvelandiag")
est_frio <- load_data("rio_frio.csv", user = "msvelandiag")

#png("calera.png")
mk_plot(inventario, muni = "La Calera", 
        bbox = makebbox(w = -73.9787, s =4.7131, e = -73.9617, n = 4.7252)) 
#dev.off()

#png("cajica.png")
#mk_plot(inventario, est_frio, muni = "Cajica" , expand = TRUE, 
#        bbox = makebbox(w = -74.0774, s =4.8836, e = -73.9902, n = 4.9315) ) 
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




