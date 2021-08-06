## Packages required

library(rgbif)
library(prettymapr)
library(rosm)
library(knitr)

## Plotting maps with the stations 


plot_est <-  function(data) { ## Function to be passed to the document
    
    est_frio <- subset(data, Rio == "Frio")
    est_neusa <- subset(data, Rio == "Neusa")
    
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
    
    par(fig=c(0,0.5,0,1))
    mk_plot(est_frio, expand = FALSE, resolution = 150,
            bbox = makebbox(w = -74.0863, s =4.8484, e = -74.0396, n = 4.9354)) 
    par(fig=c(0.5,1,0,1), new=TRUE)
    mk_plot(est_neusa, expand = FALSE, resolution = 150,
            bbox = makebbox(w = -74.0060, s =5.0728, e = -73.9291, n = 5.1670)) 
                
  
}


parse_table <- function(doc.type = NULL, latex = NULL, docx = NULL, html = NULL) {
  
  if (is.null(doc.type)) {
      doc.type <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  }
  
  if (doc.type == "latex") {
      if (!is.null(latex)) { 
          return(eval(expr = latex))
    }  
  } 
  
  if (doc.type == "docx") {
      if (!is.null(docx)) {  
          return(eval(expr = docx))        
    }  
  } 
  
  if (doc.type == "html") {
      if (!is.null(html)) {
          return(eval(expr = html))
    }
  }
  
  else {
      return(invisible())
  }
  
}

