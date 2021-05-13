## Load packages 

library(e1071)
library(ggplot2)
library(dplyr)
library(factoextra)
library(lubridate)

## Load and cleaning data

load_data <- function(ref_estations = FALSE) {
    
    ## Accesory function to load and filter the data
    
    file1 <- "Data/rio_frio.csv"
    file2 <- "Data/rio_neusa.csv"
    
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
        subset(tmp != 'Neu2_11' & tmp != 'Neu2_8' & tmp != 'Neu2_4') %>%
        subset(tmp != 'Neu3_11' & tmp != 'Neu3_10') %>%
        subset(tmp != 'Neu6_11' & tmp != 'Neu6_10' & 
               tmp != 'Neu2_8' & tmp != 'Neu2_1') %>%
        subset(tmp != 'Neu7_11' & tmp != 'Neu7_10' & tmp != 'Neu7_1') %>%
        subset(select = -tmp)
        
    
    if (ref_estations) {
        
        rio_frio <- c('Fri1', 'Fri2', 'Fri3', 'Fri4', 'Fri5')
        rio_neusa <- c('Neu4', 'Neu6', 'Neu7')
        reference <- c('Neu1', 'Neu2', 'Neu3')
        
        data$Tipo_Estacion[data$Estacion %in% rio_frio] <- 'Rio_Frio'
        data$Tipo_Estacion[data$Estacion %in% rio_neusa] <- 'Rio_Neusa'
        data$Tipo_Estacion[data$Estacion %in% reference] <- 'Referencia'
        
        return(data)
        
    } else {
    
        return(data)
    
    }
}


## Summary statistics (mean, median, quantiles, sd, skewness, kurtosis)


summary_statistics <- function(...) {
    
    ## Make a summary of the data for station by default. Pass 'by_sample = TRUE'
    ## argument to make a summary by each sampling event.  
    
    summary_data  <- function(funct, by_sample = FALSE, macroinvertebrates = FALSE) {
        
        if (macroinvertebrates) { 
            
            if (by_sample) {
                
                load_data() %>% 
                    group_by(Estacion, Muestra) %>% 
                    summarise( Number_Families = n_distinct(Familia),
                               Total_Macro = sum(Individuos)) %>%
                    return()
                
            } else {
                
                load_data() %>% 
                    group_by(Estacion) %>% 
                    summarise( Number_Families = n_distinct(Familia),
                               Total_Macro = sum(Individuos)) %>%
                    return()   
            }
            
        } else {
            
            if (by_sample) {
                
                load_data() %>% 
                    group_by(Estacion, Muestra) %>% 
                    summarise( Temperatura =funct(Temperatura),
                               Ox_disuelto = funct(Ox_disuelto),
                               pH = funct(pH),
                               Conductividad = funct(Conductividad),
                               Turbidez = funct(Turbidez)) %>% ## Añadir hidrologicos
                    return()
                
            } else {
                
                load_data() %>% 
                    group_by(Estacion) %>% 
                    summarise( Temperatura =funct(Temperatura),
                               Ox_disuelto = funct(Ox_disuelto),
                               pH = funct(pH),
                               Conductividad = funct(Conductividad),
                               Turbidez = funct(Turbidez)) %>%
                    return()
            }
            
        }
    }
    
    mean <- summary_data(mean, ...)
    median <- summary_data(median, ...)
    sd <- summary_data(sd, ...)
    skewness <- summary_data(skewness, ...)
    kurtosis <- summary_data(kurtosis, ...)
    macroinvertebrates <- summary_data(macroinvertebrates = TRUE ,...)
    
    summary_stats <- list(mean = mean, median = median, sd = sd, 
                               skewness = skewness, kurtosis = kurtosis, 
                               macroinvertebrates = macroinvertebrates)
    return(summary_stats)
}

## Graphical  eda


univariate_graphical_eda <- function(type_plot, parameters = NULL){
    
    ## Make the boxplot, histogram or qq plot for the passed parameters, 
    ## by default physicochemical parameters. 
    
    if (is.null(parameters)) {
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                        "Conductividad", "Turbidez")}
    ggobjtect_parameters <- list()
    
    eda_plots  <- function(parameter , type_plot = "boxplot" ){
        
        if (type_plot == "boxplot") { 
            plot <- load_data() %>%
                ggplot(mapping = aes(x = Estacion, y = .data[[parameter]])) +
                geom_boxplot() 
            
            return(plot)
            
        } else if (type_plot == "histogram") {
            
            plot <- load_data() %>%
                ggplot(mapping = aes(.data[[parameter]])) +
                geom_histogram() +
                facet_wrap(vars(Estacion))
            
            
            return(plot)
            
        } else if (type_plot == "quantile-normal") {
            
            plot <- load_data() %>%
                ggplot(data, mapping = aes(sample = .data[[parameter]])) +
                stat_qq() + 
                stat_qq_line(fullrange = TRUE) +
                facet_wrap(vars(Estacion))
            
            return(plot)
        }
    }
    
    for (i in 1:length(parameters)){
            
        parameter <- parameters[i]
            
        ggobjtect_parameters[[parameter]] <- eda_plots(parameter, 
                                                       type_plot)
            
    }
    
    return(ggobjtect_parameters)
    
}


make_pca <- function(type = 'fisicoquimicos', plot = TRUE, ... ) {
    
    ## perform the PCA analysis and return a list with  the ggplot objects
    ## relevants to the analysis. If plot = FALSE, returns the raw results
    ## of the analysis. 
    
    data <- load_data(ref_estations = TRUE)
    
    filter_data <- function(data, parameters) {
        
       parameters <- c("Tipo_Estacion", "Estacion", "Muestra", parameters) 
       
       data[parameters] %>% 
            distinct()  %>% 
            return()
         
     }
    
    plots_pca_analysis <- function(results_pca, plot_type = "Estacion") {
        
        eigenvalues <- fviz_eig(results_pca)
        
        
        individuals <- fviz_pca_ind(results_pca,
                                    col.ind = "cos2", 
                                    gradient.cols = c("#00AFBB", "#E7B800",
                                                      "#FC4E07"),
                                    repel = FALSE)
        
        variables <- fviz_pca_var(results_pca,
                                  col.var = "contrib",
                                  gradient.cols = c("#00AFBB", "#E7B800",
                                                    "#FC4E07"),
                                  repel = TRUE)
        
        biplot <- fviz_pca_biplot(results_pca,
                                  label = "none", 
                                  habillage = data[[plot_type]],
                                  addEllipses = FALSE)
        
        
        plot_pca <- list(eigenvalues = eigenvalues, 
                         individuals = individuals,
                         variables = variables, 
                         biplot = biplot)
        
        return(plot_pca)
        
    }
    
    if (type == 'fisicoquimicos') {
        
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                       "Conductividad", "Turbidez") 
        
        data <- filter_data(data, parameters)
        data_physicochemicals <- data[parameters]
        
        results_pca <- prcomp(data_physicochemicals, scale. = TRUE)
        
        if (plot) {
            
            plot_pca <- plots_pca_analysis(results_pca, ...)
            return(plot_pca)
            
        } else {
            
            return(results_pca)
        }
        
    } else if (type == 'hidrologicos') {
        
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                       "Conductividad", "Turbidez") 
        
        data <- filter_data(data, parameters)
        data_hydrological  <- data[parameters]
        results_pca <- prcomp(data_hydrological, scale. = TRUE)
        
        if (plot) {
            
            plot_pca <- plots_pca_analysis(results_pca, ...)
            return(plot_pca)
            
        } else {
            
            return(results_pca)
        }
    }
}


# ex <- read.csv('Data/rio_neusa.csv', stringsAsFactors = FALSE )
# ex$Fecha <- dmy(ex$Fecha) 
# ex$Mes <- month(ex$Fecha, label = TRUE, abbr = FALSE, locale = "es_CO.UTF-8" ) 
# ex$Año <- year(ex$Fecha)
# 
# ex2 <- ex %>% 
#     group_by(Estacion, Muestra, Epoca, Fecha, Mes, Año) %>% 
#     summarise( Temperatura =mean(Temperatura))
# 
# subset(ex2, Estacion == "Neu7")
# 
# inventario <- read.csv('Data/inventario.csv', stringsAsFactors = FALSE )
# inventario$Fecha <- dmy(inventario$Fecha) 
# inventario$Mes <- month(inventario$Fecha, label = TRUE, abbr = FALSE, locale = "es_CO.UTF-8" ) 
# inventario$Año <- year(inventario$Fecha)
# 
# inventario <- subset(inventario, Localidad == "Rio Frio")
# 
# inventario2 <- inventario %>% 
#     group_by(Latitud, Longitud, Fecha, Mes, Año) %>% 
#     summarise( Temperatura =mean(Ejemplares))

