## Load packages 

library(e1071)
library(ade4)
library(factoextra)
library(kableExtra)
library(flextable)
library(patchwork)
library(Hotelling)
library(gridExtra)
library(tidyverse)
library(ggrepel)

## Load data

Tipo_estacion <- function(data) {
  
  rio_frio <- c('Fri1', 'Fri2', 'Fri3', 'Fri4')
  rio_frio2 <- c('Fri5')
  rio_neusa2 <- c('Neu4', 'Neu6', 'Neu7')
  rio_neusa <- c('Neu1', 'Neu2', 'Neu3')
  
  data$Tipo_Estacion[data$Estacion %in% rio_frio] <- 'FrioB'
  data$Tipo_Estacion[data$Estacion %in% rio_frio2] <- 'FrioA'
  data$Tipo_Estacion[data$Estacion %in% rio_neusa2] <- 'NeusaB'
  data$Tipo_Estacion[data$Estacion %in% rio_neusa] <- 'NeusaA'
  
  return(data)
  
}

load_rlq <- function(RTable, LTable, QTable){
    
    ## Return an object with the R, L, and Q matrices, by default
    ## work with physichochemicals parameters 
    
    
    enviromental <- read.csv(RTable, stringsAsFactors = FALSE) %>% 
                    Tipo_estacion()
    
    abundance <- read.csv(LTable, stringsAsFactors = FALSE) %>% 
                 Tipo_estacion()


    traits <- read.csv(QTable, stringsAsFactors = FALSE, check.names = FALSE) %>% 
              column_to_rownames( var = "Familia") %>%      
              subset(rownames(.) %in% names(abundance))               
              
    RLQ <- list(R_table = enviromental, Q_table = traits,
                L_table = abundance)
    
    return(RLQ)
    
} 

## Summary statistics (mean, median, quantiles, sd, skewness, kurtosis)

summary_statistics <- function(data, ...) {
    
    ## Make a summary of the data for station by default. Pass 'by_sample = TRUE'
    ## argument to make a summary by each sampling event.  
  
    ## data = read.csv("../Data/muestreo_completo.csv)
    
    summary_data  <- function(data, funct, by_sample = FALSE, macroinvertebrates = FALSE) {
        
        if (macroinvertebrates) { 
            
            if (by_sample) {
                
                data %>% 
                    group_by(Estacion, Muestra) %>% 
                    summarise( Number_Families = n_distinct(Familia),
                               Total_Macro = sum(Individuos)) %>%
                    return()
                
            } else {
                
                data %>% 
                    group_by(Estacion) %>% 
                    summarise( Familias = n_distinct(Familia),
                               Total_Macro = sum(Individuos)) %>%
                    return()   
            }
            
        } else {
            
            if (by_sample) {
                
                data %>% 
                    group_by(Estacion, Muestra) %>% 
                    summarise( Temperatura =funct(Temperatura),
                               OxDisuelto = funct(OxDisuelto),
                               pH = funct(pH),
                               Conductividad = funct(Conductividad),
                               Turbidez = funct(Turbidez),
                               Profundidad = funct(Profundidad),
                               Velocidad = funct(Velocidad)) %>% 
                    return()
                
            } else {
                
                data %>% 
                    group_by(Estacion) %>% 
                    summarise( Temperatura =funct(Temperatura),
                               OxDisuelto = funct(OxDisuelto),
                               pH = funct(pH),
                               Conductividad = funct(Conductividad),
                               Turbidez = funct(Turbidez), 
                               Profundidad = funct(Profundidad),
                               Velocidad = funct(Velocidad),
                               Altitud = funct(Altitud)) %>%
                    return()
            }
            
        }
    }
    
    mean <- summary_data(data, mean, ...)
    median <- summary_data(data, median, ...)
    sd <- summary_data(data, sd, ...)
    skewness <- summary_data(data, skewness, ...)
    kurtosis <- summary_data(data, kurtosis, ...)
    macroinvertebrates <- summary_data(data, macroinvertebrates = TRUE ,...)
    
    summary_stats <- list(mean = mean, median = median, sd = sd, 
                               skewness = skewness, kurtosis = kurtosis, 
                               macroinvertebrates = macroinvertebrates)
    return(summary_stats)
}

make_manova <- function(data, funct = manova , est1, est2, global = FALSE ) {
    
    ## If run with the hotelling.test function of the Hotelling package 
    ## the results are the same
  
   
    if (global) {
      
      data_physicochemicals <- data %>% Tipo_estacion()
   
    
      results_manova <- funct(cbind(Temperatura, OxDisuelto, pH, 
                                    Conductividad, Turbidez) ~ Tipo_Estacion, 
                              data = data_physicochemicals )
      
      
      return(results_manova)
    }
    
    else {
      
        data_physicochemicals <- data %>% Tipo_estacion()
        
        
        data_physicochemicals <- filter(data_physicochemicals, 
                                        Tipo_Estacion == est1 |
                                            Tipo_Estacion == est2)
       
        results_manova <- funct(cbind(Temperatura, OxDisuelto, pH, 
                                   Conductividad, Turbidez) ~ Tipo_Estacion, 
                                 data = data_physicochemicals )
            
           
        return(results_manova)
        
    } 
}

## Graphical  eda

univariate_graphical_eda <- function(data, type_plot, parameters = NULL){
    
    ## Make the boxplot, histogram or qq plot for the passed parameters, 
    ## by default physicochemical parameters. 
    
    if (is.null(parameters)) {
      parameters <- c("Temperatura", "OxDisuelto", "pH", 
                      "Conductividad", "Turbidez", 
                      "Velocidad") }
  
    ggobjtect_parameters <- list()
    
    eda_plots  <- function(data, parameter , type_plot = "boxplot" ){
        
        if (type_plot == "boxplot") { 
            plot <- data %>%
                ggplot(mapping = aes(x = Estacion, y = .data[[parameter]])) +
                geom_boxplot() + theme_classic()
            
            return(plot)
            
        } else if (type_plot == "histogram") {
            
            plot <- data %>%
                ggplot(mapping = aes(.data[[parameter]])) +
                geom_histogram() +
                facet_wrap(vars(Estacion)) +
                theme_classic()
            
            
            return(plot)
            
        } else if (type_plot == "quantile-normal") {
            
            plot <- data %>%
                ggplot(mapping = aes(sample = .data[[parameter]])) +
                stat_qq() + 
                stat_qq_line(fullrange = TRUE) +
                facet_wrap(vars(Estacion)) + theme_classic()
            
            return(plot)
        }
    }
    
    for (i in 1:length(parameters)){
            
        parameter <- parameters[i]
            
        ggobjtect_parameters[[parameter]] <- eda_plots(data, parameter, 
                                                       type_plot)
            
    }
    
    return(ggobjtect_parameters)
    
}
          
make_pca <- function(data, parameters = NULL, plot = TRUE, ... ) {
    
    ## perform the PCA analysis and return a list with  the ggplot objects
    ## relevants to the analysis. If plot = FALSE, returns the raw results
    ## of the analysis. 
    
   
    plots_pca_analysis <- function(results_pca, plot_type = "Estacion") {
        
        eigenvalues <- fviz_eig(results_pca, main = "", 
                                ylab = "% Varianza", xlab = "Componentes") +
          theme_classic()
        
        
        individuals <- fviz_pca_ind(results_pca,
                                    col.ind = "cos2", 
                                    gradient.cols = c("#00AFBB", "#E7B800",
                                                      "#FC4E07"),
                                    repel = FALSE) +
          theme_classic()
        
        variables <- fviz_pca_var(results_pca,
                                  col.var = "contrib",
                                  gradient.cols = c("#00AFBB", "#E7B800",
                                                    "#FC4E07"),
                                  repel = TRUE,  title = "") +
          theme_classic()
        
        biplot <- fviz_pca_biplot(results_pca,
                                  label = "var", 
                                  habillage = data[[plot_type]],
                                  addEllipses = FALSE,
                                  legend = "left", 
                                  palette = "futurama", 
                                  title = "") 
        
        
        plot_pca <- list(eigenvalues = eigenvalues, 
                         individuals = individuals,
                         variables = variables, 
                         biplot = biplot)
        
        return(plot_pca)
        
    }
  
    if (is.null(parameters)) {
      
      parameters <- c("Temperatura", "OxDisuelto", "pH", 
                       "Conductividad", "Turbidez") }
        
    data_physicochemicals <- data[parameters]
    
    results_pca <- prcomp(data_physicochemicals, scale. = TRUE)
    
    if (plot) {
        
        plot_pca <- plots_pca_analysis(results_pca, ...)
        return(plot_pca)
        
    } else {
        
        return(results_pca)
    }
        
}

make_cluster <- function(data, parameters = NULL) {
  
    if (is.null(parameters)) {
      
        parameters <- c("Temperatura", "OxDisuelto", "pH", 
                        "Conductividad", "Turbidez") }
    
    data_physicochemicals <- data %>%
                             select(-Muestra) %>%
                             group_by(Tipo_Estacion, Estacion) %>%
                             select(all_of(parameters)) %>%
                             summarise(across(.cols = everything() , mean))
    
    distance_matrix <-  data_physicochemicals %>%
                        ungroup() %>%
                        select(-Tipo_Estacion) %>%
                        column_to_rownames(var = 'Estacion') %>%
                        scale() %>%
                        dist() 
    
    cluster <-  hclust(distance_matrix, method = "average")

    return(cluster)
 
} 

  
## Fuzzy Correspondence Analysis and RLQ analysis ###  Refactor from here

make_fca <- function(R_Table, L_Table, Q_Table, presence_ausence = FALSE, 
                     parameters = NULL ) {

   ## This function takes as imput a fuzzy coded table of traits  in each site
   ## and a table of enviromental variables and perform a CCA. return a dudi
   ## object of the class pcai
  
    if (is.null(parameters)) {
      
        parameters <- c("Temperatura", "OxDisuelto", "pH", 
                        "Conductividad", "Turbidez") }

    abundance <- L_Table %>%  
        select(-Tipo_Estacion) %>%
        unite(col = names, Estacion, Muestra) %>%
        column_to_rownames('names') 
    
    if (presence_ausence) {
        abundance[abundance>0] <- 1
    }
    
    envir <- R_Table  %>%
        select(-Tipo_Estacion) %>%
        unite(col = names, Estacion, Muestra) %>%
        column_to_rownames('names') %>%
        select(all_of(parameters))
    
    traits <- Q_Table 
    
    traits_by_site <- as.matrix(abundance) %*% as.matrix(traits) 
    traits_by_site <- as.data.frame(traits_by_site)
    traits_by_site <- traits_by_site %>%
      prep.fuzzy.var(col.blocks = c(Tipo_Alimento = 6, 
                                    Habitos_Alimenticios = 5, 
                                    Respiracion = 4,
                                    Forma_Corporal = 4, 
                                    Movilidad = 6,
                                    Tamaño_Maximo= 5))
                    
    object <- dudi.fca(traits_by_site, scannf = FALSE, nf = 3 )
    
    
    pcaiv <- pcaiv(object, envir, scannf = FALSE, nf = 3)
  
    return(pcaiv)

  
}

make_rlq <- function(R_Table, L_Table, Q_Table, fourthcorner = FALSE, 
                     parameters = NULL ) {
  
    if (is.null(parameters)) {
      
        parameters <- c("Temperatura", "OxDisuelto", "pH", 
                    "Conductividad", "Turbidez") 
    }
  
    abundance <<- L_Table %>%  
                 select(-Tipo_Estacion) %>%
                 unite(col = names, Estacion, Muestra) %>%
                 column_to_rownames('names') 
    
    abundance_test <<- dudi.coa(abundance, scannf = FALSE, nf = 3)

    envir <<- R_Table %>% ungroup() %>%
           select(-Tipo_Estacion) %>%
           unite(col = names, Estacion, Muestra) %>%
           column_to_rownames('names') %>%
           select(all_of(parameters))
    
    envir_test <<-  dudi.pca(envir, scannf = FALSE, nf = 3, 
                            row.w = abundance_test$lw)
  
    traits <<- Q_Table %>%
              prep.fuzzy.var(col.blocks = c(Tipo_Alimento = 6, 
                                            Habitos_Alimenticios = 5, 
                                            Respiracion = 4,
                                            Forma_Corporal = 4, 
                                            Movilidad = 6,
                                            Tamaño_Maximo= 5),
                             row.w = abundance_test$cw) 
    traits_test <<-  dudi.fca(traits, scannf = FALSE, nf = 3)
      
    rlq1 <- rlq(envir_test, abundance_test, traits_test,  scannf = FALSE, nf = 2)
    

    
    if (fourthcorner) {
        
        f4c_R <- fourthcorner.rlq(rlq1,type="R.axes",
                                  p.adjust.method.G = "none",
                                  p.adjust.method.D = "none",
                                  nrepet = 5000)
        f4c_Q <- fourthcorner.rlq(rlq1,type="Q.axes", 
                                  p.adjust.method.G = "none",
                                  p.adjust.method.D = "none",
                                  nrepet = 5000)
        f4c <-  list(envir = f4c_R, traits = f4c_Q)   
        
        return(f4c)
        
    }
    
    else {
      
        return(rlq1)
    }
}

make_fourthcorner <- function(R_table, L_Table, Q_Table, nrepet = NULL) {
 
   if (is.null(nrepet)) {
      nrepet <- 1000 # the higher, the longer the run lasts
   }
  
   if (is.null(parameters)) {
    
    parameters <- c("Temperatura", "OxDisuelto", "pH", 
                    "Conductividad", "Turbidez") 
   }
  
    abundance <- L_Table %>%  
        select(-Tipo_Estacion) %>%
        unite(col = names, Estacion, Muestra) %>%
        column_to_rownames('names') 
    
    envir <- R_Table %>% ungroup() %>%
        select(-Tipo_Estacion) %>%
        unite(col = names, Estacion, Muestra) %>%
        column_to_rownames('names') %>%
        select(all_of(parameters))
      
    traits <- Q_Table %>%
        prep.fuzzy.var(col.blocks = c(Tipo_Alimento = 6 , 
                                      Habitos_Alimenticios = 5, 
                                      Respiracion = 4,
                                      Forma_Corporal = 4, 
                                      Movilidad = 6,
                                      Tamaño_Maximo= 5)
                       ) 
    
 fourthcorner_results <- fourthcorner(envir, abundance, traits, 
                                      modeltype = 6, 
                                      p.adjust.method.G = "none",
                                      p.adjust.method.D = "none",
                                      nrepet = nrepet)
 return(fourthcorner_results)
 

}

