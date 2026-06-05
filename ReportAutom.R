library(ggplot2)
library(magclass)
library(dplyr)
library(patchwork)
library(officer)

scenarios <- c("SSP1","SSP2","SSP3","SSP4","SSP5") 


runsReport <- c(SSP1 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP1_2026-06-03_08.39.09/report.mif",
          SSP2 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP2_2026-06-03_08.10.28/report.mif",
          SSP3 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP3_2026-06-03_08.17.58/report.mif",
          SSP4 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP4_2026-06-03_08.25.44/report.mif",
          SSP5 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP5_2026-06-03_08.33.38/report.mif")    


variables_magpie <- c(
  "Population (million people)",
  "Income MER (million US$2017 MER/yr)",
  "Demand|Food|Crops|Other crops|+|Pulses (Mt DM/yr)",
  "Demand|Feed|Crops|Other crops|+|Pulses (Mt DM/yr)",
  "Demand|Food|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  "Demand|Feed|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  "Demand|Processing|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  "Demand|Feed|+|Forage (Mt DM/yr)",
  "Demand|Feed|Secondary products|+|Oilcakes (Mt DM/yr)",
  "Demand|Food|Crops|Oil crops|+|Other oil crops incl rapeseed (Mt DM/yr)",
  "Demand|Material|Crops|Oil crops|+|Other oil crops incl rapeseed (Mt DM/yr)",
  "Demand|Processing|Crops|Oil crops|+|Other oil crops incl rapeseed (Mt DM/yr)",
  "Demand|Bioenergy|Crops|Oil crops|+|Other oil crops incl rapeseed (Mt DM/yr)",
  "Demand|Food|Crops|Oil crops|+|Sunflower (Mt DM/yr)",
  "Demand|Material|Crops|Oil crops|+|Sunflower (Mt DM/yr)",
  "Demand|Processing|Crops|Oil crops|+|Sunflower (Mt DM/yr)",
  "Demand|Bioenergy|Crops|Oil crops|+|Sunflower (Mt DM/yr)",
  "Demand|Food|Crops|Oil crops|+|Oilpalms (Mt DM/yr)",
  "Demand|Material|Crops|Oil crops|+|Oilpalms (Mt DM/yr)",
  "Demand|Processing|Crops|Oil crops|+|Oilpalms (Mt DM/yr)",
  "Demand|Bioenergy|Crops|Oil crops|+|Oilpalms (Mt DM/yr)",
  "Trade|Net-Trade|Secondary products|+|Oilcakes (Mt DM/yr)",
  "Demand|Secondary products|+|Oilcakes (Mt DM/yr)",
  "Production|Secondary products|+|Oilcakes (Mt DM/yr)",
  "Trade|Net-Trade|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  "Demand|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  "Production|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  "Trade|Net-Trade|Crops|Other crops|+|Pulses (Mt DM/yr)",
  "Demand|Crops|Other crops|+|Pulses (Mt DM/yr)",
  "Production|Crops|Other crops|+|Pulses (Mt DM/yr)",
  "Trade|Net-Trade|Crops|Oil crops|+|Other oil crops incl rapeseed (Mt DM/yr)",
  "Demand|Crops|Oil crops|+|Other oil crops incl rapeseed (Mt DM/yr)",
  "Production|Crops|Oil crops|+|Other oil crops incl rapeseed (Mt DM/yr)",
  "Trade|Net-Trade|Crops|Oil crops|+|Sunflower (Mt DM/yr)",
  "Demand|Crops|Oil crops|+|Sunflower (Mt DM/yr)",
  "Production|Crops|Oil crops|+|Sunflower (Mt DM/yr)",
  "Demand|Crops|Oil crops|+|Oilpalms (Mt DM/yr)",
  "Production|Crops|Oil crops|+|Oilpalms (Mt DM/yr)",
  "Trade|Net-Trade|+|Crops (Mt DM/yr)",
  "Production|+|Crops (Mt DM/yr)",
  "Demand|++|Crops (Mt DM/yr)",
  "Trade|Net-Trade|+|Livestock products (Mt DM/yr)",
  "Production|+|Livestock products (Mt DM/yr)",
  "Demand|++|Livestock products (Mt DM/yr)",
  "Trade|Net-Trade|+|Secondary products (Mt DM/yr)",
  "Demand|++|Secondary products (Mt DM/yr)",
  "Production|+|Secondary products (Mt DM/yr)",
  "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Symbiotic Crops (Mt Nr/yr)",
  "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer (Mt Nr/yr)",
  "Resources|Nitrogen|Cropland Budget|Balance|+|Nutrient Surplus (Mt Nr/yr)",
  "Resources|Nitrogen|Cropland Budget|Inputs|+|Recycled Aboveground Crop Residues (Mt Nr/yr)",
 "Resources|Nitrogen|Cropland Budget|Inputs|+|Recycled Belowground Crop Residues (Mt Nr/yr)",
 "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Harvested Crops (Mt Nr/yr)",
 "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Aboveground Crop Residues (Mt Nr/yr)",
 "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure Recycled from Confinements (Mt Nr/yr)",
 "Labor|Employment|Agricultural employment (mio people)",
"Labor|Employment|Agricultural employment|+|Crop products (mio people)",
"Resources|Land Cover|+|Cropland (million ha)",
"Resources|Land Cover|+|Forest (million ha)",
"Resources|Land Cover|+|Other Land (million ha)",
"Resources|Land Cover|+|Pastures and Rangelands (million ha)",
"Resources|Land Cover|+|Urban Area (million ha)"
)


############# Parametros #############
colors<- c("SSP1" = "#3AB050", "SSP2" = "#4169E1", "SSP3" = "#D9234B", "SSP4" = "#F28522", "SSP5" = "#91219E")


### MAgPIE plot style function
magpiePlotStyle <- function() {
  list(
    scale_color_manual(values = colors),
    theme_bw(base_family = "sans"),
    theme(
      # Título principal del subplot centrado y negrita
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      
      # Ejes en negrita
      axis.title.x = element_text(face = "bold", size = 10),
      axis.title.y = element_text(face = "bold", size = 10),
      
      # Rotar años en X
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      
      # Paneles (Facets) parecidos a la imagen
      strip.background = element_rect(fill = "grey90", color = "black"),
      strip.text = element_text(size = 9, color = "black"),
      
      # Configuración de leyenda compartida (se moverá abajo)
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 8),
      legend.key = element_blank(),
      
      # Cuadrícula
      panel.grid.major = element_line(color = "grey85", linewidth = 0.5)
    )
  )
}

#### Graph creation

compoundGraph <- function(data_variable_single, nombre_variable) {
  
  title <- sub(" \\(.*\\)$", "", nombre_variable)
  units <- sub(".*\\((.*)\\)$", "\\1", nombre_variable)

  data_variable_single <- data_variable_single[,seq(1995,2050,5),]
  # A. Convertir datos magpie a long-format para dplyr y ggplot2
  # Asumimos un dataframe long con columnas: Year, Region, Scenario (Model), Variable, Value
  df_long <- as.data.frame(data_variable_single, rev = TRUE)[,c("Region","Year","Data1","Data2","Value")] 
  # NOTA: Ajusta los nombres de las columnas si son diferentes en tu objeto magclass (e.g., 'Data1' -> 'Scenario')
  names(df_long) <- c("Region", "Year", "Scenario", "Variable","Value") # Layout estándar de magclass
  df_long$Year <- as.numeric(gsub("y", "", as.character(df_long$Year)))
  

  # B. --- GRÁFICO IZQUIERDO: Solo EUR ---
  
  # Filtrar solo EUR
  df_eur <- df_long %>% 
    filter(Region == "EUR")
  
  plot_left <- ggplot(df_eur, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    # Línea vertical punteada al inicio (aprox 1995)
    geom_vline(xintercept = 1995, linetype = "dashed", color = "black") +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    magpiePlotStyle() +
    facet_wrap(~ Region, ncol = 3, scales = "free_y") + 
    labs(
      #title = title, # Título del subplot
      y = units, # Reemplazar con tus unidades reales
      color = "Model: MAgPIE"
    )
  
  # C. --- GRÁFICO DERECHO: Facetas Regionales ---
  
  regions <- c("DEU", "EUC", "EUN", "EUS", "EUW")
  # Filtrar solo la lista específica de regiones
  df_regions <- df_long %>% 
    filter(Region %in% regions)
  
  plot_right <- ggplot(df_regions, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    # Línea vertical punteada
    geom_vline(xintercept = 1995, linetype = "dashed", color = "black") +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    # Aquí está la magia de facetas
    facet_wrap(~ Region, ncol = 3, scales = "free_y") + 
    magpiePlotStyle() +
    labs(
     # title = title, # Título
      y = units,
      color = "Model: MAgPIE"
    )
  
  # D. --- COMBINACIÓN CON PATCHWORK ---
  
  # Usar el operador '+' para ponerlos lado a lado.
  # El argumento 'guide_area()' crea un espacio dedicado para la leyenda combinada.
  combined_plot <- (plot_left | plot_right) +
    plot_layout(guides = "collect", widths = c(1, 1.5)) +# Ancho relativo opcional
    plot_annotation(
      title = nombre_variable,
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))
    ) & theme(legend.position = "bottom")


return(combined_plot)
}




for (sce in scenarios) {
  data <- read.report(runsReport[sce])[[1]][[1]][,,variables_magpie]
  getNames(data) <- paste0(sce,".", getNames(data))
  data_list2 <- if (exists("data_list2")) { mbind(data_list2, data) } else { data} 
}

write.magpie(data_list2, file_name = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/PlottingScripts/DataRunsLegumES/dataMAgPIE040526.rds")

doc <- officer::read_pptx()

for(var_name in variables_magpie) {
  
  cat(paste0("Creando gráfico compuesto para: ", var_name, "\n"))
  
  # 1. Filtrar los datos magpie solo para esta variable
  data_filt_var <- data_list2[, , var_name]
  
  # 2. Generar el gráfico lado a lado usando la nueva función
  micompoundGraph  <- compoundGraph(data_filt_var, var_name)
  
  # --- AQUÍ CONECTAS CON EL PPTX (officer) ---
  # El objeto 'mi_grafico_compuesto' es lo que pasas a ph_with()
   doc <- add_slide(doc, layout = "Title and Content")
   doc <- ph_with(doc, value = micompoundGraph, location = ph_location_fullsize())
  
  # Para probar aquí en la consola, puedes simplemente ploteárlo:
   
}



print(doc, target="/p/projects/landuse/users/mbacca/Collaborations/LegumES/PlottingScripts/Report/LegumES040626.pptx") 

# Filtrar solo las variables que necesitas


#  file <- "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-calib-H16EU-level07_2026-06-02_18.31.28/report.mif"
#  data <- read.report(file)[[1]]
 mi_objeto <- unique(getNames(data[[1]]))
write.csv(as.data.frame(mi_objeto), file = "mis_datos_legumes2.csv", row.names = FALSE)
