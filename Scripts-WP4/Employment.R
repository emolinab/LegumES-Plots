library(magpie4)
library(dplyr)
library(ggplot2)

scenarios <- c("SSP1","SSP2","SSP3","SSP4","SSP5") 

fulldata <- c(SSP1 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpie/output/LegumES-H16EU-SSP1_2026-06-03_08.39.09/fulldata.gdx",
          SSP2 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP2_2026-06-03_08.10.28/fulldata.gdx",
          SSP3 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP3_2026-06-03_08.17.58/fulldata.gdx",
          SSP4 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP4_2026-06-03_08.25.44/fulldata.gdx",
          SSP5 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP5_2026-06-03_08.33.38/fulldata.gdx")

runNames <- c(SSP1 = "LegumES-H16EU-SSP1",
          SSP2 = "LegumES-H16EU-SSP2",
          SSP3 = "LegumES-H16EU-SSP3",
          SSP4 = "LegumES-H16EU-SSP4",
          SSP5 = "LegumES-H16EU-SSP5")

employment_list <- list()
legumes <- c("puls_pro", "soybean", "groundnut", "foddr")
regions <- c("World", "DEU", "EUC", "EUN", "EUS", "EUW")
eu_regions <- c("DEU", "EUC", "EUN", "EUS", "EUW")

employment_accum <- suppressWarnings(
  do.call(rbind, lapply(scenarios, function(sce) {
    agEmployment(fulldata[sce], detail = "byProduct", level = "regglo") |>
      (\(emp) mbind(emp, setCells(dimSums(emp[eu_regions ,,], dim = 1), "EUR"), setCells(dimSums(emp, dim = 1), "World")))() |>
      as.data.frame() |>
      transform(scenario = runNames[sce])
  }))
)

employment_accum <- employment_accum |>
  mutate(
    Year  = as.integer(as.character(Year)),
    Data1 = as.character(Data1)
  )


plotEmployment <- function(employment_accum,Regions,crops,tag){

cropsN<-c(puls_pro="Pulses", soybean="Soybean", groundnut="Groundnut", foddr="Fodder")

x<-list()

for(crop in crops){

  plot<- ggplot(data=subset(employment_accum, Region %in% Regions & Data1 %in% crop), aes(x=Year,y=Value, color = scenario))+geom_line(linewidth = 1)+
  geom_point(size = 2) +
  facet_wrap(~Region, scales = "free_y", ncol = 3)+
  theme_bw(base_family = "sans")+
  theme(# Título principal en negrita y centrado
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    # Títulos de los ejes en negrita
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
    
    # Etiquetas del eje X rotadas 90 grados
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    
    # Formato de los paneles (gris con texto negro)
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(size = 11, color = "black"),
    
    # Configuración de la leyenda
    legend.position = "bottom",
    legend.direction = "vertical", # Apila los elementos de la leyenda
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.key = element_blank(), # Quita el fondo gris detrás de las líneas en la leyenda
    
    # Cuadrícula interna (grid) sutil
    panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25))+  
  labs(
    title = paste0("Labor|Employment|Agricultural employment|+|Crop production|+|",cropsN[crop]),
    x = "Year",
    y = "mio people",
    color = "Model: MAgPIE"
  ) +scale_color_manual(values = c( "LegumES-H16EU-SSP1" = "#D9234B",
                                    "LegumES-H16EU-SSP2" = "#3AB050",
                                    "LegumES-H16EU-SSP3" = "#4169E1",
                                    "LegumES-H16EU-SSP4" = "#F28522",
                                    "LegumES-H16EU-SSP5" = "#91219E"))+
    geom_vline(xintercept = 1995, linetype = "dashed", color = "black")

   ggsave(
    filename =  paste0("/p/projects/landuse/users/mbacca/Collaborations/LegumES/PlottingScripts/Employment/Employment_",crop,"-",tag,".png"),
    plot = plot,
    width = 30,
    height = 30,
    dpi = 320,
    units = "cm"
  )


}


}

pl <- plotEmployment(employment_accum[,, legumes], Regions = c("DEU", "EUC", "EUN", "EUS", "EUW"), crops = legumes, tag="subregions")
pl2 <- plotEmployment(employment_accum[,, legumes], Regions = c("EUR"), crops = legumes, tag="EU")


#### Produces the data to use in plots in ReportPlots.R
# suppressWarnings(rm(emp_list))
# for(sce in scenarios){

#   emp<- agEmployment(fulldata[sce], detail = "byProduct", level = "regglo")
#   getNames(emp)<-paste0(sce,".",getNames(emp))
#   emp <-mbind(emp, setCells(dimSums(emp[eu_regions ,,],dim=1),"EUR"))
#   emp_list<-if(exists("emp_list")) mbind(emp_list,emp) else emp
# }



