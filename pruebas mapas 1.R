setwd("C:/Users/EMILIO JUSTINIANO/Desktop/Crónica de una Traición")
getwd()

ps<- import("PadillaSalud.csv")
View(ps)
install.packages("googleVis")
  library(googleVis)
##2do gráfico
# cargar el mapa
mapa <- readOGR(dsn = "Departamental.dbf",
                layer = "Departamental", verbose = FALSE)

#cargar los pop ups
mapa_popup <- paste0("<strong>DEPARTAMENTO: </strong>", mapa$NOM_DEP, "<br>", 
                     "<strong>Proyectos: </strong>", mapa$departam_3, "<br>",
                     "<strong>Desembolsos Totales: </strong>", mapa$departam_1,"<br>",
                     "<strong>Desembolsos Parciales: </strong>", mapa$departam_2,"<br>",
                     "<strong>Bs Desembolsados: </strong>", mapa$departam_5,"<br>",
                     "<strong>Costo Total en Bs:</strong>", mapa$departam_4,"<br>",
                     "<strong>Saldo sin Ejecutar en Bs:</strong>", mapa$departam_6)

# Visualizar el mapa                    
a <- leaflet(mapa) %>% 
  addPolygons(color = "black", weight = 0.6, smoothFactor = 0.5,
              opacity = 9.0, fillOpacity = 3, 
              popup = mapa_popup, 
              fillColor = ~colorFactor(c("#c6ecd9","#006C6E","#39ac73"),
                                      departam_7)(departam_7)) 
a
mapa@data




#Primer gráfico

a1<- rio::import("Transferencias por Gestiones.csv")
names(a1)<- c("Gestiones","Ingresos","Egresos", "Saldo")
plot_ly(a1, x = ~Gestiones, y = ~Ingresos, type = 'bar', 
        name = 'Ingresos en Bs.',
        marker = list(color = 'rgba(11, 170, 157, 0.8 )',
                      line = list(color = 'rgba(11, 170, 157, 0.8 )',
                                  width = 3))) %>%
  add_trace(y = ~Egresos, name = 'Egresos en Bs.',
            marker = list(color = 'rgba(0, 108, 110, 0.8 )',
                          line = list(color = 'rgba(0, 108, 110, 0.8 )',
                                      width = 3))) %>%
  layout(barmode = 'stack',
         xaxis = list(title = ""),
         yaxis = list(title ="")
         )%>% hide_legend()

#Organizaciones Sociales

f3<-import("organización.csv")
colors <- c('rgba(11, 170, 157, 0.8 )','rgba(11, 170, 157, 0.8 )','rgba(11, 170, 157, 0.8 )',
            'rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)','rgba(0, 108, 110, 0.8 )')


f3_1<- plot_ly() %>%
  add_pie(data = f3, labels = f3$`Organización Social`,
          values = f3$`Total desembolsado`, type = "pie",
          name = "Desembolso", 
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  add_pie(data = f3, labels = f3$`Organización Social`, 
          values = f3$`Proyectos aprobados`,
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          name = "Proyectos", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
  add_pie(data = f3, labels = f3$`Organización Social`,
          values = f3$`Costo del Proyecto según Convenio`,
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          name = "Costos", domain = list(x = c(0.25, 0.75), y = c(0, 0.6))) %>%
  layout(title = "", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

f3_1
# Estado de los proyectos
f4<- import("estado de los proyectos.csv")
names(f4)<- c("Clasificacion","Proyectos","Porcentaje","Importe Transferido Bs.")
colors1 <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 
            'rgb(171,104,87)', 'rgb(114,147,203)','rgba(0, 108, 110, 0.8 )')

f4_1<- plot_ly() %>%
  add_pie(data = f4, labels = f4$Clasificacion,
          values = f4$Proyectos, type = "pie",
          name = "Proyectos", 
          marker = list(colors = colors1,
                        line = list(color = '#FFFFFF', width = 1)),
          domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  add_pie(data = f4, labels = f4$Clasificacion, 
          values = f4$`Importe Transferido Bs.`,
          marker = list(colors = colors1,
                        line = list(color1 = '#FFFFFF', width = 1)),
          name = "Desembolsos en Bs", domain = list(x = c(0.6, 1),
                                                    y = c(0.4, 1))) %>%
  layout(title = "", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
f4_1

