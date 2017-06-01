### Establecer directorio
setwd("C:/Users/EMILIO JUSTINIANO/Desktop/Fondioc")


# instalando paquetes necesarios

install.packages( c("rio", "magrittr", "xlsx", "readxl", "stringi", "gdata",
                    "gsubfn", "dplyr", "plyr", "leaflet", "plotly"))

# Cargado de paquetes necesarios

pkgs <- c("rio", "magrittr", "xlsx", "readxl", "stringi", "gdata",
          "gsubfn", "dplyr", "plyr", "leaflet", "plotly" )

lapply(pkgs, function(x) require(x, character.only = TRUE))
rm(pkgs)

#Revisando directorio

getwd()

#Cargando base de datos

fechas_org<- import("Fechas_orgabs.xlsx")

prueba<- data.frame(fechas_org$`Tipo de fecha`,fechas_org$Fecha,fechas_org$`Suma total`)


vistaf_org <- ggplot(data = fechas_org, aes(x = Fecha, y = `Suma total`, group = factor(`Tipo de fecha`))) +
  geom_line(stat = "identity", aes(linetype = factor(`Tipo de fecha`)), size = 0.5, colour = "#552683") +
  ylab("Cantidad de Proyectos Desembolsados") + xlab("Fechas de desembolsos") + ggtitle("Linea de Tiempo de Desembolsos del Fondioc")

sfdg<-vistaf_org + scale_x_date(labels=date_format ("%b %y"))
sfdg
# kobe theme

kobe_theme2 <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Impact", colour = "#552683", size = 10),
    legend.background = element_rect(fill = "#E2E2E3"),
    legend.key = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    legend.text = element_text(family = "Impact", colour = "#E7A922", size = 10),
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.y = element_line(colour = "#E7A922"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}

vistaf_org + kobe_theme2()  + scale_linetype_discrete( "Tipo de fecha")





transorg<- import("Transferencia Total por ORGSOC.csv")

p3 <- ggplot(data = transorg, aes(x = ORGSOC, y = org_transf )) +
  geom_bar(stat = "identity", fill = "#552683") + coord_polar() +
  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE")
p3 + kobe_theme2()

df1<- orgs_cantproyecabs[ ,c(1:27)]


ggplot(fechas_org,aes(x,y))+geom_line(aes(color="First line"))+
  geom_line(data=df2,aes(color="Second line"))+
  labs(color="Legend text")

autoplot(prueba, facets = FALSE)







#guardado por si acaso
a_1<-plot_ly(x=a1$DEP , y = a1$DEP_transf,
             type = 'bar',
             name ='Bs./Dpto.',
             marker = list(color = 'rgba(11, 170, 157, 0.8 )'),
             width =1000, height = 300)%>%
  layout(title="",
         font=t)

a_2<-plot_ly(x= a2$Dpto.,
             y = a2$Proyectos ,
             name ='Proyectos/Dpto.',
             marker = list(color = 'rgba(0, 108, 110, 0.8 )'),
             type = 'bar',width =1000, height = 300)%>%
  layout(title="",
         font=t)
subplot(a_1,a_2)%>% hide_legend()

