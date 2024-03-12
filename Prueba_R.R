## cargue de librerias
library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(reshape2)

setwd("C:/Users/jsaj_/Desktop/prueba_adres")
## cargue de información 
Municipios <- read_excel("Municipios/Municipios.xlsx")
prestadores <- read_excel("Prestadores/Prestadores.xlsx")

## limpieza de texto
Municipios$Departamento <- gsub("[^a-zA-Z0-9áéíóúÁÉÍÓÚüÜñÑ ]", "", Municipios$Departamento)
Municipios$Municipio <- gsub("[^a-zA-Z0-9áéíóúÁÉÍÓÚüÜñÑ ]", "", Municipios$Municipio)
Municipios$Departamento <- gsub("^\\s+|\\s+$", "", Municipios$Departamento)
Municipios$Municipio <- gsub("^\\s+|\\s+$", "", Municipios$Municipio)
Municipios$Municipio <- gsub("\\s{2,}", " ", Municipios$Municipio)
Municipios$Departamento <- gsub("\\s{2,}", " ", Municipios$Departamento)
Municipios$Departamento <- str_to_title(tolower(Municipios$Departamento))
Municipios$Municipio <- str_to_title(tolower(Municipios$Municipio))
prestadores$depa_nombre <- str_to_title(tolower(prestadores$depa_nombre))
prestadores$muni_nombre <- str_to_title(tolower(prestadores$muni_nombre))


### homologacion llaves

## quitar acentos
prestadores$muni_nombre <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", prestadores$muni_nombre)
prestadores$depa_nombre <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", prestadores$depa_nombre)
Municipios$Municipio <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", Municipios$Municipio)
Municipios$Departamento <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", Municipios$Departamento)

## unificar nombres
prestadores$depa_nombre <- gsub("Barranquilla", "Atlantico", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("Bogota D.c", "Bogota", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("Bogota D.c", "Bogota", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("Buenaventura", "Valle Del Cauca", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("Cali", "Valle Del Cauca", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("San Andres Y Providencia", "San Andres", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("San Andres Y Providencia", "San Andres", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("Cartagena", "Bolivar", prestadores$depa_nombre)
prestadores$depa_nombre <- gsub("Santa Marta", "Magdalena", prestadores$depa_nombre)
prestadores$muni_nombre[(prestadores$depa_nombre == 'Boyaca')&(prestadores$muni_nombre == 'Buenavista')] <- 'Buena Vista'
prestadores$muni_nombre[(prestadores$depa_nombre == 'Magdalena')&(prestadores$muni_nombre == 'Puebloviejo')] <- 'Pueblo Viejo'
prestadores$muni_nombre[(prestadores$depa_nombre == 'Meta')&(prestadores$muni_nombre == 'Vistahermosa')] <- 'Vista Hermosa'
prestadores$muni_nombre[(prestadores$depa_nombre == 'Tolima')&(prestadores$muni_nombre == 'Rioblanco')] <- 'Rio Blanco'
prestadores$muni_nombre <- gsub("Valle De Guamez", "Valle Del Guamez", prestadores$muni_nombre)
prestadores$muni_nombre <- gsub("Valle Del Guamuez", "Valle Del Guamez", prestadores$muni_nombre)
Municipios$Municipio <- gsub("San Andres De Cuerquia", "San Andres", Municipios$Municipio)
Municipios$Municipio <- gsub("Bogota D C", "Bogota", Municipios$Municipio)
Municipios$Departamento <- gsub("Bogota D C", "Bogota", Municipios$Departamento)
Municipios$Municipio <- gsub("San Andres De Cuerquia", "San Andres", Municipios$Municipio)
Municipios$Municipio[(Municipios$Departamento == 'Bolivar')&(Municipios$Municipio == 'San Pablo De Borbur')] <- 'San Pablo'
Municipios$Municipio[(Municipios$Departamento == 'Boyaca')&(Municipios$Municipio == 'Buenavista')] <- 'Buena Vista'
Municipios$Municipio <- gsub("Dibula", "Dibulla", Municipios$Municipio)
Municipios$Municipio <- gsub("Chivolo", "Chibolo", Municipios$Municipio)
Municipios$Municipio[(Municipios$Departamento == 'Magdalena')&(Municipios$Municipio == 'Puebloviejo')] <- 'Pueblo Viejo'
Municipios$Municipio[(Municipios$Departamento == 'Meta')&(Municipios$Municipio == 'Vistahermosa')] <- 'Vista Hermosa'
Municipios$Municipio <- gsub("San Andres De Tumaco", "Tumaco", Municipios$Municipio)
Municipios$Municipio <- gsub("Valle De Guamez", "Valle Del Guamez", Municipios$Municipio)
Municipios$Municipio <- gsub("San Luis De Since", "Since", Municipios$Municipio)
Municipios$Municipio[(Municipios$Departamento == 'Tolima')&(Municipios$Municipio == 'Rioblanco')] <- 'Rio Blanco'



### Uso de SQLite mediante una conexion temporal

## carge de la informacion a tablals temporales

mydb <- dbConnect(RSQLite::SQLite(), "")

dbWriteTable(mydb, "municipios", Municipios)
dbWriteTable(mydb, "prestadores", prestadores)



## consolidacion de la tabla 
tab3 <- dbGetQuery(mydb, "select t1.*,t2.Superficie,t2.Poblacion
from 
(select depa_nombre,muni_nombre,clpr_nombre,count(1) conteo_pretadores
from prestadores
group by 1,2,3) t1
LEFT JOIN
(select Departamento,Municipio,Superficie,Poblacion
from municipios) t2
on t1.muni_nombre=t2.Municipio AND t1.depa_nombre=t2.Departamento")
dbWriteTable(conn = mydb, "consolidado_general", tab3)


### grafica de proporcion por tipo de prestador 

tabla_1_1<- dbGetQuery(mydb,"select clpr_nombre,count(1) conteo_pretadores
from prestadores
group by 1")
tabla_1_1$nombre <- c("IPS", "OSDPSS", "PI", "TEP")

porcentajes <- tabla_1_1$conteo_pretadores / sum(tabla_1_1$conteo_pretadores) * 100
pie(tabla_1_1$conteo_pretadore, labels = paste(tabla_1_1$nombre, "\n", round(porcentajes, 1), "%"), main = "Gráfico de Torta con Porcentajes")


## graficas de cantidad de prestadores por region

tabla_1<- dbGetQuery(mydb,"select t1.clpr_nombre,t2.Region,count(1) as prestadores_region
from 
(select clpr_nombre,muni_nombre,depa_nombre
from prestadores
) t1
LEFT JOIN
(SELECT Departamento,Municipio, Region
from municipios) t2
on t1.muni_nombre=t2.Municipio AND t1.depa_nombre=t2.Departamento
group by 1,2")


tipos_unicos <- unique(tabla_1$clpr_nombre)

for (tipo in tipos_unicos ) {
  mi_tabla_filtrada <- tabla_1 %>% filter((clpr_nombre == tipo)&(Region!='NA'))
  plot_title <- paste("Gráfico de Barras ", tipo)
  print(ggplot(mi_tabla_filtrada, aes(x = clpr_nombre, y = prestadores_region, fill = Region)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = plot_title,
         x = "Región",
         y = "Total") +
    theme_minimal())
  }

## conteo por razon social


tabla_2<- dbGetQuery(mydb,"select clpr_nombre,naju_nombre,count(1) conteo_prestadores
from prestadores
group by 1,2")


tabla_cruzada <- dcast(tabla_2, clpr_nombre ~ naju_nombre, value.var = "conteo_prestadores", fun.aggregate = sum)
tabla_cruzada$Total <- rowSums(tabla_cruzada[, -1])
total_por_columna <- colSums(tabla_cruzada[, -1])

# Agregar una fila con el total por columnas
tabla_cruzada <- rbind(tabla_cruzada, c("Total por tipo", total_por_columna))

# Mostrar la tabla cruzada
print(tabla_cruzada)

### calcular la antiguedad de los Prestadores 
tabla_3<- dbGetQuery(mydb,"SELECT
depa_nombre,
muni_nombre,
fecha_vencimiento_form,
fecha_radicacion_form,
(julianday(fecha_vencimiento_form) - julianday(fecha_radicacion_form)) AS dias_proxima_renovacion,
round((julianday(date('now')) - julianday(fecha_radicacion_form))/360,2) AS anios_activo,
round((julianday(date('now')) - julianday(fecha_radicacion_form))/12,2) AS mes_activo,
julianday(date('now')) - julianday(fecha_radicacion_form) AS dia_activo
FROM
(SELECT depa_nombre,
muni_nombre,
strftime('%Y-%m-%d', substr(fecha_radicacion, 1, 4) || '-' || substr(fecha_radicacion, 5, 2) || '-' || substr(fecha_radicacion, 7, 2)) as fecha_radicacion_form,
strftime('%Y-%m-%d', substr(fecha_vencimiento, 1, 4) || '-' || substr(fecha_vencimiento, 5, 2) || '-' || substr(fecha_vencimiento, 7, 2)) as fecha_vencimiento_form
FROM prestadores)")

histograma <- hist(tabla_3$anios_activo, breaks = 25, col = "lightblue", main = "Distribución años de servicio",xlab = "Años de antigüedad")

### analisis departamental de la cobertura 

tabla_4<- dbGetQuery(mydb,"Select t1.*,
t2.total_municipios,
round(CAST(t1.municipios_presencia as REAL)/t2.total_municipios, 2) as cobertura
FROM
(select depa_nombre,sum(conteo_pretadores) as prestadores, 
round(sum(conteo_pretadores)/sum(DISTINCT Superficie),3) as densidad_prestadores_km,
round(sum(conteo_pretadores)/(CAST(sum(DISTINCT Poblacion)AS REAL)/100000),4) as densidad_prestadores_pob_100, 
round(sum(DISTINCT Poblacion)/sum(DISTINCT Superficie),3) as habitante_km, 
round((sum(conteo_pretadores)/sum(DISTINCT Poblacion))/sum(DISTINCT Superficie),3)as prestadores_densidad,
count(distinct muni_nombre) as municipios_presencia
from consolidado_general
group by 1) t1
LEFT JOIN
(select Departamento,count(1) as total_municipios
from municipios
group by 1) t2
on t1.depa_nombre=t2.Departamento")

## estadisticas por cobertura por municipios
subset(tabla_4, cobertura == max(tabla_4$cobertura))$depa_nombre
subset(tabla_4, cobertura == min(tabla_4$cobertura))$depa_nombre


## estadisticas por densidad poblacional 
subset(tabla_4, densidad_prestadores_pob_100 == max(tabla_4$densidad_prestadores_pob_100))$depa_nombre
subset(tabla_4, densidad_prestadores_pob_100 == min(tabla_4$densidad_prestadores_pob_100))$depa_nombre

## estadisticas por densidad terrirorial
subset(tabla_4, prestadores_densidad == max(tabla_4$prestadores_densidad))$depa_nombre
subset(tabla_4, prestadores_densidad == min(tabla_4$prestadores_densidad))$depa_nombre




dbDisconnect(mydb)

