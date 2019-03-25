Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos
```{r}
#Se cargan los paquetes que se van a utlizar

library(readxl)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(plotly)
library(shinyWidgets)

#Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos

#Hay que eliminar las 6 primeras filas ya que no son nombres y datos redundantes para la tabla 

url <- "http://www.ine.es/jaxiT3/files/t/es/xlsx/4932.xlsx?nocab=1"
destfile <- "Porcentajes_respecto_total.xlsx"
curl::curl_download(url, destfile)
activos_valores_Abs <- read_excel(destfile, skip = 6, col_names = FALSE)

#Renombro las columnas por numeros para poder hacer una extraccion más facil 
for ( i in 1:79)
  colnames(activos_valores_Abs)[i] <- i

#AMBOS SEXOS#

#Extraccion de una subtabla que contenga el total  de ambos sexos 
ambos_total.1 <- as.data.frame( activos_valores_Abs[2:23,1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_total.1) = ambos_total.1[1,]
ambos_total.1 = ambos_total.1[-1,]
ambos_total.1 = ambos_total.1[-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de ambos sexos 
ambos_16_19.1 <- as.data.frame( activos_valores_Abs[2:23,c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_16_19.1) = ambos_16_19.1[1,]
ambos_16_19.1 = ambos_16_19.1[-1,]
ambos_16_19.1 = ambos_16_19.1[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de ambos sexos 
ambos_20_24.1 <- as.data.frame( activos_valores_Abs[2:23,c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_20_24.1) = ambos_20_24.1[1,]
ambos_20_24.1 = ambos_20_24.1[-1,]
ambos_20_24.1 = ambos_20_24.1[-1,]

#Extraccion de una subtabla que contenga de 25 a 34 años  de ambos sexos 
ambos_25_34.1 <- as.data.frame( activos_valores_Abs[2:23,c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_25_34.1) = ambos_25_34.1[1,]
ambos_25_34.1 = ambos_25_34.1[-1,]
ambos_25_34.1 = ambos_25_34.1[-1,]

#Extraccion de una subtabla que contenga de 35 a 44 años  de ambos sexos 
ambos_35_44.1 <- as.data.frame( activos_valores_Abs[2:23,c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_35_44.1) = ambos_35_44.1[1,]
ambos_35_44.1 = ambos_35_44.1[-1,]
ambos_35_44.1 = ambos_35_44.1[-1,]

#Extraccion de una subtabla que contenga de 45 a 54 años  de ambos sexos 
ambos_45_54.1 <- as.data.frame( activos_valores_Abs[2:23,c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_45_54.1) = ambos_45_54.1[1,]
ambos_45_54.1 = ambos_45_54.1[-1,]
ambos_45_54.1 = ambos_45_54.1[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de ambos sexos 
ambos_55_mas.1 <- as.data.frame( activos_valores_Abs[2:23,c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_55_mas.1) = ambos_55_mas.1[1,]
ambos_55_mas.1 = ambos_55_mas.1[-1,]
ambos_55_mas.1 = ambos_55_mas.1[-1,]

#HOMBRES#

#Extraccion de una subtabla que contenga el total  de hombres
hombres_total.1 <- as.data.frame( activos_valores_Abs[c(2,25:44),1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_total.1) = hombres_total.1[1,]
hombres_total.1 = hombres_total.1[-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de hombres
hombres_16_19.1 <- as.data.frame( activos_valores_Abs[c(2,25:44),c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_16_19.1) = hombres_16_19.1[1,]
hombres_16_19.1 = hombres_16_19.1[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de hombres 
hombres_20_24.1 <- as.data.frame( activos_valores_Abs[c(2,25:44),c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_20_24.1 ) = hombres_20_24.1 [1,]
hombres_20_24.1  = hombres_20_24.1 [-1,]

#Extraccion de una subtabla que contenga de 25 a 34 años  de hombres 
hombres_25_34.1 <- as.data.frame( activos_valores_Abs[c(2,25:44),c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_25_34.1) = hombres_25_34.1[1,]
hombres_25_34.1 = hombres_25_34.1[-1,]

#Extraccion de una subtabla que contenga de 35 a 44 años  de hombres 
hombres_35_44.1 <- as.data.frame( activos_valores_Abs[c(2,25:44),c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_35_44.1) = hombres_35_44.1[1,]
hombres_35_44.1 = hombres_35_44.1[-1,]

#Extraccion de una subtabla que contenga de 45 a 54 años  de hombres 
hombres_45_54.1 <- as.data.frame( activos_valores_Abs[c(2,25:44),c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_45_54.1) = hombres_45_54.1[1,]
hombres_45_54.1 = hombres_45_54.1[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de hombres 
hombres_55_mas.1 <- as.data.frame( activos_valores_Abs[c(2,25:44),c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_55_mas.1) = hombres_55_mas.1[1,]
hombres_55_mas.1 = hombres_55_mas.1[-1,]

#MUJERES#

#Extraccion de una subtabla que contenga el total  de mujeres
mujeres_total.1 <- as.data.frame( activos_valores_Abs[c(2,46:65),1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_total.1) = mujeres_total.1[1,]
mujeres_total.1 = mujeres_total.1[-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de mujeres
mujeres_16_19.1 <- as.data.frame( activos_valores_Abs[c(2,46:65),c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_16_19.1) = mujeres_16_19.1[1,]
mujeres_16_19.1 = mujeres_16_19.1[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de mujeres
mujeres_20_24.1 <- as.data.frame( activos_valores_Abs[c(2,46:65),c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_20_24.1 ) = mujeres_20_24.1 [1,]
mujeres_20_24.1  = mujeres_20_24.1 [-1,]

#Extraccion de una subtabla que contenga de 25 a 34 años  de mujeres
mujeres_25_34.1 <- as.data.frame( activos_valores_Abs[c(2,46:65),c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_25_34.1) = mujeres_25_34.1[1,]
mujeres_25_34.1 = mujeres_25_34.1[-1,]

#Extraccion de una subtabla que contenga de 35 a 44 años  de mujeres
mujeres_35_44.1 <- as.data.frame( activos_valores_Abs[c(2,46:65),c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_35_44.1) = mujeres_35_44.1[1,]
mujeres_35_44.1 = mujeres_35_44.1[-1,]

#Extraccion de una subtabla que contenga de 45 a 54 años  de mujeres 
mujeres_45_54.1 <- as.data.frame( activos_valores_Abs[c(2,46:65),c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_45_54.1) = mujeres_45_54.1[1,]
mujeres_45_54.1 = mujeres_45_54.1[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de mujeres 
mujeres_55_mas.1 <- as.data.frame( activos_valores_Abs[c(2,46:65),c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_55_mas.1) = mujeres_55_mas.1[1,]
mujeres_55_mas.1 = mujeres_55_mas.1[-1,]
```

Variable Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad
```{r}
library(readxl)
#Variable Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad
#Hay que eliminar las 6 primeras filas ya que no son nombres y datos redundantes para la tabla 

url <- "http://www.ine.es/jaxiT3/files/t/es/xlsx/4934.xlsx?nocab=1"
destfile <- "Porcentajes_respecto_total.xlsx"
curl::curl_download(url, destfile)
Activos_Porcentajes_respecto_total <- read_excel(destfile, skip = 6, col_names = FALSE)

#Renombro las columnas por numeros para poder hacer una extraccion más facil 
for ( i in 1:79)
  colnames(Activos_Porcentajes_respecto_total)[i] <- i

#AMBOS SEXOS#

#Extraccion de una subtabla que contenga el total  de ambos sexos 
ambos_total.2 <- as.data.frame( Activos_Porcentajes_respecto_total[2:23,1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_total.2) = ambos_total.2[1,]
ambos_total.2 = ambos_total.2[-1,]
ambos_total.2 = ambos_total.2[-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de ambos sexos 
ambos_16_19.2 <- as.data.frame( Activos_Porcentajes_respecto_total[2:23,c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_16_19.2) = ambos_16_19.2[1,]
ambos_16_19.2 = ambos_16_19.2[-1,]
ambos_16_19.2 = ambos_16_19.2[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de ambos sexos 
ambos_20_24.2 <- as.data.frame( Activos_Porcentajes_respecto_total[2:23,c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_20_24.2) = ambos_20_24.2[1,]
ambos_20_24.2 = ambos_20_24.2[-1,]
ambos_20_24.2 = ambos_20_24.2[-1,]

#Extraccion de una subtabla que contenga de 25 a 34 años  de ambos sexos 
ambos_25_34.2 <- as.data.frame( Activos_Porcentajes_respecto_total[2:23,c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_25_34.2) = ambos_25_34.2[1,]
ambos_25_34.2 = ambos_25_34.2[-1,]
ambos_25_34.2 = ambos_25_34.2[-1,]

#Extraccion de una subtabla que contenga de 35 a 44 años  de ambos sexos 
ambos_35_44.2 <- as.data.frame( Activos_Porcentajes_respecto_total[2:23,c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_35_44.2) = ambos_35_44.2[1,]
ambos_35_44.2 = ambos_35_44.2[-1,]
ambos_35_44.2 = ambos_35_44.2[-1,]

#Extraccion de una subtabla que contenga de 45 a 54 años  de ambos sexos 
ambos_45_54.2 <- as.data.frame( Activos_Porcentajes_respecto_total[2:23,c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_45_54.2) = ambos_45_54.2[1,]
ambos_45_54.2 = ambos_45_54.2[-1,]
ambos_45_54.2 = ambos_45_54.2[-1,]
S Z
#Extraccion de una subtabla que contenga de 55 a mas años  de ambos sexos 
ambos_55_mas.2 <- as.data.frame( Activos_Porcentajes_respecto_total[2:23,c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_55_mas.2) = ambos_55_mas.2[1,]
ambos_55_mas.2 = ambos_55_mas.2[-1,]
ambos_55_mas.2 = ambos_55_mas.2[-1,]

#HOMBRES#

#Extraccion de una subtabla que contenga el total  de hombres
hombres_total.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,25:44),1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_total.2) = hombres_total.2[1,]
hombres_total.2 = hombres_total.2[-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de hombres
hombres_16_19.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,25:44),c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_16_19.2) = hombres_16_19.2[1,]
hombres_16_19.2 = hombres_16_19.2[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de hombres 
hombres_20_24.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,25:44),c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_20_24.2 ) = hombres_20_24.2 [1,]
hombres_20_24.2  = hombres_20_24.2 [-1,]

#Extraccion de una subtabla que contenga de 25 a 34 años  de hombres 
hombres_25_34.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,25:44),c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_25_34.2) = hombres_25_34.2[1,]
hombres_25_34.2 = hombres_25_34.2[-1,]

#Extraccion de una subtabla que contenga de 35 a 44 años  de hombres 
hombres_35_44.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,25:44),c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_35_44.2) = hombres_35_44.2[1,]
hombres_35_44.2 = hombres_35_44.2[-1,]

#Extraccion de una subtabla que contenga de 45 a 54 años  de hombres 
hombres_45_54.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,25:44),c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_45_54.2) = hombres_45_54.2[1,]
hombres_45_54.2 = hombres_45_54.2[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de hombres 
hombres_55_mas.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,25:44),c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_55_mas.2) = hombres_55_mas.2[1,]
hombres_55_mas.2 = hombres_55_mas.2[-1,]

#MUJERES#

#Extraccion de una subtabla que contenga el total  de mujeres
mujeres_total.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,46:65),1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_total.2) = mujeres_total.2[1,]
mujeres_total.2 = mujeres_total.2[-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de mujeres
mujeres_16_19.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,46:65),c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_16_19.2) = mujeres_16_19.2[1,]
mujeres_16_19.2 = mujeres_16_19.2[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de mujeres
mujeres_20_24.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,46:65),c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_20_24.2 ) = mujeres_20_24.2 [1,]
mujeres_20_24.2  = mujeres_20_24.2 [-1,]

#Extraccion de una subtabla que contenga de 25 a 34 años  de mujeres
mujeres_25_34.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,46:65),c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_25_34.2) = mujeres_25_34.2[1,]
mujeres_25_34.2 = mujeres_25_34.2[-1,]

#Extraccion de una subtabla que contenga de 35 a 44 años  de mujeres
mujeres_35_44.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,46:65),c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_35_44.2) = mujeres_35_44.2[1,]
mujeres_35_44.2 = mujeres_35_44.2[-1,]

#Extraccion de una subtabla que contenga de 45 a 54 años  de mujeres 
mujeres_45_54.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,46:65),c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_45_54.2) = mujeres_45_54.2[1,]
mujeres_45_54.2 = mujeres_45_54.2[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de mujeres 
mujeres_55_mas.2 <- as.data.frame( Activos_Porcentajes_respecto_total[c(2,46:65),c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_55_mas.2) = mujeres_55_mas.2[1,]
mujeres_55_mas.2 = mujeres_55_mas.2[-1,]
```

Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma
```{r}
library(readxl)
url <- "http://www.ine.es/jaxiT3/files/t/es/xlsx/4933.xlsx?nocab=1"
destfile <- "Tasas_actividad.xlsx"
curl::curl_download(url, destfile)
Tasas_actividad <- read_excel(destfile, skip = 6, 
                              col_names = FALSE)

#Renombro las columnas por numeros para poder hacer una extraccion más facil 
for ( i in 1:79)
  colnames(Tasas_actividad)[i] <- i

#AMBOS SEXOS#

#Extraccion de una subtabla que contenga el total  de ambos sexos 
ambos_total.3 <- as.data.frame( Tasas_actividad[2:23,1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_total.3) = ambos_total.3[1,]
ambos_total.3 = ambos_total.3[-1,]
ambos_total.3 = ambos_total.3[-1,]

#Extraccion de una subtabla que contenga los menores de 25  de ambos sexos 
ambos_menos25.3 <- as.data.frame( Tasas_actividad[2:23,c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_menos25.3) = ambos_menos25.3[1,]
ambos_menos25.3 = ambos_menos25.3[-1,]
ambos_menos25.3 = ambos_menos25.3[-1,]

#Extraccion de una subtabla que contenga de 25 y mas añso  de ambos sexos 
ambos_25_mas.3 <- as.data.frame( Tasas_actividad[2:23,c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_25_mas.3) = ambos_25_mas.3[1,]
ambos_25_mas.3 = ambos_25_mas.3[-1,]
ambos_25_mas.3 = ambos_25_mas.3[-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de ambos sexos 
ambos_16_19.3 <- as.data.frame( Tasas_actividad[2:23,c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_16_19.3) = ambos_16_19.3[1,]
ambos_16_19.3 = ambos_16_19.3[-1,]
ambos_16_19.3 = ambos_16_19.3[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de ambos sexos 
ambos_20_24.3 <- as.data.frame( Tasas_actividad[2:23,c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_20_24.3) = ambos_20_24.3[1,]
ambos_20_24.3 = ambos_20_24.3[-1,]
ambos_20_24.3 = ambos_20_24.3[-1,]

#Extraccion de una subtabla que contenga de 25 a 54 años  de ambos sexos 
ambos_25_54.3 <- as.data.frame( Tasas_actividad[2:23,c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_25_54.3) = ambos_25_54.3[1,]
ambos_25_54.3 = ambos_25_54.3[-1,]
ambos_25_54.3 = ambos_25_54.3[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de ambos sexos 
ambos_55_mas.3 <- as.data.frame( Tasas_actividad[2:23,c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(ambos_55_mas.3) = ambos_55_mas.3[1,]
ambos_55_mas.3 = ambos_55_mas.3[-1,]
ambos_55_mas.3 = ambos_55_mas.3[-1,]

#HOMBRES#

#Extraccion de una subtabla que contenga el total  de hombres
hombres_total.3 <- as.data.frame( Tasas_actividad[c(2,25:44),1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_total.3) = hombres_total.3[1,]
hombres_total.3 = hombres_total.3[-1,]

#Extraccion de una subtabla que contenga menos 25  de hombres
hombres_menos_25.3 <- as.data.frame( Tasas_actividad[c(2,25:44),c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_menos_25.3) = hombres_menos_25.3[1,]
hombres_menos_25.3 = hombres_menos_25.3[-1,]

#Extraccion de una subtabla que contenga de 25 y mas añs  de hombres 
hombres_25_mas.3 <- as.data.frame( Tasas_actividad[c(2,25:44),c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_25_mas.3 ) = hombres_25_mas.3 [1,]
hombres_25_mas.3  = hombres_25_mas.3 [-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de hombres 
hombres_16_19.3 <- as.data.frame( Tasas_actividad[c(2,25:44),c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_16_19.3) = hombres_16_19.3[1,]
hombres_16_19.3 = hombres_16_19.3[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de hombres 
hombres_20_24.3 <- as.data.frame( Tasas_actividad[c(2,25:44),c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_20_24.3) = hombres_20_24.3[1,]
hombres_20_24.3 = hombres_20_24.3[-1,]

#Extraccion de una subtabla que contenga de 25 a 54 años  de hombres 
hombres_25_54.3 <- as.data.frame( Tasas_actividad[c(2,25:44),c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_25_54.3) = hombres_25_54.3[1,]
hombres_25_54.3 = hombres_25_54.3[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de hombres 
hombres_55_mas.3 <- as.data.frame( Tasas_actividad[c(2,25:44),c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(hombres_55_mas.3) = hombres_55_mas.3[1,]
hombres_55_mas.3 = hombres_55_mas.3[-1,]

#MUJERES#

#Extraccion de una subtabla que contenga el total  de mujeres
mujeres_total.3 <- as.data.frame( Tasas_actividad[c(2,46:65),1:14], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_total.3) = mujeres_total.3[1,]
mujeres_total.3 = mujeres_total.3[-1,]

#Extraccion de una subtabla que contenga de menos 25 años  de mujeres
mujeres_menos_25.3 <- as.data.frame( Tasas_actividad[c(2,46:65),c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_menos_25.3) = mujeres_menos_25.3[1,]
mujeres_menos_25.3 = mujeres_menos_25.3[-1,]

#Extraccion de una subtabla que contenga de 25 a mas años  de mujeres
mujeres_25_mas.3 <- as.data.frame( Tasas_actividad[c(2,46:65),c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_25_mas.3 ) = mujeres_25_mas.3 [1,]
mujeres_25_mas.3  = mujeres_25_mas.3 [-1,]

#Extraccion de una subtabla que contenga de 16 a 19 años  de mujeres
mujeres_16_19.3 <- as.data.frame( Tasas_actividad[c(2,46:65),c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_16_19.3) = mujeres_16_19.3[1,]
mujeres_16_19.3 = mujeres_16_19.3[-1,]

#Extraccion de una subtabla que contenga de 20 a 24 años  de mujeres
mujeres_20_24.3 <- as.data.frame( Tasas_actividad[c(2,46:65),c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_20_24.3) = mujeres_20_24.3[1,]
mujeres_20_24.3 = mujeres_20_24.3[-1,]

#Extraccion de una subtabla que contenga de 25 a 54 años  de mujeres 
mujeres_25_54.3 <- as.data.frame( Tasas_actividad[c(2,46:65),c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_25_54.3) = mujeres_25_54.3[1,]
mujeres_20_24.3 = mujeres_25_54.3[-1,]

#Extraccion de una subtabla que contenga de 55 a mas años  de mujeres 
mujeres_55_mas.3 <- as.data.frame( Tasas_actividad[c(2,46:65),c(1,80:92)], drop=false)
#cambiamos el nombre de las columnas a los años 
colnames(mujeres_55_mas.3) = mujeres_55_mas.3[1,]
mujeres_55_mas.3 = mujeres_55_mas.3[-1,]

```

Variable Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma
```{r}
# Variable Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma
url <- "http://www.ine.es/jaxiT3/files/t/es/xlsx/4935.xlsx?nocab=1"
destfile <- "activos_va.xlsx"
curl::curl_download(url, destfile)
activos_VA <- read_excel(destfile, skip = 6,
                         col_names = FALSE)
#Renombro las columnas por numeros para poder hacer una extraccion
for ( i in 1:79)
  colnames(activos_VA)[i] <- i

#Extraccion de una subtabla que contenga el total de ambos sexos
total_ambos.4 <- as.data.frame( activos_VA[3:23,1:14], drop=false)
#cambiamos el nombre de las columnas a los años
colnames(total_ambos.4) = total_ambos.4[1,]
total_ambos.4 = total_ambos.4[-1,]

#Extraccion de una subtabla que contenga el total de hombres
total_hombres.4 <- as.data.frame( activos_VA[3:23,c(1,15:27)], drop=false)
#cambiamos el nombre de las columnas a los años
colnames(total_hombres.4) = total_hombres.4[1,]
total_hombres.4 = total_hombres.4[-1,]

#Extraccion de una subtabla que contenga el total mujeres
total_mujeres.4 <- as.data.frame( activos_VA[3:23,c(1,28:40)], drop=false)
#cambiamos el nombre de las columnas a los años
colnames(total_mujeres.4) = total_mujeres.4[1,]
total_mujeres.4 = total_mujeres.4[-1,]

#Extraccion de una subtabla que contenga de 16 a 64 ambos sexos
ambos_16_64.4 <- as.data.frame( activos_VA[3:23,c(1,41:53)], drop=false)
#cambiamos el nombre de las columnas a los años
colnames(ambos_16_64.4) = ambos_16_64.4[1,]
ambos_16_64.4 = ambos_16_64.4[-1,]

#Extraccion de una subtabla que contenga de 16 a 64  hombres
hombres_16_64.4 <- as.data.frame( activos_VA[3:23,c(1,54:66)], drop=false)
#cambiamos el nombre de las columnas a los años
colnames(hombres_16_64.4) = hombres_16_64.4[1,]
hombres_16_64.4 = hombres_16_64.4[-1,]

#Extraccion de una subtabla que contenga de 16 a 64 mujeres
mujeres_16_64.4 <- as.data.frame( activos_VA[3:23,c(1,67:79)], drop=false)
#cambiamos el nombre de las columnas a los años
colnames(mujeres_16_64.4) = mujeres_16_64.4[1,]
mujeres_16_64.4 = mujeres_16_64.4[-1,]
```


Shiny
```{r}
library(shiny)
library(readxl)
library(shiny)
library(dygraphs)
library(plotly)
# Define UI for application that draws a histogram
ui <- navbarPage(title= "Variables",
                 tabPanel("1. Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("com1","Comunidades Autónomas:",choices = c("Total Nacional"=1,
                                                                                     "Andalucía"=2,
                                                                                     "Aragón"=3,
                                                                                     "Principado de Asturias"=4, 
                                                                                     "Islas Baleares"=5,
                                                                                     "Canarias"=6,
                                                                                     "Cantabria"=7,
                                                                                     "Castilla y León"=8, 
                                                                                     "Castilla-La Mancha"=9, 
                                                                                     "Cataluña"=10,
                                                                                     "Comunidad Valenciana"=11,
                                                                                     "Extremadura"=12,
                                                                                     "Galicia"=13, 
                                                                                     "Comunidad de Madrid"=14,
                                                                                     "Región de Murcia"=15, 
                                                                                     "Comunidad Foral de Navarra"=16, 
                                                                                     "Pais Vasco"=17 , 
                                                                                     "La Rioja"=18, 
                                                                                     "Ceuta"=19,
                                                                                     "Melilla"=20 )),
                              
                              selectInput("edad1","Edades:",choices = c("Total"=1,"De 16 a 19 años"=2,"De 20 a 24 años"=3,
                                                                          "De 25 a 34 años"=4,"De 35 a 44 años"=5,"
                                                                         De 45 a 55 años"= 6,"De 55 a más años"=7)),
                              radioButtons("sexo1","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres"))
                            ),
                            
                            
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Grafico", plotlyOutput("graf1")),
                                tabPanel("Datos", tableOutput("dat1"))
                              )
                            )
                          )),
                 
                 tabPanel("2. Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("com2","Comunidades Autónomas:",choices = c("Total Nacional"=1,
                                                                                     "Andalucía"=2,
                                                                                     "Aragón"=3,
                                                                                     "Principado de Asturias"=4, 
                                                                                     "Islas Baleares"=5,
                                                                                     "Canarias"=6,
                                                                                     "Cantabria"=7,
                                                                                     "Castilla y León"=8, 
                                                                                     "Castilla-La Mancha"=9, 
                                                                                     "Cataluña"=10,
                                                                                     "Comunidad Valenciana"=11,
                                                                                     "Extremadura"=12,
                                                                                     "Galicia"=13, 
                                                                                     "Comunidad de Madrid"=14,
                                                                                     "Región de Murcia"=15, 
                                                                                     "Comunidad Foral de Navarra"=16, 
                                                                                     "Pais Vasco"=17 , 
                                                                                     "La Rioja"=18, 
                                                                                     "Ceuta"=19,
                                                                                     "Melilla"=20 )),
                              
                              selectInput("edad2","Edades:",choices = c("De 16 a 19 años"=2,"De 20 a 24 años"=3,"De 25 a 34 años"=4,"De 35 a 44 años"=5,"
                                               De 45 a 55 años"= 6,"De 55 a más años"=7)),
                              radioButtons("sexo2","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres"))
                            ),
                            
                            
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Grafico", plotlyOutput("graf2")),
                                tabPanel("Datos", tableOutput("dat2"))
                              )
                            )
                          )),
                 
                 tabPanel("3. Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("com3","Comunidades Autónomas:",choices = c("Total Nacional"=1,
                                                                                     "Andalucía"=2,
                                                                                     "Aragón"=3,
                                                                                     "Principado de Asturias"=4, 
                                                                                     "Islas Baleares"=5,
                                                                                     "Canarias"=6,
                                                                                     "Cantabria"=7,
                                                                                     "Castilla y León"=8, 
                                                                                     "Castilla-La Mancha"=9, 
                                                                                     "Cataluña"=10,
                                                                                     "Comunidad Valenciana"=11,
                                                                                     "Extremadura"=12,
                                                                                     "Galicia"=13, 
                                                                                     "Comunidad de Madrid"=14,
                                                                                     "Región de Murcia"=15, 
                                                                                     "Comunidad Foral de Navarra"=16, 
                                                                                     "Pais Vasco"=17 , 
                                                                                     "La Rioja"=18, 
                                                                                     "Ceuta"=19,
                                                                                     "Melilla"=20 )),
                              
                              selectInput("edad3","Edades:",choices = c("Total"=1,"Menos de 25 años"=6, "Más de 25 años"=7, "De 16 a 19 años"=2,"De 20 a 24 años"=3,
                                                                       "De 25 a 54 años"=4,"De 55 a más años"=5 )),
                              radioButtons("sexo3","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres"))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Grafico", plotlyOutput("graf3")),
                                tabPanel("Datos", tableOutput("dat3"))
                              )
                            )
                          )),
                 
                 tabPanel("4. Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma",
                          sidebarPanel(
                            selectInput("com4","Comunidades Autónomas:",choices = c("Total Nacional"=1,
                                                                                    "Andalucía"=2,
                                                                                    "Aragón"=3,
                                                                                    "Principado de Asturias"=4, 
                                                                                    "Islas Baleares"=5,
                                                                                    "Canarias"=6,
                                                                                    "Cantabria"=7,
                                                                                    "Castilla y León"=8, 
                                                                                    "Castilla-La Mancha"=9, 
                                                                                    "Cataluña"=10,
                                                                                    "Comunidad Valenciana"=11,
                                                                                    "Extremadura"=12, 
                                                                                    "Galicia"=13, 
                                                                                    "Comunidad de Madrid"=14,
                                                                                    "Región de Murcia"=15, 
                                                                                    "Comunidad Foral de Navarra"=16, 
                                                                                    "Pais Vasco"=17 ,
                                                                                    "La Rioja"=18,
                                                                                    "Ceuta"=19,
                                                                                    "Melilla"=20 )),
                            
                            radioButtons("sexo4","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres")),
                            radioButtons("edad4","Edades:",choices = c("Total","De 16 a 64 años"))
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Grafico", plotlyOutput("graf4")),
                              tabPanel("Datos", tableOutput("dat4"))
                            )
                          )
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$com <-{(
    renderText(input$com)
  )}
  
  output$sexo <-{(
    renderText(input$sexo)
  )}
  output$edad <-{(
    renderText(input$edad)
  )}
  
  tabla1<-reactive({
    #ambos sexos 
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Ambos sexos") && (input$edad1 == 1)){
        return( as.data.frame( ambos_total.1[i,14:2], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Ambos sexos") && (input$edad1 == 2)){
        return( as.data.frame( ambos_16_19.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Ambos sexos") && (input$edad1 == 3)){
        return( as.data.frame( ambos_20_24.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Ambos sexos") && (input$edad1 == 4)){
        return( as.data.frame( ambos_25_34.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Ambos sexos") && (input$edad1 == 5)){
        return( as.data.frame( ambos_35_44.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Ambos sexos") && (input$edad1 == 6)){
        return( as.data.frame( ambos_45_54.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Ambos sexos") && (input$edad1 == 7)){
        return( as.data.frame( ambos_55_mas.1[i,14:2], drop=false))
      }
    }
    
    #Hombres
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Hombres") && (input$edad1 == 1)){
        return( as.data.frame( hombres_total.1[i,14:2], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Hombres") && (input$edad1 == 2)){
        return( as.data.frame( hombres_16_19.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Hombres") && (input$edad1 == 3)){
        return( as.data.frame( hombres_20_24.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Hombres") && (input$edad1 == 4)){
        return( as.data.frame( hombres_25_34.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Hombres") && (input$edad1 == 5)){
        return( as.data.frame( hombres_35_44.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Hombres") && (input$edad1 == 6)){
        return( as.data.frame( hombres_45_54.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Hombres") && (input$edad1 == 7)){
        return( as.data.frame( hombres_55_mas.1[i,14:2], drop=false))
      }
    }
    
    #MUJERES
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Mujeres") && (input$edad1 == 1)){
        return( as.data.frame( mujeres_total.1[i,14:2], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Mujeres") && (input$edad1 == 2)){
        return( as.data.frame( mujeres_16_19.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Mujeres") && (input$edad1 == 3)){
        return( as.data.frame( mujeres_20_24.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Mujeres") && (input$edad1 == 4)){
        return( as.data.frame( mujeres_25_34.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Mujeres") && (input$edad1 == 5)){
        return( as.data.frame( mujeres_35_44.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Mujeres") && (input$edad1 == 6)){
        return( as.data.frame( mujeres_45_54.1[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com1 == i)&& (input$sexo1== "Mujeres") && (input$edad1 == 7)){
        return( as.data.frame( mujeres_55_mas.1[i,14:2], drop=false))
      }
    }
    
  })
  output$graf1 <- renderPlotly({
    
    
    
    activos1 <-t(tabla1())
    año1<- rownames(activos1)
    df1 <- data.frame(año1,
                      activos1)
    ggplot(data=df1, aes(x=año1, y=activos1, group=1)) +
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Valores absolutos ")+
      theme_classic()
    
  })
  output$dat1 <- renderTable({
    tabla1()
  })
  
  tabla2<- reactive({
    #ambos sexos 
    
    
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Ambos sexos") && (input$edad2 == 2)){
        return( as.data.frame( ambos_16_19.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Ambos sexos") && (input$edad2 == 3)){
        return( as.data.frame( ambos_20_24.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Ambos sexos") && (input$edad2 == 4)){
        return( as.data.frame( ambos_25_34.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Ambos sexos") && (input$edad2 == 5)){
        return( as.data.frame( ambos_35_44.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Ambos sexos") && (input$edad2 == 6)){
        return( as.data.frame( ambos_45_54.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Ambos sexos") && (input$edad2 == 7)){
        return( as.data.frame( ambos_55_mas.2[i,14:2], drop=false))
      }
    }
    
    #Hombres
    
    
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Hombres") && (input$edad2 == 2)){
        return( as.data.frame( hombres_16_19.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Hombres") && (input$edad2 == 3)){
        return( as.data.frame( hombres_20_24.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Hombres") && (input$edad2 == 4)){
        return( as.data.frame( hombres_25_34.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Hombres") && (input$edad2 == 5)){
        return( as.data.frame( hombres_35_44.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Hombres") && (input$edad2 == 6)){
        return( as.data.frame( hombres_45_54.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Hombres") && (input$edad2 == 7)){
        return( as.data.frame( hombres_55_mas.2[i,14:2], drop=false))
      }
    }
    
    #MUJERES
    
    
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Mujeres") && (input$edad2 == 2)){
        return( as.data.frame( mujeres_16_19.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Mujeres") && (input$edad2 == 3)){
        return( as.data.frame( mujeres_20_24.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Mujeres") && (input$edad2 == 4)){
        return( as.data.frame( mujeres_25_34.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Mujeres") && (input$edad2 == 5)){
        return( as.data.frame( mujeres_35_44.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Mujeres") && (input$edad2 == 6)){
        return( as.data.frame( mujeres_45_54.2[i,14:2], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com2 == i)&& (input$sexo2== "Mujeres") && (input$edad2 == 7)){
        return( as.data.frame( mujeres_55_mas.2[i,14:2], drop=false))
      }
    }
  })
  
  output$graf2 <- renderPlotly({
    
    activos2 <-t(tabla2())
    año2<- rownames(activos2)
    df2 <- data.frame(año2,
                      activos2)
    ggplot(data=df2, aes(x=año2, y=activos2, group=1)) +
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Porcentaje respecto al total de cada comunidad ")+
      theme_classic()
    
    
  })
  
  output$dat2 <- renderTable({
    tabla2()
  })
  
  tabla3<-reactive({
     #ambos sexos 
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Ambos sexos") && (input$edad3 == 1)){
        return( as.data.frame( ambos_total.3[i,2:14], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Ambos sexos") && (input$edad3 == 2)){
        return( as.data.frame( ambos_16_19.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Ambos sexos") && (input$edad3 == 3)){
        return( as.data.frame( ambos_20_24.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Ambos sexos") && (input$edad3 == 4)){
        return( as.data.frame( ambos_25_54.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Ambos sexos") && (input$edad3 == 5)){
        return( as.data.frame( ambos_55_mas.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Ambos sexos") && (input$edad3 == 6)){
        return( as.data.frame( ambos_menos25.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Ambos sexos") && (input$edad3 == 7)){
        return( as.data.frame( ambos_25_mas.3[i,2:14], drop=false))
      }
    }
    
    #Hombres
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Hombres") && (input$edad3 == 1)){
        return( as.data.frame( hombres_total.3[i,2:14], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Hombres") && (input$edad3 == 2)){
        return( as.data.frame( hombres_16_19.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Hombres") && (input$edad3 == 3)){
        return( as.data.frame( hombres_20_24.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Hombres") && (input$edad3 == 4)){
        return( as.data.frame( hombres_25_54.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Hombres") && (input$edad3 == 5)){
        return( as.data.frame( hombres_55_mas.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Hombres") && (input$edad3 == 6)){
        return( as.data.frame( hombres_menos_25.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Hombres") && (input$edad3 == 7)){
        return( as.data.frame( hombres_25_mas.3[i,2:14], drop=false))
      }
    }
    
    #MUJERES
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Mujeres") && (input$edad3 == 1)){
        return( as.data.frame( mujeres_total.3[i,2:14], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Mujeres") && (input$edad3 == 2)){
        return( as.data.frame( mujeres_16_19.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Mujeres") && (input$edad3 == 3)){
        return( as.data.frame( mujeres_20_24.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Mujeres") && (input$edad3 == 4)){
        return( as.data.frame( mujeres_25_54.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Mujeres") && (input$edad3 == 5)){
        return( as.data.frame( mujeres_55_mas.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Mujeres") && (input$edad3 == 6)){
        return( as.data.frame( mujeres_menos_25.3[i,2:14], drop=false))
      }
    }
    for (i in 1:20){
      if ((input$com3 == i)&& (input$sexo3== "Mujeres") && (input$edad3 == 7)){
        return( as.data.frame( mujeres_25_mas.3[i,2:14], drop=false))
      }
    }
    
  })
  output$graf3 <- renderPlotly({
    
    activos3 <-t(tabla3())
    año3<- rownames(activos3)
    df3 <- data.frame(año3,
                     activos3)
    ggplot(data=df3, aes(x=año3, y=activos3, group=1)) +
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Valores absolutos ")+
      theme_classic()
    
    
  })
  output$dat3 <- renderTable({
    tabla3()
  })
  
  
  tabla4<- reactive({
    #edades totales
    for (i in 1:20){
      if ((input$com4 == i)&& (input$sexo4== "Ambos sexos") && (input$edad4 == "Total")){
        return( as.data.frame( total_ambos.4[i,14:2], drop=false))
        
      }
    }
    
    for (i in 1:20){
      if ((input$com4 == i)&& (input$sexo4== "Hombres") && (input$edad4 == "Total")){
        return( as.data.frame( total_hombres.4[i,14:2], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com4 == i)&& (input$sexo4== "Mujeres") && (input$edad4 == "Total")){
        return(as.data.frame( total_mujeres.4[i,14:2], drop=false))
      }
    }
    
    #de 16 a 64 años
    
    for (i in 1:20){
      if ((input$com4 == i)&& (input$sexo4== "Ambos sexos") && (input$edad4 == "De 16 a 64 años")){
        return( as.data.frame( ambos_16_64.4[i,14:2], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com4 == i)&& (input$sexo4== "Hombres") && (input$edad4 == "De 16 a 64 años")){
        return (as.data.frame( hombres_16_64.4[i,14:2], drop=false))
      }
    }
    
    for (i in 1:20){
      if ((input$com4 == i)&& (input$sexo4== "Mujeres") && (input$edad4 == "De 16 a 64 años")){
        return( as.data.frame( mujeres_16_64.4[i,14:2], drop=false))
      }
    }
    
    
  })
  output$graf4 <- renderPlotly({
    
    activos4 <-t(tabla4())
    año4<- rownames(activos4)
    df4 <- data.frame(año4,
                      activos4)
    ggplot(data=df4, aes(x=año4, y=activos4, group=1)) +
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Tasas de actividad ")+
      theme_classic()
    
    
  })
  
  output$dat4 <- renderTable({
    tabla4()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

```




