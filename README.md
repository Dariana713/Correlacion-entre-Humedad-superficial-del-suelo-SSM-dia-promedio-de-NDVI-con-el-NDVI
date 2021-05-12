# Correlacion-entre-Humedad-superficial-del-suelo-SSM-dia-promedio-de-NDVI-con-el-NDVI
Se muestra la correlación del dia promedios de humada en base al dia de NDVI, y se realiza la correlación con el NDVI, para conocer como responde la vegetación a la humedad del suelo


## CORRELACIÓN SSM_NDVI

###### Cargar librería raster 
library(raster)
library(maps)
library(dichromat)
library(spatialEco)
library(mapview)
library(rasterVis)
library(RColorBrewer)
###### Kmeans 
library("raster")  
library("cluster")
library("randomForest")
###### Regresión lineal
library(moonBook)
library(ggiraphExtra)
library(devtools)

#### VARIABLE TOPOGRAFICA

#KmeasnTopo muestra los 3 grupos topograficos (GADM, la base de datos de áreas administrativas globales)
kmeansTopo <- raster("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Georfometria R_SAGA GIS/4 ATRIBUTOS TOPOGRAFICOS/kmeans_3gruposTopograficos_bueno.tif")
limn <- getData('GADM', country='ESP', level=2)
Km_Topografic <- mask(kmeansTopo,limn)
plot(Km_Topografic)

#### MES DE ENERO

###### ubicación del directorio de trabajo
setwd("~/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Vegetación/NDVI_1km_v2_2020/1_Enero_NDVI_1km_v2_2020")

###### Unificar archivos
s <- stack(list.files(pattern = ".nc"))
plot(extent(s))

###### Colocar el mapa mundial
maps:: map("world", add =TRUE)

###### Extención del area de investigación
ex <- extent(-10.13393, 6.169643, 35.48214, 43.91964)

###### Stack de la extención y los archivos "nc"
ESP_NDVI_E <- stack(crop(s, ex))
plot(ESP_NDVI_E)

![image](https://user-images.githubusercontent.com/78845785/117954626-7d230980-b317-11eb-9c34-f0f0fabb7267.png)

##### Guardar en formato tif cada dia de vegetación 
dia1eneroNDVI_2020 <-ESP_NDVI_E[[1]]
plot(dia1eneroNDVI_2020)
writeRaster(dia1eneroNDVI_2020, file="Dia1_enero2020ndvi.tif")

#Guardar en formato tif cada dia de vegetación 
dia11eneroNDVI_2020 <-ESP_NDVI_E[[2]]
plot(dia11eneroNDVI_2020 )
writeRaster(dia11eneroNDVI_2020, file="Dia11_enero2020ndvi.tif")

#Guardar en formato tif cada dia de vegetación 
dia21eneroNDVI_2020 <-ESP_NDVI_E[[3]]
plot(dia21eneroNDVI_2020)
writeRaster(dia21eneroNDVI_2020, file="Dia21_enero2020ndvi.tif")

##### Guardar las tres imagenes en un solo formato tif (cada imagen representa un dia 1=1, 2=11, 3=21)
writeRaster(ESP_NDVI_E, file="ESP_NDVI_E.tif")


### DATOS de SSM promedio ENERO

SSM_media_enero <- stack(list.files("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Humedad/SSM_Bir  2020/1_Enero_SSM_Bir_2020/Imagenes correctas", full.names = TRUE))

## METODO ESTADISTICO DE CORRELACIÓN

###### Primero se Ajustan los datos para el mismo mes (cambio de tamaño de pixel a uno mismo), es la misma capa Ajuste_NDVIenero, para que quede en el mismo tamaño ssm'ngb' este metodo se refiere que ajusta a los vecinos cercanos 
Ajuste_NDVI_Enero <- resample(ESP_NDVI_E, SSM_media_enero, method='ngb')
plot(Ajuste_NDVI_Enero)

#### Se puede calcular la mediana o la media ( ya que estamos colocando en un solo raster los tres dias de SSSM y NDVI del mes, ya que hacerlos diarios hace que la compu trabaje mas lento pero se puede hacer diario, ¡Opinion!)

###### Mediana de los 3 dias de enero NDVI
medianaNDVIenero <- calc(Ajuste_NDVI_Enero, median)
writeRaster(medianaNDVIenero, file="medianaNDVIenero.tif")


###### Mediana de los 3 dias de enero SSM
medianaSSMenero <- calc(SSM_media_enero, median)
writeRaster(medianaSSMenero, file="medianaSSMenero.tif")

###### Desviación estandar de los 3 dias de enero NDVI
SDNDVIenero <- calc(Ajuste_NDVI_Enero, sd)
writeRaster(SDNDVIenero, file="DesviacionstandarNDVIenero.tif")

###### Desviación estandar de los 3 dias de enero SSM
SDSSMenero <- calc(SSM_media_enero, sd)
writeRaster(SDSSMenero, file="DesviacionstandarSSMenero.tif")

##### Raster correlation (s es el tamaño de la ventana móvil para la correlación, x=SSM; y=NDVI)
###### se utiliza una ventana movil de 9, ya que si se usa una mas pequeña, la resolucion es menor y se pierde mucha información.

correlaciónSSMNDVIenero <-rasterCorrelation(medianaSSMenero, medianaNDVIenero,  s = 9, type = "pearson")
###### Eliminar los infinitos
correlaciónSSMNDVIenero[!is.finite(correlaciónSSMNDVIenero)] <- NA

plot(correlaciónSSMNDVIenero)

![image](https://user-images.githubusercontent.com/78845785/117965529-26bbc800-b323-11eb-92e9-ed4f66987065.png)


mapview(correlaciónSSMNDVIenero)
![image](https://user-images.githubusercontent.com/78845785/117965684-579bfd00-b323-11eb-9fb3-5fd33bca54ea.png)

![image](https://user-images.githubusercontent.com/78845785/117965844-874b0500-b323-11eb-8025-deb88bf0f86b.png)

writeRaster(correlaciónSSMNDVIenero, file='correlacioneneroSSMNDVIs9.tif')


### Union de las tres variables para formar el cubo de información, y luego elaborar la matriz de correlación 

##### NDVI Proyectada
medianNDVI <- mask(medianaNDVIeneroc,limn)
NDVI <- projectRaster(medianNDVI, Km_Topografic)

##### Humedad proyectada
medianSSM <- mask(medianaSSMeneroc,limn)
ssm <- projectRaster(medianSSM,Km_Topografic)

#Unificación 3 variables hace falta incluir las coberturas vegetales.

Unión3variable <- stack(NDVI, ssm, Km_Topografic)
plot(Unión3variable)

![image](https://user-images.githubusercontent.com/78845785/117966069-caa57380-b323-11eb-9447-28063e895a79.png)

df <- na.omit(as.data.frame(Unión3variable, xy=TRUE))

![image](https://user-images.githubusercontent.com/78845785/117966366-1e17c180-b324-11eb-8972-abeb039c2a09.png)


