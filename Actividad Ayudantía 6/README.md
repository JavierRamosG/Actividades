Actividad Ayudantía 6 - Javier Ramos
================

## Cargar datos

``` r
load("D:/UAI/Minería de datos/Actividades/Actividad Ayudantía 6/beats.RData")
```

## Librerías

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.5
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(cluster)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

## Sample de datos

``` r
data<- beats %>% slice_sample(n=10000)
```

\#Pre Procesamiento de datos

## Limpieza de datos:

Trabajar con variables relevantes

``` r
data <- data[,!(colnames(data) %in% c("artist_id", "album_id", "album_type","album_release_date", "album_release_date_precision", "analysis_url", "disc_number", "explicit", "track_href", "is_local", "track_preview_url", "track_number", "type", "track_uri", "external_urls.spotify", "album_name", "key_mode", "mode_name", "key_name"))]
```

Verificar datos NA y entidades repetidas

``` r
data[data == ""] <- NA

data_pre <- na.omit(data)

# Ahora corroboraremos si existen canciones que esten duplicadas
data_pre %>% count(duplicated(data_pre$track_name))
```

    ##   duplicated(data_pre$track_name)    n
    ## 1                           FALSE 8199
    ## 2                            TRUE 1788

``` r
# Como existen canciones repetidas realizamos la consulta para obtener los valores distintos, pero este hecho obvia que hayan canciones con el mismo nombre pero de distinto autores  
#data_pre %>% distinct(track_name, .keep_all = TRUE, )

# Por lo que creamos una variables que almacene si existe duplicidad en la cacion y/o en el artista
data_pre$duplicate <- duplicated(data_pre[,c("track_name", "artist_name")])

# Generamos un sub data frame que almacenara solo los valores que haya obtenido el valor TRUE a la consulta anterior y los ordenamos por album_release_year
data_dupli <- data_pre %>% 
  filter(data_pre$duplicate == TRUE) %>% 
  arrange("track_name", "album_release_year", desc(album_release_year))

# Seleciono las filas que sean distintas, borro todas las canciones que se repite
data_dupli <- data_dupli %>% 
  distinct(track_name, artist_name, .keep_all = TRUE)

# Elimino de mi data pre procesada los datos que dieron positivo a la duplicidad, para que al momento de re insertar los datos sobrevivieron a la limpieza de duplicidad no se genere la duplicidad que se estaba evitando
data_pre <- data_pre[!(data_pre$duplicate == TRUE),]

# Junto la data pre procesada con los datos que sobrevivieron a la limpieza de duplicidad
data_pre <- rbind(data_pre, data_dupli)

# Elimino la columna que me indicaba duplicidad ya que no sera util mas adelante
data_pre$duplicate <- NULL
```

## Revisar Estructura de Datos

``` r
# Transformamos cada variables al tipo de variable que sale en el archivo .txt con la descripcion de cada una
data_pre$track_id <- as.character(data_pre$track_id)
data_pre$track_name <- as.character(data_pre$track_name)
data_pre$artist_name <- as.character(data_pre$artist_name)

data_pre$danceability <- as.double(as.character(data_pre$danceability))
data_pre$energy <- as.double(as.character(data_pre$energy))
data_pre$key <- as.double(as.character(data_pre$key))
data_pre$loudness <- as.double(as.character(data_pre$loudness))
data_pre$mode <- as.double(as.character(data_pre$mode))
data_pre$speechiness <- as.double(as.character(data_pre$speechiness)) 
data_pre$acousticness <- as.double(as.character(data_pre$acousticness))
data_pre$instrumentalness <- as.double(as.character(data_pre$instrumentalness))
data_pre$liveness <- as.double(as.character(data_pre$liveness))
data_pre$valence <- as.double(as.character(data_pre$valence))
data_pre$tempo <- as.double(as.character(data_pre$tempo))
data_pre$duration_ms <- as.double(as.character(data_pre$duration_ms))
data_pre$time_signature <- as.double(as.character(data_pre$time_signature))
data_pre$album_release_year <- as.double(as.character(data_pre$album_release_year))

# Character
data_char <- c("track_id", "track_name", "artist_name")

# Double
data_dou <- c("danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms","time_signature", "album_release_year")
# Volvemos a borrar los datos que puedan haber quedado como NA con el cambio de tipo de variable

data_pre <- na.omit(data)
```

## Separo Datos

``` r
datanum <- data_pre %>% 
  select(data_dou)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(data_dou)` instead of `data_dou` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
datachar <- data_pre %>% 
  select(data_char)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(data_char)` instead of `data_char` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

## Escalar Datos

``` r
data_sca <- sapply(datanum, scale)
#min_max_norm <- function(x) {
#    return((x - mean(x))/(max(x) - min(x)))    
#  }
#div_norm <- function(y) {
#    y/100
#  }
  
#des_norm <- function(z) {
#    return((z+min(z))*(max(z) - min(z)))
#  }
#data_scalmin <- min_max_norm(datanum)
```

# Procesamiento de los Datos

## Clustering Jerarquico

-   Matriz de Distancias

``` r
#Distancia Euclideana
d = dist(data_sca, method = "euclidean")

#Distancia Manhattan
d1 = dist(data_sca, method = "manhattan")

#Distancia Minkowski
d2 = dist(data_sca, method = "minkowski")
hist(d, main = "Histograma Distancia Euclideana")
```

![](README_files/figure-gfm/matriz%20distancia-1.png)<!-- -->

``` r
hist(d1, main = "Histograma Distancia Manhattan")
```

![](README_files/figure-gfm/matriz%20distancia-2.png)<!-- -->

``` r
hist(d2, main = "Histograma Distancia Minkowski")
```

![](README_files/figure-gfm/matriz%20distancia-3.png)<!-- -->

## Clustering Aglomerativo

Utilizando la funcion de R base hclust, aplicamos hierarchical
clustering, a partir de la matriz de distancias d (se usará d1 porque
explica mejor las variables), y utilizamos el criterio complete linkage

-   Complete Model

``` r
# Fijamos una seed para que cada vez que ejecutemos el codigo obtengamos los mismos valores y no vayan cambiado cada vez que ejecutamos el script
set.seed(369)
model_complete <- hclust(d1, method = "complete")
summary(model_complete)
```

    ##             Length Class  Mode     
    ## merge       19972  -none- numeric  
    ## height       9986  -none- numeric  
    ## order        9987  -none- numeric  
    ## labels          0  -none- NULL     
    ## method          1  -none- character
    ## call            3  -none- call     
    ## dist.method     1  -none- character

-   Ward Model

``` r
set.seed(369)
model_ward <- hclust(d1, method = "ward.D")
summary(model_ward)
```

    ##             Length Class  Mode     
    ## merge       19972  -none- numeric  
    ## height       9986  -none- numeric  
    ## order        9987  -none- numeric  
    ## labels          0  -none- NULL     
    ## method          1  -none- character
    ## call            3  -none- call     
    ## dist.method     1  -none- character

Generamos un dendrograma para visualizar la jerarquia. La libreria
‘ggdendro’ permite hacer estos diagramas en una sintaxis equivalente a
ggplot.

``` r
library("ggdendro")
ggdendrogram(model_complete, rotate = TRUE, theme_dendro = TRUE) 
```

![](README_files/figure-gfm/grafico%20dendrograma-1.png)<!-- -->

## Corte

``` r
# Determinamos un valor para h lo que nos entregara un valor distinto de k para cada h que escogamos, tambien podemos definir el k desde un inicio
groups <- cutree(model_complete, h = 25)

# Se imprimen los tamaños de cada cluster
table(groups)
```

    ## groups
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
    ## 1276 1575  435 1002 1306  589  991  280  696  135  388  215  383  224   30  236 
    ##   17   18   19   20   21   22   23   24 
    ##   53   59   42   34   34    1    2    1

``` r
# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion (tanto en data_pre y datanum)
data_pre$clust <- as.factor(groups)
datanum$clust <- as.factor(groups)

# Graficamos las observaciones agrupadas por su cluster
fviz_cluster(list(data = data_sca, cluster = groups))
```

![](README_files/figure-gfm/corte%20arbol-1.png)<!-- -->

## Caracteristicas de los clusters encontrados

``` r
datanum$clust <- as.numeric(as.character(datanum$clust))

# Generamos una tabla que almacenara los valores promedios para cada uno de los clusters encontrados lo que nos permitira caracterizar a cada uno de ellos
infoclusters <- aggregate(datanum, by=list(cluster=datanum$clust), mean)

# Borramos la columna clust ya que se repite esa informacion en la tabla generada
infoclusters$clust <- NULL

# Transformamos el tiempo de la cancion a minutos
infoclusters <- infoclusters %>% mutate(duration_min = infoclusters$duration_ms/60000)

# Borramos la columna de la duracion en milisegundoss
infoclusters$duration_ms <- NULL
infoclusters
```

    ##    cluster danceability     energy       key   loudness        mode speechiness
    ## 1        1    0.2835465 0.09687049  4.888715 -24.828323 0.987460815  0.04765165
    ## 2        2    0.4698463 0.18263599  5.446984 -20.713896 0.765079365  0.05050838
    ## 3        3    0.4561869 0.80719770  6.372414  -6.651701 0.981609195  0.07197655
    ## 4        4    0.5672675 0.70853393  5.113772  -7.660874 0.373253493  0.06431846
    ## 5        5    0.2965279 0.10807535  4.192190 -25.284717 0.954058193  0.04598185
    ## 6        6    0.6681630 0.69678081  4.662139  -7.411927 0.959252971  0.10685925
    ## 7        7    0.3017543 0.11834831  5.480323 -24.112093 0.005045409  0.04618890
    ## 8        8    0.5748571 0.41764357  6.035714 -12.121014 0.603571429  0.04470286
    ## 9        9    0.2420684 0.70383707  3.808908 -20.761509 0.883620690  0.09504871
    ## 10      10    0.3276356 0.14208926  5.192593 -20.585244 0.948148148  0.04019630
    ## 11      11    0.5138652 0.34916915  2.914948 -12.366951 0.992268041  0.05829330
    ## 12      12    0.4129981 0.74233209  6.995349  -7.979274 0.320930233  0.08981860
    ## 13      13    0.2717963 0.12680830  5.561358 -20.868225 0.373368146  0.04230836
    ## 14      14    0.2808960 0.20609844  5.825893 -23.543036 0.633928571  0.05221027
    ## 15      15    0.3032667 0.98256667  9.466667 -17.458400 0.000000000  0.53303333
    ## 16      16    0.2366280 0.62779435  7.872881 -23.597127 0.004237288  0.13197034
    ## 17      17    0.6826226 0.22925528  6.320755 -19.861208 0.641509434  0.91652830
    ## 18      18    0.2299712 0.15594949  3.542373 -19.830915 0.728813559  0.04340000
    ## 19      19    0.4290238 0.44953571  2.428571 -22.222786 0.714285714  0.34357619
    ## 20      20    0.0000000 0.24419359  4.529412 -20.867618 0.705882353  0.00000000
    ## 21      21    0.5974412 0.23293529  3.911765 -21.326059 0.705882353  0.61691176
    ## 22      22    0.0000000 0.00002030  0.000000  -8.873000 1.000000000  0.00000000
    ## 23      23    0.6185000 0.53910000 10.500000 -16.440000 0.000000000  0.64500000
    ## 24      24    0.0742000 0.02320000 11.000000 -28.524000 1.000000000  0.04930000
    ##    acousticness instrumentalness  liveness    valence     tempo time_signature
    ## 1     0.9439726     0.7075594130 0.1297053 0.19583832  88.77012       3.903605
    ## 2     0.9375401     0.6588377905 0.1488348 0.54330025 112.40658       3.669841
    ## 3     0.1177039     0.0737143109 0.4554959 0.44894759 131.86485       3.960920
    ## 4     0.1628895     0.2285920619 0.1880513 0.46287894 120.79169       3.979042
    ## 5     0.8995475     0.8273998211 0.1253155 0.23663810 114.06208       3.662328
    ## 6     0.2841479     0.0288677489 0.2130075 0.71414261 122.36683       3.976231
    ## 7     0.9294686     0.6017702949 0.1337861 0.22317487 104.08776       3.775984
    ## 8     0.5619946     0.0437377993 0.1793868 0.57391607 110.86543       3.892857
    ## 9     0.5124955     0.5392193417 0.5722691 0.08341950  99.30421       3.863506
    ## 10    0.9249556     0.7067807253 0.1348704 0.20688222 105.95986       3.837037
    ## 11    0.7006621     0.0458991931 0.2373448 0.41747500 110.93777       3.840206
    ## 12    0.2027176     0.1242871480 0.5689926 0.38819628 129.77270       3.953488
    ## 13    0.9345561     0.7153241671 0.1301050 0.12970548 105.07922       3.780679
    ## 14    0.8699296     0.6059471420 0.2106960 0.19210402 102.49679       1.000000
    ## 15    0.7772000     0.6348072633 0.6114333 0.04649000 112.62157       3.600000
    ## 16    0.4624690     0.5118983235 0.5211708 0.06417589  93.78053       3.805085
    ## 17    0.7789811     0.0532745391 0.3002283 0.55979245  97.04015       3.566038
    ## 18    0.8956576     0.7061444915 0.1484763 0.08087288  97.57675       3.796610
    ## 19    0.6471913     0.3817508329 0.3799452 0.18660238  99.82031       3.047619
    ## 20    0.8650294     0.2322462765 0.3396471 0.00000000   0.00000       0.000000
    ## 21    0.7732647     0.0003552244 0.3450382 0.43873529 106.92765       3.852941
    ## 22    0.0816000     0.6340000000 0.1120000 0.00000000   0.00000       0.000000
    ## 23    0.2894450     0.0570000000 0.1930000 0.17300000 110.28500       4.000000
    ## 24    0.8290000     0.7080000000 0.1270000 0.03270000 174.91900       4.000000
    ##    album_release_year duration_min
    ## 1            2016.765    3.7496809
    ## 2            2018.270    2.2721905
    ## 3            2005.182    4.5235230
    ## 4            2008.115    4.3563788
    ## 5            2016.868    3.9906538
    ## 6            2007.297    3.5147494
    ## 7            2016.460    3.3541291
    ## 8            1985.879    3.9146574
    ## 9            2018.938    2.8587061
    ## 10           1982.593    5.0808951
    ## 11           2011.116    3.6810794
    ## 12           2001.958    4.6375238
    ## 13           2010.248    9.8426035
    ## 14           2016.536    2.9324327
    ## 15           2020.067    3.0687500
    ## 16           2020.076    2.4922640
    ## 17           2012.717    1.5016384
    ## 18           2001.644   19.2772449
    ## 19           2018.500    2.5482548
    ## 20           2012.147    0.7352407
    ## 21           2012.529    1.7072564
    ## 22           1995.000   34.0677667
    ## 23           2000.500   41.0888833
    ## 24           2019.000   47.0442500

## Filtremos por clusters con mas datos

``` r
# 1er Cluster con mas datos
data_c1 <- data_pre %>% 
  filter(data_pre$clust == 1)
# 2do Cluster con mas datos
data_c2 <- data_pre %>% 
  filter(data_pre$clust == 4)
# 3er Cluster con mas datos
data_c3 <- data_pre %>% 
  filter(data_pre$clust == 2)
```

## Tomemos a c2

``` r
# Borramos la columna clust para escalar la datanum de c2
data_c2$clust <- NULL

# Selecciono las variables numericas, se escalan las variables y se almacenan los datos en una tabla
datanumc2 <- data_c2 %>% 
  select(data_dou) %>% 
  scale() %>% 
  as_tibble()
```

Ahora a C2 le aplicaremos un clustering divisivo

## Clustering Divisivo

``` r
# Generamos un modelo divisvo mediante la funcion diana de clusters
modelo_div <- diana(datanumc2)
# Le pedimos el coeficiente de divisivilidad al modelo
modelo_div$dc
```

    ## [1] 0.8343586

``` r
# Graficamos nuestro dendrograma divisivo
pltree(modelo_div, cex = 0.8, hang = -1.5, main = "Dendrogram of diana")
```

![](README_files/figure-gfm/clustering%20divisivo-1.png)<!-- --> \#\#
Cantidad Clusters

``` r
# Para el caso divisivo le entregaremos el numero de clusters con los que queremos agrupar nuestros datos
groupsc2 <- cutree(modelo_div, k = 5)

# Se imprimen los tamaños de cada cluster
table(groupsc2)
```

    ## groupsc2
    ##   1   2   3   4   5 
    ## 414 511  59  17   1

``` r
# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion de data_c2
data_c2$clust <- as.factor(groupsc2)

# Graficamos las observaciones agrupadas por su cluster
fviz_cluster(list(data = datanumc2, cluster = groupsc2))
```

![](README_files/figure-gfm/division%20arbol-1.png)<!-- -->

``` r
# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion de datanumc2
datanumc2$clust <- as.factor(groupsc2)
```

## Caracteristicas Clusters encontrados

``` r
datanumc2$clust <- as.numeric(as.character(datanumc2$clust))

# Generamos una tabla que almacenara los valores promedios para cada uno de los clusters encontrados lo que nos permitira caracterizar a cada uno de ellos
infoclustersc2 <- aggregate(datanumc2, by=list(cluster=datanumc2$clust), mean)

# Borramos la columna clust ya que se repite esa informacion en la tabla generada
infoclustersc2$clust <- NULL

# Transformamos el tiempo de la cancion a minutos
infoclustersc2 <- infoclustersc2 %>% mutate(duration_min = infoclustersc2$duration_ms/60000)

# Borramos la columna de la duracion en milisegundoss
infoclustersc2$duration_ms <- NULL
infoclustersc2
```

    ##   cluster danceability      energy        key   loudness       mode speechiness
    ## 1       1   -0.4333813 -0.12220298 -0.4202015 -0.1953465  0.9158139  -0.2612674
    ## 2       2    0.2402959  0.11027908  0.2584640  0.1293120 -0.7147117  -0.1539109
    ## 3       3    1.0537413  0.02454208  0.7195856  0.4976120 -0.4560994   3.1628038
    ## 4       4   -0.3693810 -0.42480656 -0.1129721 -0.6432723  0.8089361  -0.2093185
    ## 5       5    0.7373734  0.01315230  1.3533235 -3.6284507 -0.7713281   3.7661872
    ##   acousticness instrumentalness     liveness    valence      tempo
    ## 1  -0.08456184        0.4127143 -0.014525106 -0.5638373  0.2102714
    ## 2   0.05238975       -0.3091244 -0.004373758  0.3782628 -0.0933048
    ## 3   0.06006634       -0.4972863  0.215893329  0.7556545 -0.7693336
    ## 4   0.22208537        1.0058712 -0.297434321 -0.2652432  0.4222122
    ## 5   0.91807629       -0.6610335  0.567061466  0.0618752 -1.1605077
    ##   time_signature album_release_year  duration_min
    ## 1    0.164114893         0.04015324  5.314446e-06
    ## 2   -0.002823404        -0.03955520 -3.508328e-06
    ## 3    0.356719350         0.06935710 -8.042467e-06
    ## 4   -4.863735987         0.11705410  5.628743e-06
    ## 5   -4.863735987        -2.49272389 -2.860811e-05
