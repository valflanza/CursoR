# Esto es un comentario
library(nycflights13)
library(tidyverse)

# Vamos a probar que funciona bien
flights
View(flights)


# Empazamos a usar select y filter (ESTA NO ES LA FORMA BUENA DE HACERLO)

filter(select(flights,origin,dest),origin == "EWR")

# Vamos a utilizar el piping %>%

flights %>% select(origin,dest) %>% filter(origin == "EWR")

# Vamos a filtar todos los vuelos con una distancia mayor de 
# 1000 y selecionamos "Carrier","Origin","Distancia"

flights %>% filter(distance > 1000) %>% select(carrier,origin,distance)

# Vamos filtrar los vuelos que tuvieron un retraso mayor de 10 minutos
# en la salida y seleccionamos 

flights %>% filter(dep_delay > 10) %>% select(dep_delay,month, day, hour)

# Vamos a filtrar todos los vuelos de la compañia "UA" y que ademas 
# recorran mas 1000

flights %>% filter(carrier == "UA") %>% filter(distance > 1000)
flights %>% filter(carrier == "UA", distance >100)

# La funcion arrange() ordena los datos
flights %>%
  filter(carrier == "UA") %>% filter(distance > 1000) %>%
  select(origin,carrier,distance,dest) %>%
  arrange(desc(distance))

# Vamos a filtar los vuelos que volaron a las 12, vamos a 
# seleccionar el origen, destino, retraso en salida y los ordamos por 
# orden ascendente del retraso

flights %>%
  filter(dep_time == 1200) %>% select(origin,dest, dep_delay,dep_time,hour) %>%
  arrange(dep_delay)

flights %>%
  filter(dep_time > 1200) %>% filter(dep_time <1300) %>% select(origin,dest, dep_delay,dep_time,hour) %>%
  arrange(dep_delay)

# Uso de top_n()

flights %>% arrange(desc(air_time)) %>%
  top_n(100,air_time) %>%
  select(origin,dest,air_time)

# Uso de group_by

flights %>% group_by(origin) %>% summarise(Nvuelos = n())

# Vamos calcular el numero vuelos de los aeropuertos de destino

flights %>% group_by(dest) %>% summarise(Nvuelos =n()) %>% arrange(desc(Nvuelos))

# Vamos calcular el retraso que acumularon los aeropuertos de 
# origen (sin contar los adelantos) 
# y usamos la funcion sum() para calcular el total 

flights %>% filter(dep_delay > 0) %>% group_by(origin) %>%
  summarise(RetrasoTotal = sum(dep_delay))

# Vamos calcular el retraso que acumularon, media de retraso y 
# el numero total de vuelos los aeropuertos de 
# origen (sin contar los adelantos) 
# y usamos la funcion sum() para calcular el total y mean() para la media

flights %>% filter(dep_delay > 0) %>% group_by(origin) %>%
  summarise(RetrasoTotal = sum(dep_delay), MediaRetrato = mean(dep_delay), Nvuelos = n())

# Voy a calcular por compañia y aeropuerto cuales son los que mas retraso acumulan

flights %>% filter(dep_delay > 0) %>%
  group_by(carrier,origin) %>% summarise(TotalRetraso = sum(dep_delay)) %>% View()

# Calcular los trayectos (origin, dest) con mas retraso

flights %>% filter(dep_delay > 0) %>%
  group_by(origin, dest) %>% summarise(TotalRetraso = sum(dep_delay), MediaRetraso = mean(dep_delay)) %>%
  arrange(desc(MediaRetraso))

# Vamos a crear una nueva columna a partir de columnas ya existentes.
# Vamos a crear una columna que va a ser el ratio que hay entre el 
# tiempo de retraso y la distancia del vuelo

flights %>% mutate(RatioDelayDistancia = dep_delay / distance) %>% View()

# Vamos a calcular la velocidad del vuelo (distancia / tiempo)

flights %>% mutate(Velocidad = distance / air_time) %>% View()

# Como guardar los resultados
Vuelos = flights

CompañiaOrigen = flights %>% filter(dep_delay > 0) %>%
  group_by(carrier,origin) %>% summarise(TotalRetraso = sum(dep_delay)) %>% View()

Vuelos= Vuelos %>% mutate(Velocidad = distance / air_time) 

# Vamos a usar el mutate junto con group_by

flights %>% group_by(origin) %>% summarise(TotalVuelosOrigen = n())

flights %>%
  group_by(origin) %>% mutate(TotalVuelosOrigen = n()) %>%
  group_by(dest) %>% mutate(TotalVuelosDestino = n()) %>% View()

# Vamos a calcular la velocidad media y la media del retraso
# del los trayectos (origen y destino) y lo guardamos en dos nuevas columnas

flights  %>% filter(!is.na(air_time)) %>%
  group_by(origin,dest) %>% 
  summarise(VelocidadMedia = mean(distance/air_time))


flights %>% group_by(origin,dest) %>%
  mutate(VelocidadMedia = mean(distance/air_time, na.rm = TRUE)) %>% 
  group_by(origin) %>% mutate(MediaRetraso = mean(dep_delay, na.rm = TRUE)) %>% View()

# Vamos a usar el xxx_join


### extra: calcular el numero total de Aeropuertos de destino del la tabla flights

flights %>% group_by(dest) %>% summarise(Ntotal = n()) %>% count()

left_join(flights, airports %>% rename(dest = faa) %>% select(dest,name)) %>% View()

## Hacer un join que junte SOLO los aeropuertos comunes a las tablas flights y airports

inner_join(flights, airports %>% rename(dest = faa) %>% select(dest,name)) %>% View()


# Juntar las tablas planes y flights

TablaCompleta = inner_join(flights %>% select(-year),planes) 









