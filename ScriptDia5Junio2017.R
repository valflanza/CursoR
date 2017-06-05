library(nycflights13)
library(tidyverse)

### separate() convierte una columna en dos o mas columnas usando un determinado caracter como separador (sep = " "), en este caso un espacio en blanco

flights %>% separate(time_hour, c("date", "dummy"), sep = " ")

### Guardamos la nueva tabla en la variable Vuelos
Vuelos = flights %>% separate(time_hour, c("date", "dummy"), sep = " ") %>%
  select(
    month,
    day,
    dep_delay,
    arr_delay,
    carrier,
    tailnum,
    origin,
    dest,
    air_time,
    distance,
    date
  )

### Ahora añadimos el nombre de los aeropuertos a la nueva tabla transformando la columna "faa" en la columna "dest" para que coincida con la columna de la tabla Vuelos


tmp = airports %>% select(dest = faa, airport.name = name)

Vuelos = inner_join(Vuelos, tmp)

### Añadimos los campos "carrier.name" a la table Vuelos

tmp = airlines %>% select(carrier, carrier.name = name)
Vuelos = inner_join(Vuelos, tmp)

### Añadimos todos los campos de la tabla "planes" exectop la columna "speed"

tmp = planes %>% select(-speed) %>% rename(year.manuc = year)
Vuelos = inner_join(Vuelos, tmp)

### Ahora vamos a añadir los datos de la tabla "weather". En este caso la tabla weather tiene varias entradas (filas) por cada dia.
### En nuestro caso como la tabla Vuelos es por dias tenemos que resumir los datos a dias. Así que vamos a calcular la temperatura media del dia, la velocidad media del viento
### la precipitacion total del dia, la presion media y la visibilidad media.

tmp = weather %>% group_by(origin, month, day) %>% summarise(
  temp_media = mean(temp, na.rm = TRUE),
  wind_speed_media = mean(wind_speed, na.rm = TRUE),
  precip_total = sum(precip, na.rm = TRUE),
  pressure_media = mean(pressure, na.rm = TRUE),
  visib_media = mean(visib, na.rm = TRUE)
)

Vuelos = inner_join(Vuelos, tmp)

# Convertimos la columna "month" que actualmente es numerica a una columna de factores (as.factor)

Vuelos = Vuelos %>% mutate(month = as.factor(month))
# A continuacion asignamos nuevos "levels" a los factores para que en vez de numero tengamos las abreviaturias de los meses

levels(Vuelos$month) = c("JAN",
                         "FEB",
                         "MAR",
                         "ABR",
                         "MAY",
                         "JUN",
                         "JUL",
                         "AGO",
                         "SEP",
                         "OCT",
                         "NOV",
                         "DIC")


### Finalmente calculamos la columna fly_speed
Vuelos = Vuelos %>% mutate(fly_speed = distance / air_time)



### GRAFICAS ###

### HISTOGRAMAS ###

# Histograma de las distancias de los vuelos
Vuelos %>% ggplot(aes(x = distance)) + geom_histogram()

# Histogramas de la temperatura media del año
Vuelos %>% ggplot(aes(x = temp_media)) + geom_histogram(bins = 10)

# Hisograma del numero de vuelos por dia
Vuelos %>% group_by(month, day) %>% summarise(VuelosDia = n()) %>%
  ggplot(aes(x = VuelosDia)) + geom_histogram()

# Histograma de los retrasos (arr_delay) por dia con 50 "bins"
Vuelos %>% filter(arr_delay > 0) %>%
  group_by(month, day) %>%
  summarise(TotalRetraso = sum(arr_delay, rm.na = TRUE)) %>%
  ggplot(aes(x = TotalRetraso)) + geom_histogram(bins = 50)


### GRAFICAS DE DOS VARIABLES (PUNTOS Y LINEAS)
# Grafica de puntos (x,y) con los meses y vuelos del mes

Vuelos %>% group_by(month) %>% summarise(TotalVuelos = n()) %>%
  ggplot(aes(x = month, y = TotalVuelos)) + geom_point()

# Representar la temperatura media por mes en grafica de puntos

Vuelos %>% group_by(month) %>%
  summarise(TemperaturaMedia = mean(temp_media, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = TemperaturaMedia)) + geom_point()

# Añadimos ahora un capa "geom" con una linea que une los puntos. La capa geom_line requiere el argumento "group" 
# en este caso como no hay un group explicito podemos asignar un valor cualquiera, por ejemplo 1

Vuelos %>% group_by(month) %>%
  summarise(TemperaturaMedia = mean(temp_media, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = TemperaturaMedia)) + geom_point() + geom_line(aes(group =1))

# Añadimos la variable "origin" para dibujar los aeropuertos por separado
Vuelos %>% group_by(month, origin) %>%
  summarise(TemperaturaMedia = mean(temp_media, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = TemperaturaMedia)) +
  geom_point(aes(color = origin)) +
  geom_line(aes(group = origin, color = origin))

# Representar la velocidad media del viento por mes y distinguir entre aeropuertos
Vuelos %>% group_by(month, origin) %>%
  summarise(Viento = mean(wind_speed_media, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = Viento)) +
  geom_point(aes(color = origin)) +
  geom_line(aes(group = origin, color = origin))

### BOXPLOTS ###
# Vamos a dibujar un conjunto de boxplot por cada mes que ilustren la distribucion de la velocidad media del viento por dia.
# Antes de pasar los datos a ggplot debemos eliminar los dias duplicados. Para esto tenemos dos opciones

# - 1 Usar el group_by para eliminar los duplicados
Vuelos %>% filter(!is.na(wind_speed_media)) %>%
  group_by(month, day, origin) %>%
  summarise(Viento = max(wind_speed_media)) %>%
  ggplot(aes(x = month, y = Viento, fill = origin)) +
  geom_boxplot()

# - 2 Usar el comando distinc que precisamente elimina los duplicados
Vuelos %>% filter(!is.na(wind_speed_media)) %>%
  select(month, day, origin, wind_speed_media) %>%
  distinct() %>%
  ggplot(aes(x = month, y = wind_speed_media, fill = origin)) +
  geom_boxplot()
