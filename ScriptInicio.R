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

#Convertimos la columna "date" en variables de la classe "date"
Vuelos = Vuelos %>% mutate(date = as.date(date))

### Finalmente calculamos la columna fly_speed
Vuelos = Vuelos %>% mutate(fly_speed = distance / air_time)
