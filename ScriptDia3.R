library(tidyverse)

setwd("~/CursoR/")

tratamientos <- read_csv("Tratamientos_habituales.csv")

tratamientos %>% 
  pivot_longer(trat_habitual_opc___1:trat_habitual_opc___99,names_to = "Variable", values_to = "Tratamiento") %>% 
  drop_na(Tratamiento) %>% 
  group_by(Tratamiento) %>% 
  summarise(Numero = n()) %>% 
  View()

tratamientos %>% 
  pivot_longer(starts_with("trat_habitual_opc"),names_to = "Variable", values_to = "Tratamiento") %>% 
  drop_na(Tratamiento) %>% 
  group_by(Tratamiento) %>% 
  summarise(Numero = n()) %>% 
  View()

comorbilidades <- read_csv("Comorbilidades.csv")

comorbilidades %>% 
  pivot_longer(starts_with("fact_riesgo_com"), names_to = "Variable",values_to = "Comorbilidad") %>% 
  drop_na(Comorbilidad) %>% select(record_id, Comorbilidad)

radiologia <- read_csv("Radiologia.csv")

patron_radio <-radiologia %>% 
  select(record_id,redcap_repeat_instance,starts_with("patron_radio")) %>% 
  pivot_longer(starts_with("patron_"), names_to = "Variable",values_to = "PatronRadiologico") %>% 
  drop_na(PatronRadiologico)

radio_tipo <- radiologia %>% 
  select(record_id, redcap_repeat_instance, starts_with("radio_tipo")) %>% 
  pivot_longer(starts_with("radio_tipo"), names_to = "Variable",values_to = "RadioTipo") %>% 
  drop_na(RadioTipo) %>% 
  select(-Variable)


### Ejercicios con Join

ingresos <- read_csv("Ingreso.csv")
outcome <- read_csv("Outcome.csv")


ingresos %>%
  select(-starts_with("redcap"),-ingreso_complete) %>%
  left_join(outcome) %>% 
  View()

ingresos %>% 
  inner_join(outcome, by = "record_id") %>% View()


###Ejercicio1 1.	Combina la tabla de ingreso con las comorbilidades calculadas anteriormente. 
##                Calcula la frecuencia total de cada comorbilidad por sexo.

comor_long <- comorbilidades %>% 
  pivot_longer(starts_with("fact_riesgo_com"), names_to = "Variable",values_to = "Comorbilidad") %>% 
  drop_na(Comorbilidad) %>% select(record_id, Comorbilidad)

ingresos %>% 
  inner_join(comor_long ,by = "record_id") %>% 
  group_by(sexo , Comorbilidad) %>% 
  summarise(Total = n()) %>% View()

#### Ejercicio2 2.	Combina la tabla de ingreso con los tratamientos habituales.
##                  Calcula la frecuencia total de cada tratamiento por sexo.

trat_long <- tratamientos %>% 
  pivot_longer(starts_with("trat_habitual_opc"), names_to = "Variable", values_to = "Tratamiento") %>% 
  drop_na(Tratamiento)

ingresos %>% 
  inner_join(trat_long ,by = "record_id") %>% 
  group_by(sexo,Tratamiento) %>% 
  summarise(Total = n()) %>% View()


ingresos %>% 
  group_by(sexo) %>%
  mutate(TotalPacientes = n()) %>% 
  ungroup() %>% 
  inner_join(trat_long ,by = "record_id") %>%
  group_by(sexo,Tratamiento,TotalPacientes) %>% 
  summarise(Total = n()) %>% 
  ungroup() %>% 
  mutate(frecuencia = 100* Total / TotalPacientes) %>% 
  View()


### Ejercicio 3 3.	Combina las tablas de Ingreso y Outcome y 
##                  calcula el numero de exitus en cada grupo de genero.

ingresos %>% 
  inner_join(outcome ,by = "record_id") %>% 
  filter(grepl("Exitus",outcomefinal)) %>% 
  group_by(sexo) %>% 
  summarise(Total = n()) %>% View()

ingresos %>% 
  inner_join(... ,by = "record_id") %>% 
  filter(outcomefinal =="Exitus" | outcomefinal == "Exitus LTE") %>% 
  group_by(...) %>% 
  summarise(Total = n()) %>% View()



#### Ejercicios ggplot1

### Ejercicio1 •	Representa el histograma de las edades de Ingreso: primero hombres y luego mujeres.

ingresos %>% 
  filter(peso > 20 & peso < 300) %>% 
  ggplot(aes(x = peso)) + geom_histogram() 

ingresos %>% 
  separate(fecha_nac,into = c("ano_nac","mes_nac","dia_nac"),sep = "-") %>% 
  separate(ingreso_fecha, into = c("ano_in","mes_in","dia_in"), sep ="-") %>% 
  separate(dia_in, into = c("dia_in","hora_in"), sep = " ") %>% 
  mutate(edad = as.numeric(ano_in)-as.numeric(ano_nac)) %>% 
  ggplot(aes(x = edad)) + geom_histogram()
  
### Ejercicio 2•	Representa el histograma de la media de la glucosa  de cada paciente

bioquimica <- read_csv("Bioquimica.csv")

bioquimica %>% 
  group_by(record_id) %>% 
  summarise(glucosa_media = mean(glucosa,na.rm = TRUE)) %>% 
  ggplot(aes(x=glucosa_media)) + geom_histogram()

bioquimica %>%
  drop_na(glucosa) %>% 
  group_by(record_id) %>% 
  summarise(glucosa_media = mean(glucosa)) %>% 
  ggplot(aes(x=glucosa_media)) + geom_histogram()

bioquimica %>%
  filter(glucosa < 500) %>% 
  filter(!is.na(glucosa)) %>% 
  group_by(record_id) %>% 
  summarise(glucosa_media = mean(glucosa)) %>% 
  ggplot(aes(x=glucosa_media)) + geom_histogram()


bioquimica %>%
  drop_na(glucosa) %>% 
  group_by(record_id) %>% 
  summarise(glucosa_media = mean(glucosa)) %>% 
  ggplot() + geom_histogram(aes(x=glucosa_media))


### Ejercicio 3 •	Representa en un boxplot las alturas de cada sexo (elimina los outliers).

ingresos %>% 
  ggplot(aes(x = sexo, y = altura)) + geom_boxplot()
ingresos %>% 
  filter(altura > 100 & altura < 300) %>% 
  ggplot(aes(x=sexo, y = altura)) + geom_boxplot()
ingresos %>% 
  filter(altura > 100 & altura < 300) %>% 
  ggplot(aes(x=sexo, y = altura)) + geom_dotplot(binaxis = "y", stackdir = "center")

#### Ejercicio 4 •	Representa en un boxplot las creatininas por cada grupo de outcome

bioquimica %>% 
  inner_join(outcome, by ="record_id") %>% 
  drop_na(creatinina) %>% 
  drop_na(outcomefinal) %>%
  mutate(creatinina = gsub(pattern = ",",replacement = ".",creatinina)) %>% ### Tenemos que cambiar el separador decimal "," por un ".". 
  mutate(creatinina = as.numeric(creatinina)) %>%   #### Es un principio creatinina es un character y tenemos que cambiarle de tipo
  ggplot(aes(x=outcomefinal, y = creatinina, fill = outcomefinal)) + geom_boxplot()





