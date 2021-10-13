library(tidyverse)
library(ggsci)

setwd("~/CursoR/")

bioquimica <- read_csv("Bioquimica.csv")

bioquimica %>% 
  filter(glucosa > 150) %>%  
  filter(urea < 40)

bioquimica %>% 
  select(record_id, glucosa:bilirrubina) %>% 
  filter(glucosa > 150) %>%  
  filter(urea < 40) %>% 
  View()

bioquimica %>% group_by(record_id) %>% summarise(MediaGlucosa = mean(glucosa, na.rm = TRUE))

bioquimica %>% group_by(record_id) %>% mutate(MediaGlucosa = mean(glucosa, na.rm = TRUE)) 

bioquimica %>% 
  mutate(creatinina = gsub(",",".",creatinina)) %>%
  mutate(creatinina = as.numeric(creatinina)) %>% 
  group_by(record_id) %>% 
  summarise(MediaCreatinina = mean(creatinina, na.rm = TRUE))

ingreso <- read_csv("Ingreso.csv")
outcomes <- read_csv("Outcome.csv")

ingreso %>% inner_join(outcomes, by = "record_id") %>% View()

comorbilidades <- read_csv("Comorbilidades.csv")

comorbilidades %>% 
  select(record_id,fact_riesgo_com___1:fact_riesgo_com___21) %>% 
  pivot_longer(names_to = "Variable", values_to = "Values", cols = -record_id) %>%
  filter(!is.na(Values)) %>% 
  View()

comorbilidades %>% 
  select(record_id,fact_riesgo_com___1:fact_riesgo_com___21) %>% 
  pivot_longer(names_to = "Variable", values_to = "Comorbilidad", cols = -record_id) %>%
  drop_na(Comorbilidad) %>% 
  ggplot(aes(x= Comorbilidad, fill = Comorbilidad)) + geom_bar()


comorbilidades %>% 
  select(record_id,fact_riesgo_com___1:fact_riesgo_com___21) %>% 
  pivot_longer(names_to = "Variable", values_to = "Comorbilidad", cols = -record_id) %>%
  drop_na(Comorbilidad) %>% 
  inner_join(ingreso) %>% 
  group_by(sexo, Comorbilidad) %>% 
  summarise(NPacientes = n()) %>% 
  ggplot(aes(x = Comorbilidad, y = NPacientes, fill = sexo)) + 
  geom_col(position = "fill") + 
  scale_fill_d3(palette = "category20") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Ejercicio 2.1 Representa en un gráfico de barras el numero de ingresos por mes. 
# Ahora, añade la información del sexo

ingreso %>% 
  separate(ingreso_fecha,c("año","mes","dia") , sep ="-") %>%
  group_by(sexo,mes) %>% 
  summarise(NIngresos = n()) %>%  
  ggplot(aes(x =mes, y=NIngresos,group =sexo, color = sexo, fill = sexo)) +
  geom_col(position = "dodge")+ 
  geom_line() +
  geom_point() +
  scale_fill_npg() +
  scale_color_npg() +
  theme_light()
  
# Ejercicio 2.2 Representa en un gráfico de barras el numero de pacientes por 
# uh_descripción (utiliza la tabla de Farmacia)

farmacia <- read_csv("Farmacia.csv")

farmacia %>%
  select(record_id,uh_descripcion) %>% 
  distinct(record_id,uh_descripcion) %>% 
  group_by(record_id,uh_descripcion) %>% 
  summarise(Npacientes = n()) %>% 
  ggplot(aes(x = uh_descripcion, y = Npacientes)) + 
  geom_col() +
  coord_flip()

farmacia %>% 
  group_by(uh_descripcion) %>% 
  summarise(Npacientes = n_distinct(record_id)) %>%
  ggplot(aes(x = uh_descripcion, y = Npacientes)) + 
  geom_col() +
  coord_flip()


# Ejercicio 2.3 Representa en un grafico de barras los tratamientos habituales por sexo.
# (debes juntar las tablas ingreso y Tratamientos_habituales)

tratamientos <- read_csv("Tratamientos_habituales.csv")

tratamientos %>% 
  select(record_id, trat_habitual_opc___1:trat_habitual_opc___12) %>% 
  pivot_longer(names_to = "Columna",values_to = "Tratamiento", -record_id) %>%   ##pivot_longer(names_to = "Variable", values_to = "Comorbilidad", cols = -record_id) %>%
  drop_na(Tratamiento) %>%
  select(-Columna) %>% 
  inner_join(ingreso, by = "record_id" ) %>% 
  group_by(sexo, Tratamiento) %>% 
  summarise(Npacientes = n()) %>% 
  ggplot(aes(x =Tratamiento, y= Npacientes, fill = sexo)) + 
  geom_col(position = 'dodge') +
  coord_flip()

# Ejercicio 2.4 Representa en un boxplot los valores de la tabla de 
#Inmunologia (debes colocar adecuendamente los datos para poder representarlo: pivot_longer).

Inmuno = read_csv("Inmunología.csv")

Inmuno$`IL-6 (pg/mL)` = gsub(",",".",Inmuno$`IL-6 (pg/mL)`)
Inmuno$`IL-10 (pg/mL)` = gsub(",",".",Inmuno$`IL-10 (pg/mL)`)

Inmuno$`IL-6 (pg/mL)` = gsub("*","",Inmuno$`IL-6 (pg/mL)`)
Inmuno$`IL-10 (pg/mL)` = gsub("*","",Inmuno$`IL-10 (pg/mL)`)

Inmuno$`IL-6 (pg/mL)` = as.numeric(Inmuno$`IL-6 (pg/mL)`)
Inmuno$`IL-10 (pg/mL)` = as.numeric(Inmuno$`IL-10 (pg/mL)`)


Inmuno %>% 
  select(record_id,`IL-6 (pg/mL)`:`IL-8 (pg/mL)`) %>% 
  pivot_longer(names_to = "Citoquina", values_to = "Valor", -record_id) %>%
  drop_na(Valor) %>% 
  ggplot(aes(x = Citoquina , y= Valor, fill=Citoquina)) +
  geom_violin() + 
  facet_wrap(.~ Citoquina, scales = "free") + scale_y_sqrt()


# Ejercicio 2.5 Representa una nube de puntos con los datos de glucosa (eje x) y urea (eje y).
# Puedes añadir “geom_smoth” para ver si existe una tendencia.

bioquimica <- read_csv("Bioquimica.csv")

bioquimica %>% 
  select(glucosa, urea) %>% 
  drop_na() %>% 
  filter(glucosa < 1500) %>% 
  filter(glucosa > 0 & urea > 0) %>% 
  ggplot(aes(x = glucosa, y = urea)) + 
  geom_point(alpha = 0.2) + geom_smooth(method = "lm") 



