library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

setwd("/Users/val/Downloads/CursoR-master/")

Ingreso <- read_csv("Ingreso.csv")

Ingreso <- read_csv("https://raw.githubusercontent.com/valflanza/CursoR/master/Ingreso.csv")

Ingreso[Ingreso$sexo == "Hombre",]

Ingreso %>% 
  filter(sexo == "Hombre") %>%
  filter(horas_urgencias < 5) %>% 
  filter(peso > 70) %>% 
  View()

### Ejercicios Filter 
  
Ingreso %>% 
  filter(peso > 50)

Ingreso %>% 
  filter(altura > 150)

Ingreso %>% 
  filter(sexo =="Hombre") %>% 
  filter(ingreso =="Urgencias")

Ingreso %>% 
  filter(is.na(peso))

Ingreso %>% 
  filter(!is.na(peso)) %>% 
  filter(!is.na(altura))

Ingreso %>% 
  filter(peso > 20 & peso <300)

Ingreso %>% 
  filter(peso > 20) %>% 
  filter(peso <300)

Ingreso %>% 
  filter(ingreso != "Urgencias")

Farmacia <- read_csv("https://raw.githubusercontent.com/valflanza/CursoR/master/Farmacia.csv")
Farmacia <- read_csv("Farmacia.csv")

Farmacia %>% 
  filter(principio_activo_descripcion == "[METILPREDNISOLONA]")

filter(Farmacia,principio_activo_descripcion == "[METILPREDNISOLONA]" )

Farmacia %>% 
  filter(uh_descripcion == "[NEUMOLOGÍA]")


#### Ejercicios SELECT

Ingreso %>% 
  select(record_id, sexo, peso)

Ingreso %>% 
  select(-starts_with("redcap"))

Farmacia %>% 
  select(uh_descripcion) %>% 
  distinct() %>% View()



Bioquimica <- read_csv("https://raw.githubusercontent.com/valflanza/CursoR/master/Bioquimica.csv")
Bioquimica <- read_csv("Bioquimica.csv")

Bioquimica %>% 
  select(cpk,proteinas,record_id)

resultado <- Bioquimica %>% 
  select(urea:ferritina)


resultado <- Ingreso %>%
  filter(sexo =="Hombre") %>%
  filter(ingreso =="Urgencias")


### Ejemplos de GROUP_BY

Ingreso %>% 
  group_by(sexo) %>% 
  summarise(Npaciente = n())

Ingreso %>% 
  group_by(sexo, ingreso) %>% 
  summarise(Npaciente = n()) 

Farmacia %>% 
  filter(grupo_terapeutico_descripcion == "[GLUCOCORTICOIDES]") %>% 
  group_by(uh_descripcion,principio_activo_descripcion) %>% 
  summarise(Ntratamientos = n()) %>% View()

Farmacia %>% 
  filter(grepl("CORTICOIDES", grupo_terapeutico_descripcion)) %>% 
  group_by(uh_descripcion,principio_activo_descripcion, grupo_terapeutico_descripcion) %>% 
  summarise(Ntratamientos = n()) %>% View()

Farmacia %>% 
  group_by(uh_descripcion,principio_activo_descripcion) %>% 
  summarise(Ntratamientos = n()) %>% View()

Farmacia %>%
  select(record_id,uh_descripcion) %>% 
  distinct() %>% 
  group_by(uh_descripcion) %>% 
  summarise(Pacientes = n())

Farmacia %>% 
  group_by(record_id) %>% 
  mutate(Prescripciones = n()) %>%
  ungroup() %>% 
  group_by(uh_descripcion) %>% 
  mutate(Pacientes = n_distinct(record_id)) %>%
  ungroup() %>% 
  group_by(uh_descripcion,Pacientes) %>%
  summarise(MediaPrescripciones = mean(Prescripciones)) %>% View()
  
resultado <- Farmacia %>% 
  group_by(record_id) %>% 
  summarise(Prescripciones = n())



resultado %>% ggplot(aes(x=Prescripciones)) + geom_histogram()


mean(resultado$Prescripciones)
median(resultado$Prescripciones)
sd(resultado$Prescripciones)

hist(resultado$Prescripciones)
