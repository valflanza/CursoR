library(tidyverse)


setwd("~/CursoR/")

### Heatmap

bioquimica <- read_csv("bioquimica.csv")

bioquimica %>% 
  select(record_id, glucosa:prot_c_react) %>% 
  pivot_longer(names_to = "Parametro",
               values_to = "Valor",
               -record_id, 
               values_transform = list(Valor = as.character)) %>%   # Este ultimo parametro nos permite 
                                                                    # decirle que todos los valores se conviertan en caracteres
  drop_na() %>% 
  mutate(Valor = gsub(",",".",Valor)) %>% 
  mutate(Valor = as.numeric(Valor)) %>% 
  group_by(record_id,Parametro) %>% 
  summarise(Media = mean(Valor)) %>% 
  pivot_wider(names_from = Parametro, values_from = Media, values_fill = 0) %>% 
  column_to_rownames("record_id") %>% 
  as.matrix() %>% 
  pheatmap::pheatmap(scale = "column")

### 5.1 Representa en un diagrama de barras los outcomes por sexo. 
##  Selecciona una paleta de colores adecuada usando el paquete “ggsci”.
library(ggsci)
ingreso <- read_csv("Ingreso.csv")
outcomes <- read_csv("Outcome.csv")


ingreso %>% 
  inner_join(outcomes, by = "record_id") %>% 
  drop_na(outcomefinal) %>% 
  ggplot(aes(x = sexo, fill = outcomefinal)) + 
  geom_bar(position = "fill") + 
  coord_flip() +
  scale_fill_d3()
  ###scale_fill_manual(values = c("red","green","blue","yellow","black","darkred","grey","darkgreen","navy","orange"))
  
### 5.2. Representa con un boxplot los análisis de Inmunología y divide el grafico en “facets” por sexo.

inmuno <- read_csv("Inmunología.csv")

inmuno %>% 
  select(-cod_unico,-`Nº petición`) %>%
  pivot_longer(names_to = "Parametro",
               values_to = "Valor",
               -record_id, 
               values_transform = list(Valor = as.character)) %>%
  drop_na(Valor) %>% 
  mutate(Valor = gsub(",",".",Valor)) %>% 
  mutate(Valor = gsub("\\*","",Valor)) %>% 
  mutate(Valor = as.numeric(Valor)) %>% inner_join(ingreso) %>% 
  filter(Valor > 0) %>% inner_join(outcomes, by ="record_id") %>% 
  ggplot(aes(x = Parametro, y = Valor, fill = Parametro)) + 
  geom_boxplot() +
  facet_grid(outcomefinal~sexo) +
  scale_y_log10() +
  coord_flip()
 

## 5.3 Representa en boxplot las edades de cada outcome.

ingreso %>% 
  separate(fecha_nac, c("Año_nac","Mes_nac","Dia_nac"), sep = "-") %>% 
  separate(ingreso_fecha, c("Año_in","Mes_in","Dia_in"), sep ="-") %>% 
  mutate(edad = as.numeric(Año_in) - as.numeric(Año_nac)) %>% 
  inner_join(outcomes, by ="record_id") %>% 
  drop_na(outcomefinal) %>% 
  ggplot(aes(x=outcomefinal, y =edad, fill = outcomefinal)) + geom_boxplot() + coord_flip()


## 5.4. Representa con un diagrama de barras las comorbilidades por outcome y 
## sepáralo en graficas por el sexo (debes juntar las tablas: Ingreso, comorbilidades y outcome) 

comorbilidades <- read_csv("Comorbilidades.csv")

comorbilidades %>% 
  select(record_id, fact_riesgo_com___1:fact_riesgo_com___20) %>%
  pivot_longer(names_to = "kk", values_to = "comorbilidad", -record_id) %>% 
  select(-kk) %>% 
  drop_na(comorbilidad) %>% 
  inner_join(ingreso) %>% 
  inner_join(outcomes, by= "record_id") %>%
  ggplot(aes(x=outcomefinal, fill = comorbilidad)) + 
  geom_bar(position = "fill") + 
  facet_wrap(~sexo) +
  coord_flip() + 
  scale_fill_d3(palette = "category20")
  
comorbilidades %>% 
  select(record_id, fact_riesgo_com___1:fact_riesgo_com___20) %>%
  pivot_longer(names_to = "kk", values_to = "comorbilidad", -record_id) %>% 
  select(-kk) %>% 
  drop_na(comorbilidad) %>% 
  inner_join(ingreso) %>% 
  inner_join(outcomes, by= "record_id") %>%
  group_by(outcomefinal,comorbilidad,sexo) %>% 
  summarise(pacientes = n()) %>% 
  ggplot(aes(x= outcomefinal, y = pacientes, fill = comorbilidad)) +
  geom_col()+
  facet_wrap(~sexo) +
  coord_flip() + 
  scale_fill_d3(palette = "category20")


#  5.7 Representa con un geom_count el numero de pacientes (tamaño del punto) según sus 
#  comorbilidades (x) y su outcome (y). Haz una gráfica por cada uno de los sexos.

comorbilidades %>% 
  select(record_id, fact_riesgo_com___1:fact_riesgo_com___20) %>%
  pivot_longer(names_to = "kk", values_to = "comorbilidad", -record_id) %>% 
  select(-kk) %>% 
  drop_na(comorbilidad) %>% 
  inner_join(ingreso) %>% 
  inner_join(outcomes, by= "record_id") %>%
  group_by(outcomefinal,comorbilidad,sexo) %>% 
  summarise(pacientes = n()) %>% 
  ggplot(aes(x= outcomefinal, y = comorbilidad, size = pacientes)) + 
  geom_count() + 
  facet_wrap(~sexo) +
  theme(axis.text.x = element_text(angle = 90))


comorbilidades %>% 
  select(record_id, fact_riesgo_com___1:fact_riesgo_com___20) %>%
  pivot_longer(names_to = "kk", values_to = "comorbilidad", -record_id) %>% 
  select(-kk) %>% 
  drop_na(comorbilidad) %>% 
  inner_join(ingreso) %>% 
  inner_join(outcomes, by= "record_id") %>%
  group_by(outcomefinal,comorbilidad,sexo) %>% 
  summarise(pacientes = n()) %>% 
  ggplot(aes(x= outcomefinal, y = comorbilidad, fill = pacientes)) + 
  geom_tile() + 
  geom_text(aes(label = pacientes),  color = "white") +
  facet_wrap(~sexo) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90))

# 5.8. Usa la función “slide_head” para seleccionar los 5 servicios médicos que mas
# pacientes atendieron  y los 15 grupos terapéuticos. Para usar la función slice_head antes 
# deberás de ordenar las tablas con la función arrange(). Tendrás que crear dos listas (servicios y medicamentos) 
# para filtrar tu tabla original. Representa los tratamientos por outcome y divídelo por servicios (facet).

farmacia <- read_csv("Farmacia.csv")

servicios <-farmacia %>%
  group_by(uh_descripcion) %>% 
  summarise(Npacientes = n_distinct(record_id)) %>% 
  arrange(desc(Npacientes)) %>% 
  slice_head(n =5)

medicamentos <- farmacia %>% 
  group_by(grupo_terapeutico_descripcion) %>% 
  summarise(Ntratamiento = n()) %>% 
  arrange(desc(Ntratamiento)) %>% 
  slice_head(n=15)

farmacia %>% 
  semi_join(servicios) %>% 
  semi_join(medicamentos) %>% 
  inner_join(outcomes, by = "record_id") %>% 
  group_by(outcomefinal, uh_descripcion, grupo_terapeutico_descripcion) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x= outcomefinal, y = N, fill = grupo_terapeutico_descripcion)) + 
  geom_col(position = "fill") + 
  facet_wrap(~uh_descripcion, scales = "free_x") +
  scale_fill_d3(palette = "category20")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90))
