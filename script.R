library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(tidyr)
# import data

paso_df <- read_csv('data/mesas_votos_priorizado.csv') %>% tibble()


# EDA

glimpse(paso_df)

paso_df$tipo_voto %>% table() #inconsistent class. keep 'positivo'

paso_df <- paso_df %>% filter(tipo_voto == 'positivo')

paso_df %>% 
  group_by(color) %>% 
  summarise(votos = sum(votos, na.rm = T)) %>% 
  ggplot(aes(x = color, y = votos))+
  geom_col( fill = 'steelblue')+
  theme_bw()

paso_df %>% 
  select(cargo,id_cargo) %>% 
  table()  # consistency check


# group by color

paso_df <- paso_df %>% 
  group_by_at(
    vars(setdiff(names(paso_df), c('agrupacion', 'fecha_hora', 'id_agrupacion', 
                              'id_agrupacion_int', 'id_lista', 'lista', 
                              'tipo_voto', 'votos', 'seccion_provincial')))) %>% 
  summarise(votos = sum(votos, na.rm = T))




# Estimate props

paso_df <- paso_df %>% 
  group_by(distrito, id_seccion, id_circuito, mesa, cargo) %>% 
  mutate(tot_votos = sum(votos, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(prop_votos = votos/tot_votos) %>% 
  filter(tot_votos >= 100)


#plot dists

ggplot(paso_df)+
  geom_density(aes(x = prop_votos, fill = color), alpha = .5)+
  scale_fill_manual(values = c('yellow','skyblue', 'red'))+
  facet_wrap(~cargo)+
  theme_bw()


# thresholds by geolvl

geo_levels <- 
  list(
    c('distrito', 'cargo', 'color', 'id_seccion'),
    c('distrito', 'cargo', 'color', 'id_seccion', 'id_circuito')
  )


geo_thresholds <- map(geo_levels, ~
  paso_df %>% 
    group_by_at(vars(.x),.drop = T) %>% 
    summarise(qlow = quantile(prop_votos, .02),
              qhigh = quantile(prop_votos, .98)) %>% 
    ungroup()
)

geo_thresholds[[1]] %>% names()
geo_thresholds[[2]] %>% names()


# add outlier flags


paso_out <- paso_df %>% 
  left_join(geo_thresholds[[1]]) %>% 
  mutate(
    outlier_h_seccion = if_else(prop_votos > qhigh, 1, 0),
    outlier_l_seccion = if_else(prop_votos < qlow, 1, 0),
    outlier_seccion = if_else((outlier_h_seccion == 1 |
                                outlier_l_seccion == 1), 1, 0)) %>% 
  select(-qhigh, -qlow) %>% 
  left_join(geo_thresholds[[2]]) %>% 
  mutate(
    outlier_h_circuito = if_else(prop_votos > qhigh, 1, 0),
    outlier_l_circuito = if_else(prop_votos < qlow, 1, 0),
    outlier_circuito = if_else(outlier_h_circuito == 1 |
                                outlier_l_circuito == 1, 1, 0),
    outlier_tot_h = if_else(outlier_h_circuito == 1 |
                              outlier_h_seccion == 1, 1, 0),
    outlier_tot_l = if_else(outlier_l_circuito == 1 |
                              outlier_l_seccion == 1, 1, 0),
    outlier_tot = if_else(outlier_tot_h == 1 |
                            outlier_tot_l == 1, 1, 0)
  ) %>% 
  select(-qhigh, -qlow) 


write_csv(paso_out, 'out/paso_out.csv')
  

