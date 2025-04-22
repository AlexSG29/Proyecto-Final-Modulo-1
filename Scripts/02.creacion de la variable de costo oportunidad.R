# -------------------------------------------------------------------------    
# Titulo :data
# Autor(es) : Maria Mercedes Romero Racine
# Afiliación : UTB 
# Fecha de creación : 2025-04-21
# ------------------------------------------------------------------------- 
# Notas :
# ------------------------------------------------------------------------- 
# Configuración -----------------------------------------------------------
# Limpiar memoria 
rm(list = ls()); invisible(gc())
# Librerias
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)
# ------------------------------------------------------------------------- 
# Cargar datos ------------------------------------------------------------
df<- read.csv("Data/Procesada/Alex_01_Depurada.csv")
df_usuario <- read_xlsx("Data/Cruda/anex-ETUP-IVtrim2024.xlsx",sheet = "3.3 V.A.C Mov. SITM") %>% clean_names()
df_num_ruta <-read_xlsx("Data/Cruda/SERVICIOS MENSUALES.xlsx",range = "A1:M22") %>% clean_names()

#parametros
parametros<- list(
  num_buseton = 117, #numero de flota total a junio de 2024
  num_patron = 162, #numero de flota total a junio de 2024
  dias_labo = 30,# dias del mes
  costo_pasaje = 3000 #costos pasaje a 2024
)



## homogenenizacion del texto y creacion de la variable de ganancia perdida por
# falla 1 dia
#homogenizacion de la data principal
df <-df %>% 
  clean_names() %>% 
  rename(costo_x_perdida = costo_operativo) %>% 
  mutate(fecha = as.Date(fecha), # trnaformacion de la varible fecha
         dia_habil = format(fecha,"%A"),#dia de ocurrencia de la falla
         year_falla = year(fecha), #año de ocurrecia de la falla
         mes_falla = format(fecha,"%B"), #mes de ocurrencia de la falla
         across(sistema_reportado:observacion,~ toupper(.x)),#se convierte todo el texto en mayuscula
         across(sistema_reportado:observacion, ~ ifelse(str_detect(.x,"N.A"),"",.x)),
         costo_x_perdida = trimws(costo_x_perdida),#se quitan los espacios que existes en esta variable
         costo_x_perdida = gsub("[[:punct:]]","",costo_x_perdida), #se quitan los caracteres especiales "$"
         costo_x_perdida = as.numeric(costo_x_perdida), #se transforma la variable a tipo numerico
         ruta = ifelse(vehiculo == "TC32091" & ruta == "","X106",ruta)
  ) %>% left_join(
    df_num_ruta <-df_num_ruta %>% 
      rename(ruta = rutas) %>% 
      select(ruta,sep,dic) %>% 
      mutate(across(c(sep,dic),~ round(.x/parametros$dias_labo))) %>% 
      rename(num_min_viajes = dic,
             num_max_viajes = sep)
  ) %>% left_join( df_usuario <-df_usuario[27:28,] %>% 
                     select(encuesta_de_transporte_urbano_de_pasajeros_etup,x12) %>% 
                     rename(total_pasajeros_trasportados_en_miles_mes = x12,
                            tipologia = encuesta_de_transporte_urbano_de_pasajeros_etup) %>% 
                     mutate(total_pasajeros_trasportados_en_miles_mes = as.numeric(total_pasajeros_trasportados_en_miles_mes),
                            total_pasajeros_trasportados_mes = total_pasajeros_trasportados_en_miles_mes*1000,
                            total_pasajeros_trasportados_diario = total_pasajeros_trasportados_mes/parametros$dias_labo,
                            tipologia = case_when(tipologia == "SITM Alimentador"~"BUSETON",
                                                  tipologia == "SITM Padrón"~"PADRON",
                                                  TRUE ~ tipologia),
                            total_usuarios_por_ruta = ifelse(tipologia == "BUSETON",
                                                             round(total_pasajeros_trasportados_diario/parametros$num_buseton),
                                                             round(total_pasajeros_trasportados_diario/parametros$num_patron))) %>% 
                     select(tipologia,total_usuarios_por_ruta)
  ) %>% 
  mutate(num_min_pasajeros = ifelse(tiplogia == "BUSETON",80,90),
         num_max_pasajeros = ifelse(tiplogia == "BUSETON",160,180),
         costo_oportunidad_plana =total_usuarios_por_ruta*parametros$costo_pasaje,
         costo_opot_min = num_min_pasajeros*parametros$costo_pasaje,
         costo_opot_max = num_max_pasajeros*parametros$costo_pasaje)









