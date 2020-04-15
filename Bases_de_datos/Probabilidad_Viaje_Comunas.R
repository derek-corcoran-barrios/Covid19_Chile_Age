library(tidyverse)
library(lubridate)
library(readxl)

#Para actualizar base de datos

githubURL <- ("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados_comunas.csv")
download.file(githubURL,"COVID_tasaXcomunas.csv", method="curl")

Pais <- read_csv("COVID_tasaXcomunas.csv") 

Pais <- Pais %>% mutate_at(as.numeric, .vars = vars(5:ncol(Pais))) %>% dplyr::select(-codigo_region, -codigo_comuna) %>% pivot_longer(cols = c(-region, -comuna), names_to = "Fecha", values_to = "Acumulados") %>% mutate(Fecha = mdy(Fecha), Acumulados = ifelse(is.na(Acumulados), 0, Acumulados)) %>% rename(Comuna = comuna)  %>% mutate(Comuna = str_to_lower(Comuna)) %>% mutate(Comuna = str_remove_all(Comuna, " \\*")) %>%
  mutate(Comuna = str_replace_all(Comuna, "á", "a"), Comuna = str_replace_all(Comuna, "é", "e"), Comuna = str_replace_all(Comuna, "í", "i"), Comuna = str_replace_all(Comuna, "ó", "o"), Comuna = str_replace_all(Comuna, "ú", "u")) %>% dplyr::filter(Comuna != "total") %>% mutate(Comuna = str_replace_all(Comuna, "aisen", "aysen"))

saveRDS(Pais, "Bases_de_datos/Pais.rds")


file.remove("COVID_tasaXcomunas.csv")

## Si no hay que actualizar la base de datos empezamos desde acá

### Esta parte del código genera la base de datos para Infectados acumulados, Infectados y Recuperados 

Dias_Hasta_Recuperado = 14

Pais <- read_rds("Bases_de_datos/Pais.rds") %>% mutate(Fecha_Reciente = max(Fecha), Diferencia = as.numeric(Fecha_Reciente - Fecha), Para_Recuperados = abs(Dias_Hasta_Recuperado - Diferencia))


Actual <- Pais %>% dplyr::filter(Fecha == max(Fecha)) %>% dplyr::select(region, Comuna, Fecha, Acumulados)

Pasado <- Pais %>% dplyr::filter(Para_Recuperados == min(Para_Recuperados)) %>% dplyr::select(region, Comuna, Acumulados) %>% rename(Acumulados_old = Acumulados)

Pais <- full_join(Actual, Pasado) %>% mutate(Infectados = Acumulados - Acumulados_old, Recuperados = Acumulados - Infectados) %>% dplyr::select(-Acumulados_old)

rm(Actual)
rm(Pasado)


### Ahora unimos esto con las poblaciones y separamos por grupo etareo, además agregamos los asintomáticos y los expuestos


Cantidad_Edad <- read_excel("Bases_de_datos/Cantidad-de-Personas-por-Sexo-y-Edad.xlsx",sheet = "COMUNAS")


colnames(Cantidad_Edad) <- make.names(colnames(Cantidad_Edad))

Cantidad_Edad <- Cantidad_Edad %>% dplyr::select(NOMBRE.REGIÓN, NOMBRE.COMUNA, Edad, TOTAL) %>% dplyr::filter(NOMBRE.COMUNA != "PAÍS", Edad != "Total País") %>% rename(Region = NOMBRE.REGIÓN, comuna = NOMBRE.COMUNA) %>% 
  mutate(Edad = case_when(Edad %in% c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24")~ "Under_25", Edad %in% c("25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64") ~ "Adult", Edad %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 a 89", "90 a 94", "95 a 99", "100 o más") ~ "Over_65", Edad == "Total Comunal" ~ "Total")) %>% 
  group_by(comuna, Edad) %>% summarise(Poblacion = sum(TOTAL)) %>% ungroup() %>% pivot_wider(values_from = Poblacion, names_from = Edad) %>% mutate(Under_25 = Under_25/Total, Adult = Adult/Total, Over_65 = Over_65/Total) %>% mutate(comuna = str_to_lower(comuna)) %>% rename(Comuna = comuna)  %>% mutate(Comuna = str_to_lower(Comuna))%>% mutate(Comuna = str_remove_all(Comuna, " \\*")) %>%
  mutate(Comuna = str_replace_all(Comuna, "á", "a"), Comuna = str_replace_all(Comuna, "é", "e"), Comuna = str_replace_all(Comuna, "í", "i"), Comuna = str_replace_all(Comuna, "ó", "o"), Comuna = str_replace_all(Comuna, "ú", "u")) %>% ungroup()



### Unimos ambas bases de datos

Pais <- full_join(Pais, Cantidad_Edad)

# Agregamos una estimación de Asintomáticos y de Expuestos y dividimos el resto de los grupos por edad


Pais <- Pais  %>%  mutate(Asintomaticos = Infectados*2.56, Expuestos = Infectados*2.79, UCI = Infectados*0.02, Recuperados = Recuperados - UCI, Suceptibles = Total - (Infectados + Asintomaticos + Expuestos + UCI + Recuperados)) %>% 
  mutate(Infectados_Adult = Infectados*Adult, Infectados_Over_65 = Infectados*Over_65, Infectados_Under_25 = Infectados*Under_25) %>% 
  mutate(Suceptibles_Adult = Suceptibles*Adult, Suceptibles_Under_25 = Suceptibles*Under_25, Suceptibles_Over_65 = Suceptibles*Over_65) %>% 
  mutate(Expuestos_Adult = Expuestos*Adult, Expuestos_Under_25 = Expuestos*Under_25, Expuestos_Over_65 = Expuestos*Over_65) %>% 
  mutate(Asintomaticos_Adult = Asintomaticos*Adult, Asintomaticos_Under_25 = Asintomaticos*Under_25, Asintomaticos_Over_65 = Asintomaticos*Over_65) %>%
  mutate(UCI_Adult = UCI*Adult, UCI_Under_25 = UCI*Under_25, UCI_Over_65 = UCI*Over_65) %>% 
  mutate(Muerto_Adult = 0, Muerto_Under_25 = 0, Muerto_Over_65 = 0) %>% 
  mutate(Recuperados_Adult = Recuperados*Adult, Recuperados_Under_25 = Recuperados*Under_25, Recuperados_Over_65 = Recuperados*Over_65) %>% 
  mutate(Cuarentenados_Adult = 0, Cuarentenados_Under_25 = 0, Cuarentenados_Over_65 = 0, Time = 1, Adult = Adult*Total, Over_65 = Over_65*Total, Under_25 = Under_25*Total) %>% 
  dplyr::select("Comuna", "Total", "Adult", "Over_65", "Under_25", "Infectados", 
                "Infectados_Adult", "Infectados_Over_65", "Infectados_Under_25", 
                "Suceptibles_Adult", "Suceptibles_Under_25", "Suceptibles_Over_65", 
                "Expuestos_Adult", "Expuestos_Under_25", "Expuestos_Over_65", 
                "Asintomaticos_Adult", "Asintomaticos_Under_25", "Asintomaticos_Over_65", 
                "UCI_Adult", "UCI_Under_25", "UCI_Over_65", "Muerto_Adult", "Muerto_Under_25", 
                "Muerto_Over_65", "Recuperados_Adult", "Recuperados_Under_25", 
                "Recuperados_Over_65", "Cuarentenados_Adult", "Cuarentenados_Under_25", 
                "Cuarentenados_Over_65", "Time") %>% rename(Poblacion = Total)

Under_25 <- Pais %>% dplyr::select(Comuna, Poblacion, contains("Under_25"), Time) %>% rename(Generacion = Under_25)

Adult <- Pais %>% dplyr::select(Comuna, Poblacion, contains("Adult"), Time) %>% rename(Generacion = Adult)

Over_65 <- Pais %>% dplyr::select(Comuna, Poblacion, contains("Over_65"), Time) %>% rename(Generacion = Over_65)

#Cambiamos los nombres de las columnas para que todos tengas cosas iguales

colnames(Under_25) <- str_remove_all(string = colnames(Under_25), pattern = "_Under_25")

colnames(Adult) <- str_remove_all(string = colnames(Adult), pattern = "_Adult")

colnames(Over_65) <- str_remove_all(string = colnames(Over_65), pattern = "_Over_65")

### Lo agregamos todo a una lista llamada df_out con los nombres de las edades


df_out <- list(Under_25, Adult, Over_65)

names(df_out) <- c("Under_25", "Adult", "Over_65")

dir.create("Bases_de_datos/Datos_Chile")

saveRDS(df_out, paste0("Bases_de_datos/Datos_Chile/df_out_", Fecha_Reciente, ".rds")) 
