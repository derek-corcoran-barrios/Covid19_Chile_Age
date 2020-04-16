library(tidyverse)
library(lubridate)
library(doParallel)

## Cargamos las funciones

source("Modelos/Funcion.R")

Start <- Sys.time()
### Elegir fecha inicial de simulacion

Inicio = dmy("10-04-2020")

### Leer probabilidades de migracion

Probs <- read_rds("Bases_de_datos/Datos_Metropolitana/Probs_2020-04-13.rds")

### Leer condiciones iniciales

df_out <- read_rds("Bases_de_datos/Datos_Metropolitana/df_out_2020-04-13.rds") 

#Infectividad de asintomaticos
betaA = 0.06

#Infectividad de Infectados
betaI = 0.06

#numero de contactos promedio
K_g = read_rds("Bases_de_datos/Contactos.rds")

#Tasa latente
Eta = 1/2.34

#Tasa de infeccion asintomática
Alpha_g = c(1/5.06, 1/2.86, 1/2.86)

#Tasa de escape
Mu_g = c(1, 1/3.2, 1/3.2)

#Fracción de casos que requieren UCI
Gamma_g = c(0.002,0.05,0.36)

#Fatalidad en la UCI
Omega_g = 0.42

#Tasa de mortalidad

Psi_g = 1/7

# Tasa de salida de la UCI

Chi_g = 0.1

# Factor de densidad

Epsilon = 0.01

#Factor de movilidad

p_G = c(0.1, 1, 0.1)

###Tamaño promedio de hogar

Sigma = 3.1


## Matriz de contactos entre generaciones


C_G_H <- read_rds("Bases_de_datos/Age_Matrix.rds")

Resultados <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 10, K_Cuar = 0.5, Umbral = 40, ncores = 4)

end <- Sys.time()

#saveRDS(Results, "AdaptativoK_0_5_Chile_2020_04_10.rds")
#write_csv(Results, "AdaptativoK_0_5_Chile_2020_04_10.csv")

Results <- Resultados %>% dplyr::filter(Comuna != "pais")

ggplot(Results, aes(x = Fecha, y = Infectados)) + geom_line() + geom_point(aes(color = Cuarentena)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()
ggplot(Results, aes(x = Fecha, y = UCI)) + geom_line() + geom_point(aes(color = Cuarentena)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()

#Maxs <- Results %>% group_by(Comuna) %>%  dplyr::filter(UCI == max(UCI)) %>% mutate(Prop_UCI = (UCI/Poblacion)*100000) %>% dplyr::select(Comuna, UCI, Prop_UCI, Fecha)  %>% arrange(desc(Prop_UCI))

#write_csv(Maxs, "Maximo_UCI.csv")

#MaxsI <- Results %>% group_by(Comuna) %>%  dplyr::filter(Infectados == max(Infectados)) %>% mutate(Prop_I = (Infectados/Poblacion)*100000) %>% dplyr::select(Comuna, Infectados, Prop_I, Fecha)  %>% arrange(desc(Prop_I)) 

#write_csv(MaxsI, "Maximo_Infectados.csv")

#MaxsD <- Results %>% group_by(Comuna) %>%  dplyr::filter(Muerto == max(Muerto)) %>% mutate(Prop_D = (Muerto/Poblacion)*100000) %>% dplyr::select(Comuna, Muerto, Prop_D, Fecha)  %>% arrange(desc(Prop_D)) 



