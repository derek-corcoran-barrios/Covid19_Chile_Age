library(tidyverse)
library(lubridate)
library(doParallel)

## Cargamos las funciones

source("Modelos/Funcion.R")

Start <- Sys.time()
### Elegir fecha inicial de simulacion

Inicio = dmy("15-04-2020")

### Leer probabilidades de migracion

Probs <- read_rds("Bases_de_datos/Datos_Arica y Parinacota/Probs_2020-04-15.rds")

### Leer condiciones iniciales

df_out <- read_rds("Bases_de_datos/Datos_Arica y Parinacota/df_out_2020-04-15.rds") 

#Infectividad de asintomaticos
betaA = 0.06
# Segun sergio 0.02767

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

p_G = c(0.5, 1, 0.5)

###Tamaño promedio de hogar

Sigma = 3.1


## Matriz de contactos entre generaciones


C_G_H <- read_rds("Bases_de_datos/Age_Matrix.rds")

Kappas <- seq(0, 1, length.out = 5)

Resultados <- list()  

for(i in 1:length(Kappas)){
  Resultados[[i]] <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 50, K_Cuar = Kappas[i], Umbral = 40, ncores = 4, Min_Days = 14)
  message(paste0(i, "of", length(Kappas)))
  beepr::beep(2) 
}

beepr::beep(8)

Results <- Resultados %>% purrr::map(~.x$Results) %>% map2(.y = Kappas, ~mutate(.x, Kappa = as.character(.y))) %>% purrr::reduce(bind_rows)

#Resultados2 <- Modelo_Edad_Total(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 30, K_Cuar = 0.5, Dias_Cuar = 5:20, ncores = 4)

#Results <- Resultados$Results %>% mutate(Estrategia = "Cuarentena dinámica")
Results <- Results %>% dplyr::filter(Comuna != "pais")

ggplot(Results, aes(x = Fecha, y = Infectados)) + geom_line(aes(color = Kappa))  + facet_wrap(~Comuna, scales = "free_y") + theme_bw()



Results2 <- Results %>% dplyr::filter(Kappa != "0.5")

ggplot(Results2, aes(x = Fecha, y = Infectados)) + geom_line(aes(lty = Kappa))  + facet_wrap(~Comuna, scales = "free_y") + theme_bw()

Results1 <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 10, K_Cuar = 0.5, Umbral = 40, ncores = 4, Min_Days = 14)

Results2 <- Modelo_Edad_Total(Inicio = (Inicio + 10), df_out = Results1$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 25, K_Cuar = 0.5, ncores = 4, Dias_Cuar = 5:20)


Results_Total <- bind_rows(Results1$Results, Results2$Results)



ggplot(Results_Total, aes(x = Fecha, y = Infectados)) + geom_line()  + facet_grid(rows = vars(Comuna), scales = "free_y") + theme_bw()
