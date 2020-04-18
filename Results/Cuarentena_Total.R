# Con este source obtenemos las funciones

source("Modelos/Funcion.R")

#Luego con este source agregamos las variables

source("Modelos/Modelo.R")

library(tidyverse)
library(lubridate)
library(doParallel)

Start <- Sys.time()
### Elegir fecha inicial de simulacion

Inicio = dmy("15-04-2020")

### Leer probabilidades de migracion

dir.create("Results/Magallanes")


Probs <- read_rds("Bases_de_datos/Datos_Magallanes/Probs_2020-04-15.rds")

### Leer condiciones iniciales

df_out <- read_rds("Bases_de_datos/Datos_Magallanes/df_out_2020-04-15.rds") 


## Cuarentena total por 7 dias

Resultados <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 5, K_Cuar = 0.5, Umbral = 40, ncores = 4, Min_Days = 1)

Resultados2 <- Modelo_Edad_Total(Inicio = (Inicio + 5), df_out = Resultados$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 7, K_Cuar = 0.5, ncores = 4, Dias_Cuar = 1:7)

Resultados3 <- Modelo_Edad(Inicio = (Inicio + 12), df_out = Resultados2$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 48, K_Cuar = 0, Umbral = 40, ncores = 4, Min_Days = 1)

Results <- list(Resultados$Results, Resultados2$Results, Resultados3$Results) %>% purrr::reduce(bind_rows)

saveRDS(Results, "Results/Magallanes/Results_2020_04_15_K_0_5_Total_7_Magallanes.rds")


## Cuarentena total por 15 dias

Resultados2 <- Modelo_Edad_Total(Inicio = (Inicio + 5), df_out = Resultados$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 15, K_Cuar = 0.5, ncores = 4, Dias_Cuar = 1:15)

Resultados3 <- Modelo_Edad(Inicio = (Inicio + 20), df_out = Resultados2$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 40, K_Cuar = 0, Umbral = 4, ncores = 4, Min_Days = 1)

Results <- list(Resultados$Results, Resultados2$Results, Resultados3$Results) %>% purrr::reduce(bind_rows)

saveRDS(Results, "Results/Magallanes/Results_2020_04_15_K_0_5_Total_15_Magallanes.rds")

## Cuarentena total por 30 dias


Resultados2 <- Modelo_Edad_Total(Inicio = (Inicio + 5), df_out = Resultados$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 30, K_Cuar = 0.5, ncores = 4, Dias_Cuar = 1:30)

Resultados3 <- Modelo_Edad(Inicio = (Inicio + 35), df_out = Resultados2$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 25, K_Cuar = 0, Umbral = 40, ncores = 4, Min_Days = 1)

Results <- list(Resultados$Results, Resultados2$Results, Resultados3$Results) %>% purrr::reduce(bind_rows)

saveRDS(Results, "Results/Magallanes/Results_2020_04_15_K_0_5_Total_30_Magallanes.rds")

### Cuarentena Dinámica


Resultados <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 60, K_Cuar = 0.5, Umbral = 40, ncores = 4, Min_Days = 7)
  

saveRDS(Resultados$Results, "Results/Magallanes/Results_2020_04_15_K_0_5_Dinamico_7_Magallanes.rds")
