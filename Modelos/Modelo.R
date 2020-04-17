library(tidyverse)
library(lubridate)
library(doParallel)

## Cargamos las funciones

source("Modelos/Funcion.R")

Start <- Sys.time()
### Elegir fecha inicial de simulacion

Inicio = dmy("15-04-2020")

### Leer probabilidades de migracion

Probs <- read_rds("Bases_de_datos/Datos_Magallanes/Probs_2020-04-15.rds")

### Leer condiciones iniciales

df_out <- read_rds("Bases_de_datos/Datos_Magallanes/df_out_2020-04-15.rds") 

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

Betas = seq(from = 0.02, to = 0.1, by = 0.02)
Resultados <- list()

for(i in 1:length(Betas)){
  Resultados[[i]] <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = Betas[i], betaA = Betas[i], Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 30, K_Cuar = 0.5, Umbral = 40, ncores = 4)
}

Results <- Resultados %>% purrr::map(~ .x$Results) %>% purrr::map2(.y = Betas, ~mutate(.x, Beta = as.character(.y))) %>% reduce(bind_rows)

#Resultados2 <- Modelo_Edad_Total(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 30, K_Cuar = 0.5, Dias_Cuar = 5:20, ncores = 4)

#Results <- Resultados$Results %>% mutate(Estrategia = "Cuarentena dinámica")
Results <- Results %>% dplyr::filter(Comuna != "pais")

ggplot(Results, aes(x = Fecha, y = Infectados)) + geom_line(aes(color = Beta)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()

Results <-list(
  Resultados[[1]]$Results %>% mutate(PS = as.character(0.5)),
  
  Resultados[[2]]$Results %>% mutate(PS = as.character(0.1)),
  
  Resultados[[3]]$Results %>% mutate(PS = as.character(1))
) %>% reduce(bind_rows)

#Resultados2 <- Modelo_Edad_Total(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 30, K_Cuar = 0.5, Dias_Cuar = 5:20, ncores = 4)

#Results <- Resultados$Results %>% mutate(Estrategia = "Cuarentena dinámica")
Results <- Results %>% dplyr::filter(Comuna != "pais")

ggplot(Results, aes(x = Fecha, y = Infectados)) + geom_line(aes(color = PS)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()


Resultados2 <- Resultados2 %>% mutate(Estrategia = "Cuarentena nacional")

Results <- bind_rows(Resultados, Resultados2)

saveRDS(Results, "Results/Valparaiso/Results_Valparairso_2020_04_16.rds")
write_csv(Results, "Results/Valparaiso/Results_Valparairso_2020_04_16.csv")

Results <- Results %>% dplyr::filter(Comuna != "pais")

ggplot(Results, aes(x = Fecha, y = Infectados)) + geom_line(aes(color = Estrategia)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()


end <- Sys.time()

#saveRDS(Results, "AdaptativoK_0_5_Chile_2020_04_10.rds")
#write_csv(Results, "AdaptativoK_0_5_Chile_2020_04_10.csv")

Results <- Resultados %>% dplyr::filter(Comuna != "pais")

ggplot(Results, aes(x = Fecha, y = Infectados)) + geom_line() + geom_point(aes(color = Cuarentena)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()
ggplot(Results, aes(x = Fecha, y = UCI)) + geom_line() + geom_point(aes(color = Cuarentena)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()

