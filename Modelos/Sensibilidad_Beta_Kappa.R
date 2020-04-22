# Con este source obtenemos las funciones

source("Modelos/Funcion.R")

#Luego con este source agregamos las variables

source("Modelos/Modelo.R")

library(tidyverse)
library(lubridate)
library(doParallel)

Start <- Sys.time()
### Elegir fecha inicial de simulacion

Inicio = dmy("13-04-2020")

### Leer probabilidades de migracion

dir.create("Results/Arica y Parinacota")


Beta_Kappa <- expand.grid(Beta_A = seq(0.01, 0.1, length.out = 5),Beta_I = seq(0.01, 0.1, length.out = 5), Kappa = seq(0,0.9, by = 0.1)) %>% dplyr::filter(Beta_I >= Beta_A)



Probs <- read_rds("Bases_de_datos/Datos_Arica y Parinacota/Probs_2020-04-13.rds")

### Leer condiciones iniciales

df_out <- read_rds("Bases_de_datos/Datos_Arica y Parinacota/df_out_2020-04-13.rds") 


Results <- list()

for(i in 1:nrow(Beta_Kappa)){
  Resultados <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = Beta_Kappa$Beta_I[i], betaA = Beta_Kappa$Beta_A[i], Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 5, K_Cuar = Beta_Kappa$Kappa[i], Umbral = 40, ncores = 4, Min_Days = 7)
  Results[[i]] <- Resultados$Results %>% mutate(Beta_I = Beta_Kappa$Beta_I[i], Beta_A = Beta_Kappa$Beta_A[i], Cuar_K = Beta_Kappa$Kappa[i])
  message(paste(i, "of", nrow(Beta_Kappa), "Ready"))
}

Results <- Results %>% reduce(bind_rows)

saveRDS(Results, "Results/Fit/Results.rds")

Results <- read_rds("Results/Fit/Results.rds") %>% dplyr::filter(Comuna == "arica") %>% mutate(Beta_A = as.character(Beta_A), Beta_I = as.character(Beta_I), Cuar_K = as.character(Cuar_K))

githubURL <- ("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv")
download.file(githubURL,"CasosActivosPorComuna.csv", method="curl")


Pais <- read_csv("CasosActivosPorComuna.csv")

ggplot(Results, aes(x = Fecha, y = Infectados, lty = Cuar_K)) + geom_line(aes(color = Beta_I)) + geom_point() + facet_wrap(~Comuna + Beta_A) + theme_bw()

