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

