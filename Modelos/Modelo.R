library(tidyverse)
library(lubridate)
library(doParallel)

Start <- Sys.time()
### Elegir fecha inicial de simulacion

Inicio = dmy("10-04-2020")

### Leer probabilidades de migracion

Probs <- read_rds("Bases_de_datos/Datos_Magallanes/Probs_2020-04-13.rds")

### Leer condiciones iniciales

df_out <- read_rds("Bases_de_datos/Datos_Magallanes/df_out_2020-04-13.rds") 

#prevalencias <- read_rds("Bases_de_datos/Datos_Magallanes/df_out_2020-04-13.rds") %>% bind_rows() %>% group_by(Comuna, Poblacion) %>% summarise(Infectados = sum(Infectados)) %>% mutate(Prevalencia = (Infectados/Poblacion)*100000) %>% ungroup()  %>%  dplyr::pull(Prevalencia) 

## Ponemos cuarentena selectiva por comuna

#for(i in 1:length(df_out)){
#  df_out[[i]]$Prevalencia <- prevalencias
#}


###Testeamos prevalencias para agregar cuarentena



### Obtenemos los nombres de las comunas

#Nombres <- df_out$Under_25$Comuna


#Infectividad de asintomaticos
betaA = 0.06

#Infectividad de Infectados
betaI = 0.06

#numero de contactos promedio
K_g = read_rds("Bases_de_datos/Contactos.rds")

#df_out <- df_out %>% purrr::map2(.y = K_g, ~mutate(.x, K_g = .y))
#rm(K_g)
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

df_out <- df_out %>% purrr::map2(.y = p_G, ~mutate(.x, p_G = .y))
#rm(p_G)
###Tamaño promedio de hogar

Sigma = 3.1


## Matriz de contactos entre generaciones


C_G_H <- read_rds("Bases_de_datos/Age_Matrix.rds")



#Mat  <- matrix(rep(0,(length(Nombres)*length(Nombres))), nrow = length(Nombres), ncol = length(Nombres))

#colnames(Mat) <- Nombres
#rownames(Mat) <- Nombres

#Func <- function(x, epsilon = 0.01){
#  y <- 1 + (1- exp(-epsilon*x))
#  return(y)
#}




#Para cada generacion

#cl <- makeCluster(4)

#registerDoParallel(cl)


#for(x in 1:3){
#  #creamos la columna a llenar 
#  df_out[[x]]$n_i_g_eff <- 0
#  #Para cada Comuna
#  for(R in 1:nrow(df_out[[x]])){
#    n_i_g_eff <- foreach(i = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
#      ((1 - df_out[[x]]$p_G[i])*ifelse(Nombres[i] == df_out[[x]][R,]$Comuna, 1, 0) + df_out[[x]]$p_G[i]*Probs %>% dplyr::filter(destino == df_out[[x]][R,]$Comuna) %>% pull(Nombres[i]))*(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[i]) %>% pull(Generacion))
#    }
#    df_out[[x]][R,]$n_i_g_eff <- sum(n_i_g_eff) 
#  }
#}

#stopCluster(cl)

# n_i_eff <- data.frame(Comuna = df_out$Adult$Comuna, n_i_eff = df_out[[1]]$n_i_g_eff + df_out[[2]]$n_i_g_eff+ df_out[[3]]$n_i_g_eff)

# z_g <- vector()

# for(x in 1:length(df_out)){
#  z_g[x] <- (df_out[[x]] %>% summarise(N = sum(Generacion)) %>% pull(N))/sum(Func(n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*df_out[[x]]$n_i_g_eff)
# }

# df_out <- df_out %>% purrr::map(~mutate(.x, K_0 = case_when(Prevalencia >= 40 ~ 0.5, Prevalencia < 40 ~ 0)))

# df_out <- df_out %>% purrr::map(~mutate(.x, K_g = (1 - K_0)*K_g + K_0*(Sigma - 1)))


#Div3 <- function(x){
#  return(x/3)
#}

##########################################
###########################################
####Loop debiera partir desde acá##########
###########################################
###########################################



#Results <- list()

#Results[[1]] <- df_out %>% bind_rows() %>% dplyr::select(-Poblacion, -n_i_g_eff) %>% rename(Poblacion = Generacion) %>% group_by(Comuna) %>% summarise_if(is.numeric, sum) %>% mutate_at(vars(Time:K_0), Div3)

#Dias <- 30

# for(d in 2:Dias){
#   
#   Num <- list()
#   Denom <- list()
#   for(g in 1:3){
#     Num[[g]] <- ((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion) + (df_out[[g]]$Recuperados/df_out[[g]]$Generacion))*df_out[[g]]$Generacion
#     Denom [[g]]<- sum(df_out[[g]]$Generacion)
#   }
#   CH_i_tc <- purrr::reduce(Num, `+`)/purrr::reduce(Denom, `+`)^Sigma
#   
#   df_out  <- df_out %>% purrr::map(~mutate(.x, p_G = (1 - K_0)*p_G)) 
#   
#   df_out <- df_out %>% purrr::map(~mutate(.x, K_g = (1 - K_0)*K_g + K_0*(Sigma - 1)))
#   
#   #Para cada generacion
#   cl <- makeCluster(4)
#   
#   registerDoParallel(cl)
#   
#   
#   for(x in 1:3){
#     #creamos la columna a llenar 
#     df_out[[x]]$n_i_g_eff <- 0
#     #Para cada Comuna
#     for(R in 1:nrow(df_out[[x]])){
#       n_i_g_eff <- foreach(i = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
#         ((1 - df_out[[x]]$p_G[i])*ifelse(Nombres[i] == df_out[[x]][R,]$Comuna, 1, 0) + df_out[[x]]$p_G[i]*Probs %>% dplyr::filter(destino == df_out[[x]][R,]$Comuna) %>% pull(Nombres[i]))*(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[i]) %>% pull(Generacion))
#       }
#       df_out[[x]][R,]$n_i_g_eff <- sum(n_i_g_eff) 
#     }
#   }
#   
#   
#   
#   n_i_eff <- data.frame(Comuna = df_out$Adult$Comuna, n_i_eff = df_out[[1]]$n_i_g_eff + df_out[[2]]$n_i_g_eff+ df_out[[3]]$n_i_g_eff)
#   
#   z_g <- vector()
#   
#   for(x in 1:length(df_out)){
#     z_g[x] <- (df_out[[x]] %>% summarise(N = sum(Generacion)) %>% pull(N))/sum(Func(n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*df_out[[x]]$n_i_g_eff)
#   }
#   
#   N_I_h_j_i <- list(Mat, Mat, Mat)
#   names(N_I_h_j_i) <- c("Under_25", "Adult", "Over_65")
#   
#   N_A_h_j_i <- list(Mat, Mat, Mat)
#   names(N_A_h_j_i) <- c("Under_25", "Adult", "Over_65")
#   
#   
#   
#   for(x in 1:length(N_I_h_j_i)){
#     for(i in 1:length(Nombres)){
#       Temp <- foreach(j = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
#         (df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion))*((df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Infectados))/(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion)))*((1-df_out[[x]]$p_G[i])*0 + df_out[[x]]$p_G[i]*Probs %>% dplyr::filter(destino == Nombres[i]) %>% pull(Nombres[j]))
#       }
#       N_I_h_j_i[[x]][,i] <- Temp
#     }
#   }
#   
#   
#   
#   
#   for(x in 1:length(N_A_h_j_i)){
#     for(i in 1:length(Nombres)){
#       Temp <- foreach(j = 1:length(Nombres), .combine=c, .inorder = T, .packages = c("dplyr")) %dopar%{
#         (df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion))*((df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Asintomaticos))/(df_out[[x]] %>% dplyr::filter(Comuna == Nombres[j]) %>% pull(Generacion)))*((1-df_out[[x]]$p_G[j])*0 + df_out[[x]]$p_G[j]*Probs %>% dplyr::filter(destino == Nombres[i]) %>% pull(Nombres[j]))
#       }
#       N_A_h_j_i[[x]][,i] <- Temp 
#     }
#   }
#   
#   
#   
#   for(g in 1:3){
#     df_out[[g]]$P_G <- NA
#     Temp2 <- list()
#     for(h in 1:3){
#       temp <- foreach(j = 1:nrow(df_out[[g]]), .inorder = T, .packages = c("dplyr")) %dopar% {
#         (1 - betaA)^(z_g[g]*df_out[[g]]$K_g[j]*Func(x = n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*C_G_H[g,h]*(N_A_h_j_i[[h]][j,]/df_out[[h]]$n_i_g_eff))*(1 - betaI)^(z_g[g]*df_out[[g]]$K_g[j]*Func(x = n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*C_G_H[g,h]*(N_I_h_j_i[[h]][j,]/df_out[[h]]$n_i_g_eff))
#       }
#       Temp2[[h]] <- purrr::reduce(temp, `*`)
#     }
#     
#     df_out[[g]]$P_G <- 1 - purrr::reduce(Temp2, `*`)
#   }
#   
#   
#   
#   for(g in 1:3){
#     df_out[[g]]$PI = NA
#     df_out[[g]]$PI <-  foreach(i = 1:nrow(df_out[[g]]), .combine=c, .inorder = T, .packages = c("dplyr", "purrr")) %dopar%{
#       df_out[[g]]$p_G[i]*(df_out[[g]] %>% dplyr::filter(Comuna == Nombres[i]) %>% pull(P_G)) + df_out[[g]]$p_G[i]*sum((Probs %>% dplyr::filter(destino == Nombres[i]) %>% select_if(is.numeric) %>% reduce(c))*df_out[[g]]$P_G)
#     }
#   }
#   
#   stopCluster(cl)
#   ###
#   
#   
#   
#   Temp1 <- df_out
#   for(g in 1:3){
#     
#     Temp1[[g]]$Suceptibles <- df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1-df_out[[g]]$PI)*(1 - df_out[[g]]$K_0*CH_i_tc))
#     Temp1[[g]]$Expuestos <- df_out[[g]]$Generacion*(((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*df_out[[g]]$PI) + (1 - Eta)*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion))
#     Temp1[[g]]$Cuarentenados <- df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*df_out[[g]]$K_0*CH_i_tc) 
#     Temp1[[g]]$Asintomaticos <- df_out[[g]]$Generacion*((Eta*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)) + (1 - Alpha_g[g])*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion))
#     Temp1[[g]]$Infectados <- df_out[[g]]$Generacion*((Alpha_g[g]*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion)) + (1 - Mu_g[g])*(df_out[[g]]$Infectados/df_out[[g]]$Generacion))
#     Temp1[[g]]$UCI <-  df_out[[g]]$Generacion*(Mu_g[g]*Gamma_g[g]*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + Omega_g*(1 - Psi_g)*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (1 - Omega_g)*(1 - Chi_g)*(df_out[[g]]$UCI/df_out[[g]]$Generacion))
#     Temp1[[g]]$Muerto <-  df_out[[g]]$Generacion*(Omega_g*Psi_g*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Muerto/df_out[[g]]$Generacion))
#     Temp1[[g]]$Recuperados <-  df_out[[g]]$Generacion*(Mu_g[g]*(1 - Gamma_g[g])*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + (1 - Omega_g)*Chi_g*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Recuperados/df_out[[g]]$Generacion))
#   }
#   
#   df_out <- Temp1
#   prevalencias <- df_out %>% bind_rows() %>% group_by(Comuna, Poblacion) %>% summarise(Infectados = sum(Infectados)) %>% mutate(Prevalencia = (Infectados/Poblacion)*100000) %>% ungroup() %>%  dplyr::pull(Prevalencia)
#   
#   for(i in 1:length(df_out)){
#     df_out[[i]]$Prevalencia <- prevalencias
#   }
#   
#   K <- df_out %>% bind_rows() %>% select(Comuna, Generacion, Infectados) %>% group_by(Comuna) %>% summarise_if(is.numeric, sum) %>% mutate(Prev = (Infectados/Generacion)*100000, K_0 = case_when(Prev >= 40 ~ 0.5, Prev< 40 ~ 0)) %>% pull(K_0)
#   
#   for(i in 1:length(df_out)){
#     df_out[[i]]$K_0 <- K
#   }
#   
#   
#   Results[[d]] <- df_out %>% bind_rows() %>% dplyr::select(-Poblacion, -n_i_g_eff, -P_G, -PI) %>% rename(Poblacion = Generacion) %>% group_by(Comuna) %>% summarise_if(is.numeric, sum)%>% mutate_at(vars(Time:K_0), Div3) %>% mutate(Time = d)
# 
#   print(paste("Día", d, "de", Dias, "Listos!", Sys.time()))
#   
# }
# 
# 
# Results <- bind_rows(Results) %>% mutate(Fecha = Inicio + (Time-1))   %>% mutate(Cuarentena = case_when(K_0 > 0~ "Cuarentena", K_0 == 0~ " Sin cuarentena"))

#end <- Sys.time()

saveRDS(Results, "AdaptativoK_0_5_Chile_2020_04_10.rds")
write_csv(Results, "AdaptativoK_0_5_Chile_2020_04_10.csv")

Results <- Results %>% dplyr::filter(Comuna != "Pais")

ggplot(Results, aes(x = Fecha, y = Infectados)) + geom_line() + geom_point(aes(color = Cuarentena)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()
ggplot(Results, aes(x = Fecha, y = UCI)) + geom_line() + geom_point(aes(color = Cuarentena)) + facet_wrap(~Comuna, scales = "free_y") + theme_bw()

Maxs <- Results %>% group_by(Comuna) %>%  dplyr::filter(UCI == max(UCI)) %>% mutate(Prop_UCI = (UCI/Poblacion)*100000) %>% dplyr::select(Comuna, UCI, Prop_UCI, Fecha)  %>% arrange(desc(Prop_UCI))

#write_csv(Maxs, "Maximo_UCI.csv")

MaxsI <- Results %>% group_by(Comuna) %>%  dplyr::filter(Infectados == max(Infectados)) %>% mutate(Prop_I = (Infectados/Poblacion)*100000) %>% dplyr::select(Comuna, Infectados, Prop_I, Fecha)  %>% arrange(desc(Prop_I)) 

#write_csv(MaxsI, "Maximo_Infectados.csv")

MaxsD <- Results %>% group_by(Comuna) %>%  dplyr::filter(Muerto == max(Muerto)) %>% mutate(Prop_D = (Muerto/Poblacion)*100000) %>% dplyr::select(Comuna, Muerto, Prop_D, Fecha)  %>% arrange(desc(Prop_D)) 



