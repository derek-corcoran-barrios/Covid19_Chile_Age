## Cuarentena dinamica extención 7 dias por 50 días cuarentena media

Resultados3 <- Modelo_Edad(Inicio = Inicio, df_out = df_out, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 50, K_Cuar = 0.5, Umbral = 40, ncores = 40, Min_Days = 7)


saveRDS(Resultados3, "Results_2020_04_15_K_0_5_Dinamica_7_Inicio_15_Abril_Metropolitana.rds")

message("50 días")




Resultados3_2 <- Modelo_Edad(Inicio = Inicio + 50, df_out = Resultados1$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 20, K_Cuar = 0.5, Umbral = 40, ncores = 40, Min_Days = 7)

Resultados3_Total <- list(Resultados3$Results, Resultados3_2$Results) %>% bind_rows()

Resultados3_Total <- list(Results =Resultados3_Total, starting = Resultados3_2$Starting)


saveRDS(Resultados3_Total, "Results_2020_04_15_K_0_5_Dinamica_7_Inicio_15_Abril_Metropolitana.rds")

message("70 días")


Resultados3_3 <- Modelo_Edad(Inicio = Inicio + 70, df_out = Resultados1_2$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 20, K_Cuar = 0.5, Umbral = 40, ncores = 40, Min_Days = 7)

Resultados3_Total <- list(Resultados3_Total$Results, Resultados3_3$Results) %>% bind_rows()

Resultados3_Total <- list(Results =Resultados3_Total, starting = Resultados3_3$Starting)


saveRDS(Resultados3_Total, "Results_2020_04_15_K_0_5_Dinamica_7_Inicio_15_Abril_Metropolitana.rds")

message("90 días")

Resultados3_4 <- Modelo_Edad(Inicio = Inicio + 90, df_out = Resultados1_3$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 20, K_Cuar = 0.5, Umbral = 40, ncores = 40, Min_Days = 7)

Resultados3_Total <- list(Resultados3_Total$Results, Resultados3_4$Results) %>% bind_rows()

Resultados3_Total <- list(Results =Resultados3_Total, starting = Resultados3_4$Starting)

saveRDS(Resultados3_Total, "Results_2020_04_15_K_0_5_Dinamica_7_Inicio_15_Abril_Metropolitana.rds")

message("110 días")


Resultados3_5 <- Modelo_Edad(Inicio = Inicio + 110, df_out = Resultados1_4$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 20, K_Cuar = 0.5, Umbral = 40, ncores = 40, Min_Days = 7)

Resultados3_Total <- list(Resultados3_Total$Results, Resultados3_5$Results) %>% bind_rows()

Resultados3_Total <- list(Results =Resultados3_Total, starting = Resultados3_5$Starting)

saveRDS(Resultados3_Total, "Results_2020_04_15_K_0_5_Dinamica_7_Inicio_15_Abril_Metropolitana.rds")


message("130 días")


Resultados3_6 <- Modelo_Edad(Inicio = Inicio + 130, df_out = Resultados1_5$Starting, Probs = Probs, betaI = betaI, betaA = betaA, Eta = Eta,K_g = K_g, Alpha_g = Alpha_g, Mu_g = Mu_g, Gamma_g = Gamma_g, Omega_g = Omega_g, Psi_g = Psi_g, Chi_g = Chi_g, Epsilon = Epsilon, p_G = p_G, Sigma = Sigma, C_G_H= C_G_H, Dias = 20, K_Cuar = 0.5, Umbral = 40, ncores = 40, Min_Days = 7)

Resultados3_Total <- list(Resultados3_Total$Results, Resultados3_6$Results) %>% bind_rows()

Resultados3_Total <- list(Results =Resultados3_Total, starting = Resultados3_6$Starting)

saveRDS(Resultados3_Total, "Results_2020_04_15_K_0_5_Dinamica_7_Inicio_15_Abril_Metropolitana.rds")

message("150 días")