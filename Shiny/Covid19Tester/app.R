#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(doParallel)
library(lubridate)

## Cargamos las funciones



source("Funcion.R")
Magallanes <- read_rds("Arica y Parinacota.rds")
Age_Matrix <- read_rds("Age_Matrix.rds")
df_out <- read_rds("df_out_2020-04-17.rds")
Probs<- read_rds("Probs_2020-04-17.rds")
Contactos <- read_rds("Contactos.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$style(
            HTML(".shiny-notification {
             height: 100px;
             width: 800px;
             position:fixed;
             top: calc(50% - 50px);
             left: calc(50% - 400px);
             font-size: 150%;
             text-align: center;
             }
             "
            )
        )
    ),

    # Application title
    titlePanel("Ejemplo de covid 19 Magallanes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(

        sidebarPanel(
            submitButton("Recalcular", icon("refresh")),
            numericInput("Days",
                         "Días a modelar",
                         5),
            checkboxInput("Suspencion", label = "Suspención de clases", value = TRUE),
            sliderInput("Kappa", "Intensidad de la cuarentena:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            selectInput("Type", "Selecciona tipo de estrategia:",
                        choices = c("Cuarentena dinámica", "Cuarentena nacional"),
                        selected = "Cuarentena dinámica"),
            numericInput("Umbral", 
                         "Prevalencia para decretar cuarentena",
                         40),
            numericInput("DayMin",
                         "Número mínimo de días con cuarentena",
                         1),
            sliderInput("Beta_A",
                        "Tasa de contagio de asintomáticos:",
                        min = 0,
                        max = 0.1,
                        value = 0.02),
            sliderInput("Beta_I",
                        "Tasa de contagio de Infectados:",
                        min = 0,
                        max = 0.1,
                        value = 0.04),
            numericInput("Eta",
                         "Número de días promedio de periodo de paso de expuesto a asintomático",
                         value = 2.34),
            numericInput("Alpha_1",
                         "Número de días promedio de periodo de paso de asintomático a infectado en menores de 25",
                         value = 5.06),
            numericInput("Alpha_2",
                         "Número de días promedio de periodo de paso de asintomático a infectado en personas de 25 a 65",
                         value = 2.86),
            numericInput("Alpha_3",
                         "Número de días promedio de periodo de paso de asintomático a infectado en mayres de 65",
                         value = 2.86),
            numericInput("Mu_1",
                         "Número de días promedio de periodo de paso de infectado a recuperado o hospitalizado en menores de 25",
                         value = 1),
            numericInput("Mu_2",
                         "Número de días promedio de periodo de paso de infectado a recuperado o hospitalizado en personas de 25 a 65",
                         value = 3.2),
            numericInput("Mu_3",
                         "Número de días promedio de periodo de paso de infectado a recuperado o hospitalizado en mayres de 65",
                         value = 3.2),
            numericInput("Gamma1",
                        "Proporción de casos que llegan a la UCI en menores de 25",
                        value = 0.002,
                        min = 0,
                        max = 1),
            numericInput("Gamma2",
                         "Proporción de casos que llegan a la UCI en personas de 25 a 65",
                         value = 0.05,
                         min = 0,
                         max = 1),
            numericInput("Gamma3",
                         "Proporción de casos que llegan a la UCI en mayores de 65",
                         value = 0.36,
                         min = 0,
                         max = 1),
            numericInput("Psi",
                         "Número de días promedio desde entrada a la UCI hasta fallecimiento",
                         value = 7),
            sliderInput("Omega",
                        "proporción de casos fatales en UCI",
                        min = 0,
                        max = 1,
                        value = 0.42),
            numericInput("Chi",
                         "Número de días promedio desde entrada a la UCI hasta recuperacion",
                         value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    output$distPlot <- renderPlot({
            
            DF <- Modelo_Edad(Inicio = dmy("17/04/2020"), df_out = df_out, Probs = Probs, betaI = input$Beta_I, betaA = input$Beta_A,  K_g = Contactos, Eta = 1/input$Eta, Alpha_g = c(1/input$Alpha_1,1/input$Alpha_2, 1/input$Alpha_3), Mu_g = c(1/input$Mu_1,1/input$Mu_2, 1/input$Mu_3), Gamma_g = c(input$Gamma1, input$Gamma2, input$Gamma3), Omega_g = input$Omega, Psi_g = 1/input$Psi, Chi_g = 1/input$Chi, Epsilon = 0.01, p_G = c(0.2, 1, 0.2), Sigma = 3.1, C_G_H  = Age_Matrix, Dias = input$Days, K_Cuar = input$Kappa, Umbral = input$Umbral, ncores = ceiling(parallel::detectCores()/2), Min_Days = input$DayMin)  
        
        
        ggplot(DF$Results, aes(x = Fecha, y = Infectados)) + geom_line() + geom_point(aes(color = Cuarentena)) + theme_bw() + facet_wrap(~Comuna, scales = "free_y")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
