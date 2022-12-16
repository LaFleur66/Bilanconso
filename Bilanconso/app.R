#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggnewscale)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(tidyr)

prod_commune <- read.csv2("production-electrique-par-filiere-a-la-maille-commune.csv")
prod_region <- read.csv2("production-electrique-par-filiere-a-la-maille-region.csv")
prod_departement <- read.csv2("production-electrique-par-filiere-a-la-maille-departement.csv")

conso_commune <- read.csv2("consommation-electrique-par-secteur-dactivite-commune.csv")
conso_region <- read.csv2("consommation-electrique-par-secteur-dactivite-region.csv")
conso_departement <- read.csv2("consommation-electrique-par-secteur-dactivite-departement.csv")

cleanames <- function(x){
  p<-colnames(x)
  p<-str_to_lower(p)
  p<-str_trim(p)
  p<-str_replace_all(p, pattern = "[éèê]+", replacement = "e")
  p<-str_replace_all(p, pattern = "[.]+", replacement = "_")
  colnames(x)<-p
  return(x)
} 

prod_commune <- cleanames(prod_commune)
prod_region <- cleanames(prod_region)
prod_departement <- cleanames(prod_departement)

conso_commune <- cleanames(conso_commune)
conso_region <- cleanames(conso_region)
conso_departement <- cleanames(conso_departement)


cleanames_column <- function(x){
  p <- x
  p<-str_to_lower(p)
  p<-str_trim(p)
  p<-str_replace_all(p, pattern = "[éèê]+", replacement = "e")
  p<-str_replace_all(p, pattern = "[.]+", replacement = "_")
  p<-str_replace_all(p, pattern = " ", replacement = "-")
  p<-str_replace_all(p, pattern = "î", replacement = "i")
  p<-str_replace_all(p, pattern = "ô", replacement = "o")
  return(p)
}

prod_commune$nom_commune <- cleanames_column(prod_commune$nom_commune)
prod_region$nom_region <- cleanames_column(prod_region$nom_region)
prod_departement$nom_departement<- cleanames_column(prod_departement$nom_departement)

conso_commune$nom_commune <- cleanames_column(conso_commune$nom_commune)
conso_region$nom_region <- cleanames_column(conso_region$nom_region)
conso_departement$nom_departement <- cleanames_column(conso_departement$nom_departement)




list_conso <- list(conso_commune,conso_departement,conso_region)
list_prod <- list(prod_commune,prod_departement,prod_region)

names(list_conso) <- c("commune","departement","region")
names(list_prod) <- c("commune","departement","region")


vec_maille <- c("region","departement","commune")
vec_annee <- unique(conso_commune$annee)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Bilan électrique"),
    
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Résumé", tabName = "resume", icon = icon("dashboard")),
        menuItem("Détail", tabName = "details", icon = icon("th"))
      ),
      selectInput("maille_interet", "Maille d'interet", choices = vec_maille),
      selectInput("annee", "Années d'interet", multiple = T, choices = NULL),
      selectInput("maille_vis","Maille",multiple = T, choices = NULL)
    ),
    
    
    ## Body content
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "resume",
                fluidRow(
                  box(plotOutput("bar_conso")),
             
                  shinydashboard::valueBoxOutput("vbox_prod", width = 2),
                
                  shinydashboard::valueBoxOutput("vbox_conso",width = 2),
                  
                  box(plotlyOutput("plot"))
                  
                  )
                ),
        # Second tab content
        tabItem(tabName = "details",
                fluidRow(
                  column(
                    8,
                    
                    dataTableOutput("conso"),
                    dataTableOutput("prod")
                    
                  )),
                  fluidRow(
                    column(
                      4,
                      downloadButton("downloadDataConso", "Conso"),
                      downloadButton("downloadDataProd", "Prod"))
                    )
                    
                    
                  )
                  
                  
                  
                )  
                
              )
)


# Define server logic required to draw a histogram
server <- function(session,input, output) {
  
    conso <- reactive({ list_conso[[input$maille_interet]] })
    
    
    prod <- reactive({ list_prod[[input$maille_interet]] })
    
    observe({
      annee <- conso() %>% dplyr::select(annee)
      maille_vis<- conso() %>% dplyr::select(sprintf("nom_%s", input$maille_interet))
      updateSelectInput(session,"annee","annee(s) d'interet",choices = unique(annee))
      updateSelectInput(session,"maille_vis","mailles",choices = unique((maille_vis)))
    })
    
    conso1 <- reactive({ conso() %>% filter(.[[sprintf("nom_%s", input$maille_interet)]] %in% input$maille_vis & annee %in% input$annee)})
    
    prod1 <- reactive({ prod() %>% filter(.[[sprintf("nom_%s", input$maille_interet)]] %in% input$maille_vis & annee %in% input$annee)})
      
      
    # Onglet Résumé 
    
    ## Graphique avec une barre sur la conso et une barre sur la prod
  
    t1 <- reactive({ apply(prod1()
                     %>% select(contains("mwh")),
                     MARGIN = 2, FUN = as.numeric ) 
                  })
    
    sum_prod <- reactive({ apply(t1(), 2, sum, na.rm= T)*0.000001 })
    
    energie <- c("photovoltaique","eolien","hydraulique","cogeneration","bio_energie","autres")
    
    prod_energie <- reactive({ data.frame(cbind(value = as.numeric(sum_prod()),energie = energie)) })
    
    plot_bar_conso <- reactive({
      ggplot(conso1() %>% group_by(annee,code_grand_secteur)
             %>% summarise(consotot=sum(as.numeric(conso_totale_mwh_),na.rm = T))
             %>% mutate(consototTWH=consotot*0.000001)) +

        geom_bar(aes(x="conso", y=consototTWH,fill=code_grand_secteur), stat = 'identity') +
        geom_bar(data = prod_energie(),
                 aes(x="prod",y=as.numeric(value),colour = energie) 
                 ,stat = 'identity')+
        scale_fill_brewer(palette="Spectral") 
    })
    
    output$bar_conso <- renderPlot({plot_bar_conso()})
    
    ## ValueBox 
    
    output$vbox_prod <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(round(sum(sum_prod()),2),
               "Production totale sur la période en TWH",
               color = "red")
    })
    

    conso_numeric <- reactive({ apply( conso1() %>% select(conso_totale_mwh_) ,
                                 MARGIN = 2, as.numeric)}) 
    
    sum_conso <- reactive({ apply( conso_numeric() ,
                                   MARGIN = 2, sum, na.rm= T)*0.000001 }) 
    
    
    output$vbox_conso <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(round(sum_conso(),2),
                               "Consommation totale sur la période en TWH",
                               color = "yellow")
    })
    
    
    ## Graphique interactif de l’évolution des consos par segment et des productions
    ## par segment selon l’année
    
    product <- reactive({ prod1() %>%
      select("annee", sprintf("nom_%s", input$maille_interet),
             sprintf("code_%s", input$maille_interet),
             starts_with("energie_produite")) %>%
      tidyr::pivot_longer(- c("annee", sprintf("nom_%s", input$maille_interet), sprintf("code_%s", input$maille_interet)),
                          values_to = "prod_mwh",
                          names_to = "type") %>%
      mutate(type = type %>%
               stringr::str_replace_all(pattern = "energie_produite_annuelle_|mwh|__", replacement = ""))
    })
    
    plot <- reactive({ggplot( conso1() %>% group_by(annee,code_grand_secteur)
                   %>% summarise(consotot=sum(as.numeric(conso_totale_mwh_),na.rm = T))
                   %>% mutate(consototTWH=consotot*0.000001))+
      aes(x=annee,y=as.numeric(consototTWH), col = code_grand_secteur)+
      geom_line()+
      geom_line(data = product() 
                  %>% group_by(annee,type)
                  %>% summarise(prodtot=sum(as.numeric(prod_mwh),na.rm = T))
                  %>% mutate(prodtotTWH=prodtot*0.000001),
                  aes(x=annee, y = as.numeric(prodtotTWH) , col = type))+
        scale_fill_brewer(palette="Spectral") 
    })
    
    
    p <- reactive({ggplotly(plot())})
    
    output$plot <- renderPlotly({p()})
    
    
    
    # Onglet Détails
    
    
    ## Dataframe conso
    
    conso2 <- reactive({conso1() %>% 
      group_by_(sprintf("nom_%s", input$maille_interet), "code_grand_secteur", "annee") %>%
      summarise(conso = sum(as.numeric(conso_totale_mwh_))) %>% 
      pivot_wider(names_from = sprintf("nom_%s", input$maille_interet), values_from = "conso") 
    })
    
    output$conso <- renderDataTable(conso2())
    
    ### Bouton download
    
    output$downloadDataConso <- downloadHandler(
      filename = function() {
        paste("data-conso", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(conso2(), file)
      }
    )
    
    
    ## Dataframe prod
    
    prod2 <- reactive({product() %>% 
        group_by_(sprintf("nom_%s", input$maille_interet), "type", "annee") %>%
        summarise(prod = sum(as.numeric(prod_mwh))) %>% 
        pivot_wider(names_from = sprintf("nom_%s", input$maille_interet), values_from = "prod") 
    })
    
    
    output$prod <- renderDataTable(prod2())
    
    output$downloadDataProd <- downloadHandler(
      filename = function() {
        paste("data-prod", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(prod2(), file)
      }
    )
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
