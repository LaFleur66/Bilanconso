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



prod_commune <- readRDS("prod_commune_clean.rds")
prod_region <- readRDS("prod_region_clean.RDS")
prod_departement <- readRDS("prod_departement_clean.rds")

conso_commune <- readRDS("conso_commune_clean.rds")
conso_region <- readRDS("conso_region_clean.rds")
conso_departement <- readRDS("conso_departement_clean.rds")

list_conso <- list(conso_commune,conso_departement,conso_region)
list_prod <- list(prod_commune,prod_departement,prod_region)

names(list_conso) <- c("commune","departement","region")
names(list_prod) <- c("commune","departement","region")


vec_maille <- c("region","departement","commune")
vec_annee <- unique(conso_region$annee)

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
      selectInput("maille_interet", "Maille d'interet", choices = vec_maille, selected = "region"),
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
      updateSelectInput(session,"annee","annee(s) d'interet",choices = unique(annee), selected = as.character(c("2020","2021")))
      updateSelectInput(session,"maille_vis","mailles",choices = unique((maille_vis)), selected = "moyenne-france")
    })
    
    conso1 <- reactive({ conso() %>% filter(.[[sprintf("nom_%s", input$maille_interet)]] %in% input$maille_vis & annee %in% input$annee)})
    
    prod1 <- reactive({ prod() %>% filter(.[[sprintf("nom_%s", input$maille_interet)]] %in% input$maille_vis & annee %in% input$annee)})
      
      
    # Onglet Résumé 
    
    ## Graphique avec une barre sur la conso et une barre sur la prod
    
    sum_prod <- reactive({ apply(prod1() %>% select(prod_tot), 2, sum, na.rm= T)*0.000001 })
    
    energie <- c("photovoltaique","eolien","hydraulique","cogeneration","bio_energie","autres")
    
    prod_energie <- reactive({ data.frame(cbind(value = as.numeric(sum_prod()),energie = energie)) })
    
    data_barplotconso <- reactive ({
      data_barplot_conso <- conso1() %>% group_by(annee,code_grand_secteur,.[[sprintf("nom_%s", input$maille_interet)]]) %>%
      summarise(consotot=sum(as.numeric(conso_totale_mwh_),na.rm = T)) %>%
      mutate(consototTWH=consotot*0.000001)
      colnames(data_barplot_conso) <-  c("annee","code_grand_secteur",sprintf("nom_%s", input$maille_interet),"consotot","consototTWH") 
      
      data_barplot_conso
      
      })
    
    data_barplotprod <- reactive ({
      data_barplot_prod <- prod1() %>% group_by(annee,type_prod,.[[sprintf("nom_%s", input$maille_interet)]]) %>%
      summarise(prodtot=sum(as.numeric(prod_tot),na.rm = T))%>%
      mutate(prodtotTWH=prodtot*0.000001)
      colnames(data_barplot_prod) <- c("annee","type_prod",sprintf("nom_%s", input$maille_interet),"prodtot","prodtotTWH")
      
      data_barplot_prod
    })
    
    
    plot_bar_conso <- reactive({
      ggplot() +
        geom_bar(data = data_barplotconso(), aes(x="conso", y=consototTWH,fill=code_grand_secteur), stat = 'identity') +
        geom_bar(data = data_barplotprod(),
                 aes(x="prod",y=prodtotTWH,colour = type_prod) 
                 ,stat = 'identity')+
        scale_fill_brewer(palette="Spectral") +
        facet_wrap(as.formula(paste("~", sprintf("nom_%s", input$maille_interet))))+
        labs(title = "Conso et prod par type",x="", y = "Conso et Prod en TWH")
        
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
    
    
    
    plot <- reactive({ggplot() +
      geom_line(data = conso1() %>% group_by(annee,code_grand_secteur)
                %>% summarise(consotot=sum(as.numeric(conso_totale_mwh_),na.rm = T))
                %>% mutate(consototTWH=consotot*0.000001) %>%
                  ungroup(),
                aes(x=annee,y=as.numeric(consototTWH), col = code_grand_secteur)
                )+
      geom_line(data = prod1() 
                  %>% group_by(annee,type_prod)
                  %>% summarise(prodtot=sum(as.numeric(prod_tot),na.rm = T))
                  %>% mutate(prodtotTWH=prodtot*0.000001) %>%
                  ungroup(),
                  aes(x=annee, y = as.numeric(prodtotTWH) , col = type_prod))+
                  labs(title = "Evolution de conso et prod par type",x = "Année", y = "Conso et Prod en TWH")+
                  scale_color_discrete(name = "Type de prod et de conso")  
    })
    
    
    p <- reactive({ggplotly(plot())})
    
    output$plot <- renderPlotly({p()})
    
    
    
    # Onglet Détails
    
    
    ## Dataframe conso
    
    
    conso2 <- reactive({
      conso_table <- conso() %>% 
      filter(.[[sprintf("nom_%s", input$maille_interet)]] %in% input$maille_vis | .[[sprintf("nom_%s", input$maille_interet)]] == "moyenne-france")%>%
      filter(annee %in% input$annee) %>%
      group_by(annee, .[[sprintf("nom_%s", input$maille_interet)]], code_grand_secteur) %>%
      summarise(conso = sum(as.numeric(conso_totale_mwh_)))
      
      colnames(conso_table) <- c("annee",sprintf("nom_%s", input$maille_interet),"code_grand_secteur","conso")
      
      conso_table <- conso_table %>% pivot_wider(names_from = sprintf("nom_%s", input$maille_interet), values_from = "conso") 
      
      conso_table
      
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
    
    prod2 <- reactive({
        prod_table <- prod() %>% 
        filter(.[[sprintf("nom_%s", input$maille_interet)]] %in% input$maille_vis | .[[sprintf("nom_%s", input$maille_interet)]] == "moyenne-france")%>%
        filter(annee %in% input$annee) %>%
        group_by(annee, .[[sprintf("nom_%s", input$maille_interet)]], type_prod) %>%
        summarise(prod = sum(as.numeric(prod_tot)))
      
        colnames(prod_table) <- c("annee",sprintf("nom_%s", input$maille_interet),"type_prod","prod")
       
        prod_table <- prod_table %>% pivot_wider(names_from = sprintf("nom_%s", input$maille_interet), values_from = "prod") 
        
        prod_table
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
