#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

prod_commune <- read.csv2("production-electrique-par-filiere-a-la-maille-commune.csv")
prod_region <- read.csv2("production-electrique-par-filiere-a-la-maille-region.csv")
prod_departement <- read.csv2("production-electrique-par-filiere-a-la-maille-departement.csv")

conso_commune <- read.csv2("consommation-electrique-par-secteur-dactivite-commune.csv")
conso_region <- read.csv2("consommation-electrique-par-secteur-dactivite-region.csv")
conso_departement <- read.csv2("consommation-electrique-par-secteur-dactivite-departement.csv")



vec_maille <- c("Commune","Département","Région")
vec_annee <- unique(conso_commune$Année)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bilan électrique de consommation"),
    
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId= "maille d’intérêt",
                      label="choisissez votre maille",
                      choices=vec_maille),
          selectInput(inputId = "annee",
                      label="choisissez votre annee",
                      choices=vec_annee,
                      multiple=TRUE),
          selectInput(inputId = "maille à regarder",
                      label="choisissez vos mailles",
                      choices=vec_maille,
                      multiple=TRUE)
        ),
        
        #Onglet Résumé"
        tabsetPanel(
          tabPanel(title = "Résumé",
                   plotOutput("penguin_plot")
          ),
          tabPanel(title = "Détails",
                   tableOutput("penguin_table")
          )
        )

        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
