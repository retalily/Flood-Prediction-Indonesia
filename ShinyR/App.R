# Load R packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(shinythemes)


newdata <- read.csv("Prediction_all_Prob.csv")


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  " Flood Warning Indonesia",
          
                  tabPanel("Forecast", 
                           sidebarLayout(
                             sidebarPanel(
                               
                               radioButtons("type_select", "Format",
                                            c(" Probabilistic Forecast (%)" = "Columns",
                                              " Map"
                                              ),
                               ),
                               
                               conditionalPanel(
                                 condition = "input.type_select == 'Columns'",
                                 uiOutput("picker"),
                                 actionButton("view", "View Selection")        
                               )
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                               h1('Daily Flood Forecast in Indonesia'),
                               h5("Perkiraan Banjir Harian dalam persen (%)"),
                              
                               DT::dataTableOutput("table"),
                  )
                ),
  
                 
                ),#Navbar1
                
                
                
                tabPanel("Handbook",
                         mainPanel(
                           h1("Forecast Information"),
                           
                           h4("English"),
                           h4("The flood forecast is generated from Machine Learning model, one of AI branches. 
                           We are using Random Forest algorithm to develop the model
                              and predict flood in the future. The accuracy of this model is up to >99%.
                              The flood forecast is presented in Probabilistic Forecast format in percent (%).
                              Moreover, we provides the forecast 2 to 3 months before flood events occur. 
                              We adopted flood warning system recommended by the UK Environment Agency.
                              This system is also used by World Meteorology Organization (WMO). 
                              Please download chart and color codes below for further information. 
                              "),
                           
                           downloadButton("download", "Download"),
                           
                           br(),
                           br(),
                           
                           h4("Bahasa Indonesia"),
                           h4("Prediksi banjir ini dihasilkan dari Machine Learning model, yang merupakan
                               salah satu cabang Artificial Intelligent (AI). Kami menggunakan algoritma Random Forest untuk mengembangkan model
                               dan memprediksi banjir di masa depan. Akurasi model ini hingga >99%.
                               Prakiraan banjir disajikan dalam format Prakiraan Probabilistik dalam persen (%).
                               Selain itu, kami memberikan prakiraan 2 hingga 3 bulan sebelum kejadian banjir terjadi.
                               Kami mengadopsi sistem peringatan banjir yang direkomendasikan oleh Badan Lingkungan Inggris.
                               Sistem ini juga digunakan oleh Organisasi Meteorologi Dunia (World Meteorology Organization/WMO).
                               Silakan download grafik dan kode warna di bawah ini untuk informasi lebih lanjut 
                              "),
                           
                           downloadButton("download1", "Download")
                           
                           
                         ) # mainPanel
                         
                ), # Navbar 2, tabPanel
                
                
                
                tabPanel("Data Explorer",
                         mainPanel(
                           h1("Welcome to Data Explorer"),
                    
                           
                           tags$h5("SORRY! We tried our best to attach all of the data, but file size is too large.
                           Please go to github page below, the page contains the data that are used 
                           to build Flood Forecast model. Feel free to download, all data are free!"
                           ),
                           uiOutput("tab"),
                           
                         
                          
                           
                         ) # mainPanel
                         
                ), # Navbar 3, tabPanel
                
                
#closing page
),

)# fluidPage


library(shiny)
library(DT)



# Server1
server <- function(session, input, output) {
  
  data <- reactive({
    newdata
  })
  
  output$picker <- renderUI({
    pickerInput(inputId = 'pick', 
                label = 'Date and Region(s)',
                choices = colnames(data()),
                options = list(`actions-box` = TRUE),multiple = T)
  })
  
  
  datasetInput <- eventReactive(input$view,{
    
    datasetInput <- data() %>% 
      select(input$pick)
    
    return(datasetInput)
    
  })
  
  
  
  output$table <- DT::renderDataTable(server = FALSE,{
    datatable(
      datasetInput(),
      filter="top", 
      rownames = FALSE,
      extensions = 'Buttons',
      
      options = list(
        dom = 'Blfrtip',
        buttons =
          list('copy', 'print', list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "File", title = NULL),
              list(extend = 'excel', filename = "File", title = NULL)),
            text = 'Download'
          ))
      ),
      class = "display"
    )
  })
  
  
  
  #Server2
  output$downloadData <- downloadHandler(
    filename = "Poster Presentation - Piano Fingering.pdf",
    content = function(file) {
      file.copy("Poster Presentation - Piano Fingering.pdf", file)
    }
  )
  output$download <- downloadHandler(
    filename = "Handbook.pdf",
    content = function(file) {
      file.copy("Handbook.pdf", file)
    }
  )
  
  
  #Server3
  url <- a("Github Data Download", href="https://github.com/retalily/Flood-Prediction-Indonesia")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  
  
  
  
} #closing session



# Create Shiny object
shinyApp(ui = ui, server = server)
