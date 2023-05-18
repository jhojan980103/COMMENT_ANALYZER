library(shiny)
library(shinydashboard)
library(readxl)
library(rstudioapi)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tm)
library(tidyr)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(igraph)
library(ggraph)
library(tictoc)
library(Rtsne)
library(factoextra)
library(DT)
library(plotly)
library(ggwordcloud)
library(imager)
library(png)
library(shinycssloaders)
library(ClusterR)
library(rgl)
library(syuzhet)
library(reshape2)
library(SnowballC)
library(stringr)
library(quanteda)
library(tidyverse)
library(openxlsx)

#library(reticulate)
# Cargar el modelo entrenado
#py_run_string("import pickle")
#ModeloSentimiento <- py_run_string("with open('ModeloSentimiento.pkl', 'rb') as file: ModeloSentimiento = pickle.load(file)")

# source_python("read_pickle.py")
# ModeloSentimiento <- read_pickle_file("ModeloSentimiento.pkl")

load("objetos_sondeos_app.Rda")
source("FUNCIONES_APP.R")
#load.image("logo_GEDA.png")   

# Define UI for application 
head<-dashboardHeader(title = "Comment Analyzer")

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Datos",
             tabName = "data",icon=icon("th")
    ),
    menuItem("Resultados", 
             tabName = "resultados",icon = icon("chart-line"))
  )
)

body<-dashboardBody(
  tabItems(
    tabItem(tabName = "data",
            fluidRow(width = 2,
                     box(title="Seleccione el archivo (.xlsx)",
                         fileInput("escoger_archivo"," "
                         ),collapsible = T),
                     box( actionButton("limpiar", "Analizar"),
                          downloadButton("base_limpia","Descargar base limpia")),
                     column(width = 12,
                            box(width=12,DT::dataTableOutput("datos_originales")%>% withSpinner(),style="height:auto; overflow-y: scroll;overflow-x: scroll;"
                                ,solidHeader = TRUE, title="Datos", status = "primary")
                     ))
            ,
            tags$hr()
    ),
    tabItem(tabName = "resultados",
            tabBox(
              tabPanel("Palabras",
                       fluidRow(
                         column(width = 6,
                                box(width = 12,title = "Palabras mas frecuentes",
                                    plotOutput ("graf_bar1",height = 280, width = "100%")),
                                box(width = 12,background = "navy",
                                    sliderInput("slider1", "Numero de terminos:", 1, 20, 10))
                         ),
                         column(width = 6,
                                box(width=12,wordcloud2Output("wc.1pal",width="100%")),
                                box(width=12,downloadButton("reporte_unapal","Generar reporte"))
                         ))
              ),
              tabPanel("Bigramas",
                       fluidRow(
                         column(width=6,
                                box(width=12,title = "Bigramas mas frecuentes",
                                    plotOutput ("graf_bar2",height = 280, width = "100%")),
                                box(width=12,background = "navy",
                                    sliderInput("slider2", "Numero de terminos:", 1, 20, 10)
                                )),
                         column(width=6,
                                box(width=12,wordcloud2Output("wc.2pal",width = "100%")),
                                box(width=12,downloadButton("reporte_dpal","Generar reporte"))
                         )
                       )
              ),
              tabPanel("Trigramas",
                       fluidRow(
                         column(width=6,
                                box(width=12,title = "Trigramas mas frecuentes",
                                    plotOutput("graf_bar3",height = 280, width = "100%")),
                                box(width=12,background = "navy",
                                    sliderInput("slider3", "Numero de terminos:", 1, 20, 10))
                         ),
                         column(width=6,
                                box(width=12,wordcloud2Output("wc.3pal",width="100%")),
                                box(width=12,downloadButton("reporte_trespal","Generar reporte"))
                         ))
                       
              ),
              tabPanel("Red de Palabras",height=12,
                       fluidRow(box(width=8,plotOutput("graf_bar6",width = "auto",height = 500)),
                                box(
                                  title = "Frecuencia por palabra",
                                  numericInput("pal.en.red",label = "Palabras que aparecen mas de _ vez",value=1),
                                  downloadButton("reporte_red","Generar reporte (.pdf) "),width = 4))),
              tabPanel("Palabras antes y despues",
                       fluidRow(column(width=6,
                                       box(width=12,plotOutput("graf_bar4",height = 250, width = "100%")),
                                       box(width=12,
                                           textInput("palabra1","Palabras despues de:",value="nivel"),
                                           downloadButton("rep_paldes","Generar reporte"))),
                                column(width=6,
                                       box(width=12,plotOutput("graf_bar5",height = 250, width = "100%")),
                                       box(width=12,
                                           textInput("palabra2","Palabras antes de:",value="nivel"),
                                           downloadButton("rep_palan","Generar reporte"))))),
              tabPanel("Clusterizacion",
                       fluidRow(box(numericInput("n_clus",label = "numero de grupos",value=5),width = 3,height = 2),
                                #box(numericInput("n_pal.c",label = "numero de palabras",value=2),width = 3,height = 2),
                                box(actionButton("clust","Analizar"),width = 1,icon = icon(name="filter")),
                                box(downloadButton("reporte_clus","Generar reporte"),width = 2,height = 2)),
                       fluidRow(#box(
                         #           column(width = 12, plotOutput("graf_clus1",height = 250, width = "100%"),
                         #               DT::dataTableOutput("tbl.cls1"),
                         #               style="height:500px; overflow-y: scroll;overflow-x: scroll;"))#,
                         box(plotlyOutput("graf_clus2",height = 500, width = "auto")),
                         # box(DT::dataTableOutput("tbl.cls2"),
                         #      style="height:500px; overflow-y: scroll;overflow-x: scroll;"),
                         box(plotOutput("word.cloud"))
                       )
              ),
              
              tabPanel("Sentimientos / Emociones",
                       fluidRow(
                         tabBox(
                           tabPanel("Grafica Sentimientos / Emociones",
                                    column(width = 6, plotOutput("sentiment_bar_graph")),
                                    column(width = 6, plotOutput("emotion_bar_graph"))),
                           tabPanel("Reporte Sentimientos / Emociones", 
                                    fluidRow(DT::dataTableOutput("sentiment_table"), downloadButton('downloadData2', 'Descargar Reporte'))),
                           
                           
                           # tabPanel("Tabla sentimientos", 
                           #          fluidRow(DT::dataTableOutput("sentiment_table"))),
                           # 
                           
                           width = 12, height = 550
                         )
                       )),
              width = 12)
    )
  )
)
ui <- dashboardPage(header = head,body = body,sidebar = sidebar)


# Define server logic required to draw a histogram
server <- function(input, output,session){
  
  tabla<-reactiveValues()
  
  output$datos_originales <-renderDataTable({
    inFile <- input$escoger_archivo
    
    if (is.null(inFile))
      return(NULL)
    
    tabla$s<-read_excel(inFile$datapath)
    
  })
  #vals<-reactiveValues()
  observeEvent(input$limpiar,{ 
    withProgress(
      message = 'Analizando',
      detail = 'Esto puede tardar un minuto...', value = 0, {
        
        s<-proc_rtas(tabla$s)
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.25)
        }
      })
    
    rtas_sondeos<-as.data.frame.list(s[1])
    unapal<-as.data.frame.list(s[2])
    dospal<-as.data.frame.list(s[3])
    trespal<-as.data.frame.list(s[4])
    
    output$datos_originales<-renderDataTable({
      datatable(rtas_sondeos[,-3],colnames=c("Respuesta","Respuesta Limpia"), options = list(autoWidth = TRUE))
      
    })
    
################################################################################
    output$graf_bar1<-renderPlot({
      ggplot(data=unapal[1:input$slider1,], aes(y = unapal.freq, x = unapal.palabra)) +
        geom_bar(stat="identity", fill="#2D53BB", alpha=.6, width=.8)+
        coord_flip() +
        xlab("") +
        ylab("Frecuencia")+
        theme_bw()
    })
    
    output$wc.1pal<-renderWordcloud2({
      wordcloud2(unapal[1:input$slider1,], size=0.5, color='random-dark',gridSize = 0.1)
    })
    
################################################################################    
    output$graf_bar2<-renderPlot({
      ggplot(data=dospal[1:input$slider2,], aes(y = dospal.n, x = dospal.bigrams)) +
        geom_bar(stat="identity", fill="#2D53BB", alpha=.6, width=.8)+
        coord_flip() +
        xlab("") +
        ylab("Frecuencia")+
        theme_bw()
      
    })
    output$wc.2pal<-renderWordcloud2({
      wordcloud2(dospal[1:input$slider2,], size=0.5, color='random-dark')
    })
    
################################################################################    
    output$graf_bar3<-renderPlot({
      ggplot(data=trespal[1:input$slider3,], aes(y = trespal.n, x = trespal.trigrams)) +
        geom_bar(stat="identity", fill="#2D53BB", alpha=.6, width=.8)+
        coord_flip() +
        xlab("") +
        ylab("Frecuencia")+
        theme_bw()
    })
    output$wc.3pal<-renderWordcloud2({
      wordcloud2(trespal[1:input$slider3,], size=0.5, color='random-dark')
    })
    ant.des<-reactiveValues()
    observeEvent(input$palabra1,{ 
      ant.des$pal.despues=as.data.frame(palabras.despues.de(rtas_sondeos,palabra = input$palabra1))
    })
    
################################################################################
    output$graf_bar4<-renderPlot({
      pal.despues=as.data.frame(palabras.despues.de(rtas_sondeos,palabra = input$palabra1))
      ggplot(data=pal.despues[1:5,], aes(palabra2, n)) +
        geom_bar(position="dodge",stat="identity",fill="#FF4000") + 
        coord_flip() +
        ggtitle(paste("Palabras seguidas de"),input$palabra1)
    })
    observeEvent(input$palabra2,{
      ant.des$pal.antes=as.data.frame(palabras.antes.de(rtas_sondeos,palabra = input$palabra2))
    })
    
################################################################################
    output$graf_bar5<-renderPlot({
      pal.antes=as.data.frame(palabras.antes.de(rtas_sondeos,palabra = input$palabra2))
      ggplot(data=pal.antes[1:5,], aes(palabra1, n)) +
        geom_bar(position="dodge",stat="identity",fill="#FF4000") + 
        coord_flip() +
        ggtitle(paste("Palabras antes de"),input$palabra2)
    })
    
################################################################################
    output$graf_bar6<-renderPlot({
      red.pal<-data_red(rtas_sondeos = rtas_sondeos,n_pal = input$pal.en.red)
      set.seed(500)
      a <- grid::arrow(type = "closed", length = unit(.08, "inches"),angle = 45)
      ggraph(red.pal, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), 
                       show.legend = F,
                       arrow = a, end_cap = circle(.05, 'inches'),
                       check_overlap = T) +
        geom_node_point(color = "#00AFBB", size = 3) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1, position = "jitter",
                       check_overlap = T,show.legend = F,repel = T) +
        theme_void()
    })
    
########SENTIMIENTOS##################################################################################  
    # Definir la función de limpieza de texto
    clean_text <- function(text) {
      # Puede personalizar esta función para satisfacer sus necesidades
      text <- tolower(text)
      text <- removePunctuation(text)
      text <- removeNumbers(text)
      #text <- removeWords(text, stopwords("spanish"))
      text <- stripWhitespace(text)
      return(text)
    }

    # Limpiar texto y realizar análisis de sentimiento
    clean_text <- sapply(tabla$s[,1], clean_text) # asumiendo que el texto está en la primera columna
    
    sentiment_scores <- get_nrc_sentiment(clean_text)
    
###########################################################################################################    

    # Calcule los puntajes de sentimiento promedio para cada categoría
    sentiment_averages <- sentiment_scores %>% summarise_all(mean, na.rm = TRUE)

    # Split the sentiment averages dataframe into two separate dataframes for sentiment and emotion
    emotion_avg_df <- data.frame(Category = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"),
                                 Average_Score = unlist(sentiment_averages[3:10]),
                                 stringsAsFactors = FALSE)
    
    sentiment_avg_df <- data.frame(Category = c("positive", "negative"),
                                   Average_Score = unlist(sentiment_averages[1:2]),
                                   stringsAsFactors = FALSE)

    # Render a bar graph of sentiment categories (positive and negative)
    output$sentiment_bar_graph <- renderPlot({
      # Traducir los nombres de las categorías de sentimientos a español
      sentiment_avg_df$Category <- recode(sentiment_avg_df$Category,
                                          "positive" = "Positivo",
                                          "negative" = "Negativo")

      ggplot(sentiment_avg_df, aes(x = reorder(Category, -Average_Score), y = Average_Score, fill = Category)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() +
        labs(x = "Categoría de sentimiento", y = "Puntajes promedio", title = "Puntuaciones de sentimiento promedio por categoría") +
        theme(text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = c("Positivo" = "#146C94", "Negativo" = "#ED2B2A"))
    })

    # Render a bar graph of emotion categories
    output$emotion_bar_graph <- renderPlot({
      # Traducir los nombres de las categorías de sentimientos a español
      emotion_avg_df$Category <- recode(emotion_avg_df$Category,
                                          "anger" = "Enojo", "anticipation" = "Anticipación", "disgust" = "Disgusto",
                                          "fear" = "Miedo", "joy" = "Alegría", "sadness" = "Tristeza",
                                          "surprise" = "Sorpresa", "trust" = "Confianza")

      ggplot(emotion_avg_df, aes(x = reorder(Category, -Average_Score), y = Average_Score, fill = Category)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() +
        labs(x = "Categoría de emoción", y = "Puntajes promedio", title = "Puntuaciones promedio de emociones por categoría") +
        theme(text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = c("Enojo" = "#ED2B2A", "Disgusto" = "#ED2B2A", "Miedo" = "#ED2B2A", "Tristeza" = "#ED2B2A", 
                                     "Anticipación" = "#146C94", "Alegría" = "#146C94", "Sorpresa" = "#146C94", "Confianza" = "#146C94"))
    })

#############################################################################################################
    
    # Translation dictionary
    translation_dict <- c("Positive" = "Positivo", "Negative" = "Negativo", "Neutral" = "Neutral", 
                          "anger" = "Enojo", "anticipation" = "Enticipación", "disgust" = "Disgusto",
                          "fear" = "Miedo", "joy" = "Alegría", "sadness" = "Tristeza", "surprise" = "Sorpresa",
                          "trust" = "Confianza", "positive" = "Positivo", "negative" = "Negativo")
    
    # Determine if sentiment is positive, negative, or neutral
    sentiment_category <- ifelse(sentiment_scores$positive > sentiment_scores$negative, "Positive",
                                 ifelse(sentiment_scores$positive < sentiment_scores$negative, "Negative", "Neutral"))
    sentiment_category <- translation_dict[sentiment_category]
    
    # Find the most predominant emotion
    emotion_scores <- sentiment_scores[1:8]
    predominant_emotion <- colnames(emotion_scores)[max.col(emotion_scores, ties.method = "last")]
    predominant_emotion <- translation_dict[predominant_emotion]
    
    ### Aplicar el modelo 'ModeloSentimiento' al texto limpio
    #modelo_results <- py$ModeloSentimiento$predict(clean_text)
    #modelo <- predict(ModeloSentimiento, clean_text)
    
    ### Create a dataframe with cleaned text, sentiment, and predominant emotion
    sentiment_table <- data.frame(Cleaned_Text = clean_text, 
                                  Sentimento = sentiment_category,
                                  Emocion = predominant_emotion,
                                  #Modelo_Resultados = modelo_results,
                                  stringsAsFactors = FALSE)
    
    # Render a table with cleaned text, sentiment, and predominant emotion
    output$sentiment_table <- DT::renderDataTable({
      sentiment_table
    })

    # Download Handler
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(sentiment_table, file)
      }
    )

################################################################################
    output$base_limpia<-downloadHandler(
      filename = function() {
        paste("base_limpia", ".csv", sep = "")
      },
      content = function(file){
        write.csv2(rtas_sondeos,file,row.names = F)}
    )
    
    output$reporte_unapal<-downloadHandler(
      filename = function() {
        paste("unapal", ".csv", sep = "")
      },
      content = function(file){
        write.csv2(unapal,file,row.names = F)}
    )
    
    output$reporte_dpal<-downloadHandler(
      filename = function() {
        paste("dospal", ".csv", sep = "")
      },
      content = function(file3){
        write.csv2(dospal,file3,row.names = F)}
    )
    
    output$reporte_trespal<-downloadHandler(
      filename = function() {
        paste("trespal", ".csv", sep = "")
      },
      content = function(file3){
        write.csv2(trespal,file3,row.names = F)}
    )
    
    output$rep_paldes<-downloadHandler(
      filename = function() {
        paste("pal.despues", ".csv", sep = "")
      },
      content = function(file4){
        write.csv(ant.des$pal.despues,file4,row.names = F)}
    )
    
    output$rep_palan<-downloadHandler(
      filename = function() {
        paste("pal.antes", ".csv", sep = "")
      },
      content = function(file5){
        write.csv(ant.des$pal.antes,file5,row.names = F)}
    )
    
################################################################################
    dims_redu<-reducir_dim(rtas_sondeos)
    
    productos.en.3D<-as.data.frame.list(dims_redu[1])
    n_grupos_opt   <-as.data.frame.list(dims_redu[2])
    
    updateNumericInput(session = session,inputId = "n_clus",value = dims_redu$n_grupos_opt)
    updateTextInput(session = session,inputId = "palabra1",value = unapal$unapal.palabra[1])
    updateTextInput(session = session,inputId = "palabra2",value = unapal$unapal.palabra[1])
    
    observeEvent(input$clust,{
      withProgress(
        message = 'Realizando la agrupacion..',
        value = 0, {
          
          hey<-clusterizar(productos.en.3D=dims_redu$productos.en.3D,
                           n_grupos =input$n_clus,
                           npal_importantes = 2,
                           bow.t=dims_redu$bow.t,
                           rtas_sondeos=rtas_sondeos)
          for (i in 1:10) {
            incProgress(1/10)
            Sys.sleep(0.25)
          }
        })
      
      productos.en.3D<-as.data.frame.list(hey[2])
      
      words_freqs<-as.data.frame.list(hey[3])
      
      output$graf_clus2<-renderPlotly(
        
        plot_ly(productos.en.3D, 
                x = ~productos.en.3D.V1, y = ~productos.en.3D.V2, z = ~productos.en.3D.V3, 
                color = ~as.factor(productos.en.3D.clus), 
                text= (~paste('Grupo:', productos.en.3D.clus, '<br>ID:',productos.en.3D.id_rta,'<br>Rta:',productos.en.3D.rtas_sondeos.rta)),
                colors = "Dark2",hoverinfo = 'text') %>%
          add_markers() %>%
          layout(scene = list(xaxis = list(title = ' ',showticklabels = FALSE),
                              yaxis = list(title = ' ',showticklabels = FALSE),
                              zaxis = list(title = ' ',showticklabels = FALSE)))%>% 
          layout(paper_bgcolor='rgb(238,238,238)')
        
      )
      
      output$reporte_clus<-downloadHandler(
        filename = function() {
          paste("grupos", ".csv", sep = "")
        },
        content = function(file.c){
          write.csv(productos.en.3D[,-c(1,2,3)],file.c,row.names = F)}
      )
      wordcloud_rep <- repeatable(wordcloud)
      output$word.cloud<-renderPlot({
        
        set.seed(4)
        ggplot(
          words_freqs,
          aes(
            label = palabras.frecuentes.palabra, 
            size = palabras.frecuentes.freq,
            x = as.factor(palabras.frecuentes.cluster),
            color = as.factor(palabras.frecuentes.cluster)
          )
        ) +
          geom_text_wordcloud_area(rm_outside = T,area_corr_power = 1) +
          scale_size_area(max_size = 24) +
          scale_x_discrete() + 
          theme(
            panel.background = element_rect(fill = "gray90",
                                            colour = "gray90"))+
          scale_color_brewer(palette="Dark2")+
          facet_wrap(~as.factor(palabras.frecuentes.cluster))

      })
    })
  }) 
  
} 

# Run the application 
shinyApp(ui = ui, server = server)
