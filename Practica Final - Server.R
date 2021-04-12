#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    data_clase <- reactive({
        data_1 <- data %>%
            dplyr:: filter(as.character(ANIO) == input$clase)
        data_1

    })

   
    data_anio<- reactive({
        df_1 <-data_clase()
        df_1$data_X <- 0
        if(as.numeric(input$clase)== 2011){
            df_1$data_X <- df_1$datos_2011
        }
        if(as.numeric(input$clase)== 2012){
            df_1$data_X <- df_1$datos_2012
        }
        if(as.numeric(input$clase)== 2013){
            df_1$data_X <- df_1$datos_2013
        }
        if(as.numeric(input$clase)== 2014){
            df_1$data_X <- df_1$datos_2014
        }
        if(as.numeric(input$clase)== 2015){
            df_1$data_X <- df_1$datos_2015
        }
        if(as.numeric(input$clase)== 2016){
            df_1$data_X <- df_1$datos_2016
        }
        if(as.numeric(input$clase)== 2017){
            df_1$data_X <- df_1$datos_2017
        }
        if(as.numeric(input$clase)== 2018){
            df_1$data_X <- df_1$datos_2018
        }
        if(as.numeric(input$clase)== 2019){
            df_1$data_X <- df_1$datos_2019
        }
        df_1
    })
    
     cuartiles <- reactive({
         df_2<-data_anio()
         cuartil<-quantile(df_2$data_X,prob=seq(0, 1, 1/4))
         cuartil
     })
     
    segmentacion <- reactive({
        df_4<-data_anio()
        cuart<-cuartiles()
        df_4$clas_PIB<-0
        
        for ( i in 1:length(df_4$data_X)){  
            if (df_4$data_X[i] < cuart[2]) 
            {
                df_4$clas_PIB[i] <-"Muy pobres"
            } 
            if( between(df_4$data_X[i], cuart[2], cuart[3]))
            {
                df_4$clas_PIB[i] <-"Pobres"
            }
            if( between(df_4$data_X[i],cuart[3],cuart[4]))
            {
                df_4$clas_PIB[i] <-"Medios"
            }
            if(df_4$data_X[i] > cuart[4])
            {
                df_4$clas_PIB[i] <-"Ricos"
            }
            
        }
        df_4
    })
    
   coloreado <- reactive({ 
       df_4<- segmentacion()
       df_4$color <- 0
       
       for ( i in 1:length(df_4$data_X)){  
        if (df_4$clas_PIB[i] == "Muy pobres") 
        {
            df_4$color[i] <- 'gray'
        } 
        if(df_4$clas_PIB[i]=="Pobres" )
        {
            df_4$color[i] <- 'cyan'
        }
        if(df_4$clas_PIB[i]=='Medios' )
        {
            df_4$color[i]<- 'red'
        }
        if(df_4$clas_PIB[i]=='Ricos')
        {
            df_4$color[i] <-'yellow'
        }
        
       }
       df_4
   })
    
    data_depart <- reactive({
        df_2<-segmentacion()
        df_3<- df_2%>%
            dplyr::filter(DEPARTAMENTOS == input$departamentos)%>%
            dplyr::filter(ANIO==input$clase)
        
        df_3$para <-0
        if(input$valor=="Desercion"){
            df_3$para<-df_3$DESERCION
        }
        if(input$valor=="Aprobacion"){
            df_3$para<-df_3$APROBACION
        }
        if(input$valor=="Reprobacion"){
            df_3$para<-df_3$REPROBACION
        }   
        if(input$valor=="Repitencia"){
            df_3$para<-df_3$REPITENCIA
        } 
        df_3<- df_3%>%
            dplyr::select(para,data_X)
        df_3
    })
    
    # output$image<- renderUI({
    #     tags$img(src = "www/educacion.png")
    #     
    # })
   
    ########################Definimos la UI##################
    
    output$tipo <- renderUI({
        
        
        radioGroupButtons(
            inputId = "valor",
            label = "Parametro educativo", 
            choices = c("Desercion", "Aprobacion", "Reprobacion","Repitencia"),
            status = "warning"
        )
    })
    output$year <- renderUI({
        pickerInput(
            inputId = 'clase',
            label = 'Curso', 
            choices = sort(unique(data$ANIO))
        )
        
    })
    
    output$departamento <- renderUI({
        pickerInput(
            inputId = 'departamentos',
            label = 'Departamento', 
            choices = sort(unique(data$DEPARTAMENTOS))
        )
        
    })

    ###########################DESERCION##########################33    
    output$plot1<-renderLeaflet({
        
        
        m_prueba <-leaflet()%>% leaflet::addTiles( )%>%
            leaflet:: addProviderTiles(providers$CartoDB.Positron) %>%  
            leaflet::addCircleMarkers(data = coloreado(),
                             lng=~Longitud, lat=~Latitud, 
                             radius = ~(DESERCION*180),
                             color=~coloreado()$color, fill = FALSE,
                             label =~DEPARTAMENTOS,
                             opacity = 0.8)
        
        m_prueba <- m_prueba %>% leaflet::addLegend(data=coloreado(),"bottomleft",colors =~c('gray','cyan','red','yellow'),
                                                        labels=~c("Muy bajo","Bajo","Medio","Alto"), title = 'PIB',
                                                        values = ~clasificacion,
                                                        group="Leyenda")
                                            
        m_prueba
        
    })
    
    #####################################APROBACION###################3
    output$plot2<-renderLeaflet({
        
        m_prueba <-leaflet()%>% addTiles( )%>%
            addProviderTiles(providers$CartoDB.Positron) %>%  
            addCircleMarkers(data = coloreado(),
                             lng=~Longitud, lat=~Latitud, 
                             radius = ~(APROBACION*20),
                             color=~coloreado()$color, fill = FALSE,
                             label =~DEPARTAMENTOS,
                             opacity = 0.8)
        
        m_prueba <- m_prueba %>% leaflet::addLegend(data=coloreado(),"bottomleft",colors =~c('gray','cyan','red','yellow'),
                                                         labels=~c("Muy bajo","Bajo","Medio","Alto"), title = 'PIB',
                                                         values = ~clasificacion,
                                                         group="Leyenda")
        m_prueba
        
    })
    
    
    ##################################REPROBACION######################
    
    output$plot3<-renderLeaflet({
        
        m_prueba <-leaflet()%>% addTiles( )%>%
            addProviderTiles(providers$CartoDB.Positron) %>%  
            addCircleMarkers(data = coloreado(),
                             lng=~Longitud, lat=~Latitud, 
                             radius = ~(REPROBACION*180),
                             color=~coloreado()$color, fill = FALSE,
                             label =~DEPARTAMENTOS,
                             opacity = 0.8)
        
        m_prueba <- m_prueba %>% leaflet::addLegend(data=coloreado(),"bottomleft",colors =~c('gray','cyan','red','yellow'),
                                                        labels=~c("Muy bajo","Bajo","Medio","Alto"), title = 'PIB',
                                                        values = ~clasificacion,
                                                        group="Leyenda")
        m_prueba
        
    })
    
    ################################REPITENCIA########################
    output$plot4<-renderLeaflet({
        
        m_prueba <-leaflet()%>% addTiles( )%>%
            addProviderTiles(providers$CartoDB.Positron) %>%  
            addCircleMarkers(data = coloreado(),
                             lng=~Longitud, lat=~Latitud, 
                             radius = ~(REPITENCIA*180),
                             color=~coloreado()$color, fill = FALSE,
                             label =~DEPARTAMENTOS,
                             opacity = 0.8)
        
        m_prueba <- m_prueba %>% leaflet::addLegend(data=coloreado(),"bottomleft",colors =~c('gray','cyan','red','yellow'),
                                                        labels=~c("Muy bajo","Bajo","Medio","Alto"), title = 'PIB',
                                                        values = ~clasificacion,
                                                        group="Leyenda")
        
        m_prueba
        
    })
    ##############Value boxes################
    output$depart_para <- renderValueBox({
        resultado <- data_depart()
        valueBox(resultado[1]*100, paste('%',input$valor), 
                 icon = icon('graduation-cap'), width = 12)
    })
    
    output$PIB_Depart <- renderValueBox({
        resultado <- data_depart()
        valueBox(trunc(resultado[2]*100), paste('PIB del Departamento en miles de pesos'), 
                 icon = icon('hand-holding-usd'), width = 12)
    })
    #################################################3
    
    output$table <- DT::renderDT({
       
        
        data_filtrada <- data_anio() %>%
            dplyr::select(DEPARTAMENTOS,data_X)
           
        data_filtrada <- rename(data_filtrada,  PIB = data_X)
        data_filtrada$PIB <- trunc(data_filtrada$PIB )
        DT::datatable(data_filtrada) 
    })
    
    #######################################################################################33
    #######Andrea
    #################################################################################
    data2 <- reactive({
        data_1 <- data %>%
            dplyr:: filter(as.character(ANIO) == input$tramos)
        data_1
    })
    
    output$matricula <- renderPlotly({
        df_2 <-data2()
        plot = ggplot(df_2) +
            geom_col(aes(DEPARTAMENTOS, APROBACION, fill = APROBACION)) + 
            theme (axis.text.x = element_text(face="italic",angle=90),
                   #axis.text.y = element_text(face="bold", colour="chocolate4", size=rel(2), angle=90, hjust=0.5)
                   legend.title = element_blank(),
                   legend.position = 'none'
                 )
        
        ggplotly(plot)
    })
    ###########################################################3333
    ########Mariana
    #####################################################3
    
    data_cober <- reactive({
        data_1 <- data %>%
            dplyr:: filter(as.character(ANIO) == input$periodo)
        data_1
        
    })
    
    
    data_curso <- reactive({
        
        data5 <- data_cober()
        
        
        if(input$parametro3=="Transicion"){
            curso <- data5 %>%
                dplyr::select(DEPARTAMENTOS, COBERTURA = COBERTURA_NETA_TRANSICION)
        }
        if(input$parametro3=="Primaria"){
            curso <- data5 %>%
                dplyr::select(DEPARTAMENTOS, COBERTURA = COBERTURA_NETA_PRIMARIA)
        }
        if(input$parametro3=="Secundaria"){
            curso <- data5 %>%
                dplyr::select(DEPARTAMENTOS, COBERTURA = COBERTURA_NETA_SECUNDARIA)
        }   
        if(input$parametro3=="Media"){
            curso <- data5 %>%
                dplyr::select(DEPARTAMENTOS, COBERTURA = COBERTURA_NETA_MEDIA)
        } 
        curso
    })
    
    
    ##################################
    
    output$categoria <- renderUI({
        
        radioGroupButtons(
            inputId = "parametro",
            label = "Seleccione",
            choices = c("Poblacion", "Cobertura"),
            status = "warning"
        )
    })
    
    output$anio <- renderUI({
        
        pickerInput(
            inputId = "periodo",
            label = "Year",
            choices = sort(unique(data$ANIO))
        )
    })
    
    output$cursos <- renderUI({
        
        radioGroupButtons(
            inputId = "parametro3",
            label = "Curso educativo",
            choices = c("Transicion", "Primaria", "Secundaria", "Media"),
            status = "warning"
        )
    })
    
    ##################################
    
    output$plot_1 <- renderPlotly({
        
        
        
        plot_1<- ggplot(data_cober(), aes(fill = DEPARTAMENTOS, x = DEPARTAMENTOS, y = POBLACION_5_16)) +
            geom_col(position = 'dodge', stat = 'identity') +
            theme(legend.position = "none") +
            geom_text(aes(label = POBLACION_5_16), vjust = 2, size = 3)+coord_flip()
       
        ggplotly(plot_1)
    })
    
    
    
    output$plot_2 <- renderPlotly({
        input$parametro
        
        plot_2<-ggplot(data_cober(), aes(fill = DEPARTAMENTOS, x = DEPARTAMENTOS, y = COBERTURA_NETA)) +
            geom_col(position = 'dodge', stat = 'identity') + 
            theme(legend.position = "none") +geom_text(aes(label = POBLACION_5_16), vjust = 2, size = 3) +coord_flip()
        ggplotly(plot_2)
       
    })
    
   
    
    output$table_1 <- DT::renderDT({
        df_7<-data_curso()
        
        DT::datatable(df_7)
        
    })
})


