#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui <- dashboardPage(skin='yellow',
    
    dashboardHeader(title = "Educacion Colombia"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Introduccion', tabName = 'intro', icon = icon('hands-helping')),
            menuItem("PIB vs parametros educativos", tabName = "pib", icon = icon("globe-americas")),
            menuItem("Aprobacion por Departamentos", tabName = "aprobacion", icon = icon("check")),
            menuItem("Cobertura vs Población", tabName='cobertura',icon = icon("graduation-cap"))
        )
    ),
    
    dashboardBody(
        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #ffd690;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #ffd690;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #ffd690;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #ffd690;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #ffd690;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #ffd690;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #ffd690;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ffd690;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffd690;
                                }

                                '))),
        # Tab de introduccion ----
        
        tabItems(
            tabItem(tabName = 'intro',
                    
                    fluidRow(
                        column(12, offset=4,
                               h1('Descripcion del trabajo'))
                    ),
                    fluidRow(
                        column(12, offset=0,
                               h4('El objetivo del presente trabajo es mostrar los datos que
                                  representan el estado de la educacion en Colombia. Para ello
                                  hemos realizado las tres siguentes pestañas:'),
                               h4('1ª- Se incluye la representación en un mapa de los diferentes
                                  parametros educativos, los cuales vienen representados por los
                                  diametros de los circulos, frente a los valores del PIB de cada
                                  Departamento, clasificados por colores.'),
                               h4('2ª- Representación mediante un diagrama de barras de la tasa de
                                  aprobacion en cada Departamento, en función del año.'),
                               h4('3ª- Representación mediante un diagrama de barras de la poblacion
                                  que deberia estar escolarizada (poblacion) frente a la que
                                  verdaderamente lo esta (cobertura).'),
                               h3('Trabajo presentado por:'),
                               h4('-Laura Maria Benitez'),
                               h4('-Mariana Benjumea'),
                               h4('-Andrea Calvo')
                        )
                                )
                
            ),
           
            tabItem(tabName ="pib",
                         fluidRow(
                             column(7,
                                    uiOutput('tipo')
                             ),
                             column(5,
                                    uiOutput('year')
                             )

                         ),
                    fluidRow(
                        column(4,
                               valueBoxOutput("depart_para",width = 12)),
                        column(3,
                               uiOutput('departamento')),
                        column(4,
                               valueBoxOutput("PIB_Depart",width=12))
                    ),
                         fluidRow(
                             column(7,
                                    box(title= paste("Representacion grafica") ,solidHeader = T,width = 12,
                                        
                                        conditionalPanel("input.valor=='Desercion'",
                                                         leafletOutput("plot1")),
                                        conditionalPanel("input.valor=='Aprobacion'",
                                                         leafletOutput("plot2")),
                                        conditionalPanel("input.valor=='Reprobacion'",
                                                         leafletOutput("plot3")),
                                        conditionalPanel("input.valor=='Repitencia'",
                                                         leafletOutput("plot4")),
                                        collapsible = TRUE,collapsed = TRUE
                                    )
                             ),
                             column(5,
                                    box(title= paste("PIB por Departamentos") ,solidHeader = T, width = 12,
                                        dataTableOutput("table"),collapsible = TRUE,collapsed = TRUE
                                    ))
                         )
                            ),
                 tabItem(tabName = "aprobacion",
                         fluidRow(
                             column(12, 
                                    box(width = 12 ,color="yellow",
                                        solidHeader = TRUE, 
                                        title = 'Curso escolar',
                                        collapsible = FALSE, 
                                        collapsed = FALSE, 
                                        radioGroupButtons(
                                            inputId = "tramos",
                                            label = "Year",
                                            choices = sort(unique(data$ANIO)),
                                            status = "warning"
                                        )
                                        
                         ))), 
                         fluidRow(
                             column(12, 
                                    plotlyOutput("matricula"))
                         )
                         ),
                   tabItem(tabName = "cobertura",
                           fluidRow(
                               column(3,
                                      uiOutput("categoria")
                               ),
                               column(3,
                                      uiOutput("anio")
                               ),
                              column(6,
                                      uiOutput("cursos")
                               )
                           ),
                           fluidRow(
                                column(7,
                                       box(title = "Representacion Grafica",
                                           solidHeader = TRUE, 
                                          width = 12,
                                           collapsible = FALSE,
                                           collapsed = FALSE,
                                           conditionalPanel("input.parametro=='Poblacion'",
                                                            plotlyOutput("plot_1")),
                                           conditionalPanel("input.parametro=='Cobertura'",
                                                            plotlyOutput("plot_2"))
                                       )
                                ),
                               column(5,
                                       box(title = "Cobertura por curso",
                                          solidHeader = TRUE, 
                                          width = 12,
                                          collapsible = FALSE,
                                           collapsed = FALSE,
                                           dataTableOutput("table_1")
                                      )
                              )
                           )
                   
                              )
                 )
        
    )
)

