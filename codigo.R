
----------------INFORMACION-----------------

Loreto: Loreto Garcia Tejada
Titulación: Doble grado en II + ADE - 2019/2020
Asignatura: Macroeconomía Avanzada y Aplicada ( Economía mundial, española y regional)
Versión: 2.0
x

IMPORTANTE:
En dos puntos del codigo hay que cambiar la direccion de descarga a donde se encuentre el documento en el ordenador 

Variables analizadas en este documento

Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos
Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad
Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma
Variable Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma


http://www.ine.es/jaxiT3/Tabla.htm?t=4932&L=0
http://www.ine.es/jaxiT3/Tabla.htm?t=4934&L=0
http://www.ine.es/jaxiT3/Tabla.htm?t=4933&L=0
http://www.ine.es/jaxiT3/Tabla.htm?t=4935&L=0
----------------IMPORTACION DE LOS DATOS-----------------

```{r}


library(pxR)
library(shiny)


#Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos

activos_valores_Abs<- read.px("http://www.ine.es/jaxiT3/files/t/es/px/4932.px?nocab=1")
activos_valores_Abs <- as.data.frame(activos_valores_Abs)

#Eliminamos el numero en las comunidades autonomas 
activos_valores_Abs[,3]<- gsub("[0-9]+", "",activos_valores_Abs[,3])
names(activos_valores_Abs)[3]<-paste("Comunidades")
names(activos_valores_Abs)[5]<-paste("Valor")



#Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad

Activos_Porcentajes_respecto_total<- read.px("http://www.ine.es/jaxiT3/files/t/es/px/4934.px?nocab=1")
Activos_Porcentajes_respecto_total <- as.data.frame(Activos_Porcentajes_respecto_total)

#Eliminamos el numero en las comunidades autonomas 
Activos_Porcentajes_respecto_total[,3]<- gsub("[0-9]+", "",Activos_Porcentajes_respecto_total[,3])
names(Activos_Porcentajes_respecto_total)[3]<-paste("Comunidades")
names(Activos_Porcentajes_respecto_total)[5]<-paste("Valor")



#Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma

Tasas_actividad<- read.px("http://www.ine.es/jaxiT3/files/t/es/px/4933.px?nocab=1")
Tasas_actividad <- as.data.frame(Tasas_actividad)

#Eliminamos el numero en las comunidades autonomas 
Tasas_actividad[,3]<- gsub("[0-9]+", "",Tasas_actividad[,3])
names(Tasas_actividad)[3]<-paste("Comunidades")
names(Tasas_actividad)[5]<-paste("Valor")



#Variable Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma

activos_vabs<- read.px("http://www.ine.es/jaxiT3/files/t/es/px/4935.px?nocab=1")
activos_vabs <- as.data.frame(activos_vabs)

#Eliminamos el numero en las comunidades autonomas 
activos_vabs[,4]<- gsub("[0-9]+", "",activos_vabs[,4])
names(activos_vabs)[4]<-paste("Comunidades")
names(activos_vabs)[5]<-paste("Valor")

años1<-  activos_valores_Abs[ activos_valores_Abs$Periodo=="2015" & activos_valores_Abs$Comunidades=="Total Nacional" & activos_valores_Abs$Sexo=="Mujeres", ];
años2<-  Activos_Porcentajes_respecto_total[ Activos_Porcentajes_respecto_total$Periodo=="2015" & Activos_Porcentajes_respecto_total$Comunidades=="Total Nacional" & Activos_Porcentajes_respecto_total$Sexo=="Mujeres", ];
años3<-  Tasas_actividad[ Tasas_actividad$Periodo=="2015" & Tasas_actividad$Comunidades=="Total Nacional" & Tasas_actividad$Sexo=="Mujeres", ];

nombrescom<-  activos_vabs[ activos_vabs$Periodo=="2015"& activos_vabs$Sexo=="Ambos sexos" & activos_vabs$Edad=="Total", ];
nomaños<-activos_vabs[  activos_vabs$Sexo=="Ambos sexos" & activos_vabs$Edad=="Total" & activos_vabs$Comunidades=="Total Nacional", ];
años<-nomaños[1]

graf<-activos_vabs[activos_vabs$Comunidades=="Total Nacional" & activos_vabs$Sexo=="Ambos sexos" & activos_vabs$Edad=="Total", ];
values<-as.data.frame(graf[5])
values<-t(t(values))


tabla<-activos_valores_Abs[activos_valores_Abs$Periodo=="2014" & activos_valores_Abs$Sexo=="Ambos sexos" & activos_valores_Abs$Edad=="Total", ];

# tabla<-as.data.frame(tabla);
dat<-t(t(tabla[5]));
com<-t(t(tabla[3]));
#    df11 <- data.frame(com, dat)



```


----------------SHINY APP-----------------
```{r}

library(shiny)
library(readxl)
library(shiny)
library(dygraphs)
library(plotly)
library(shinydashboard)
library(dygraphs)
library(shinyWidgets)
library(ggplot2)
library(shinythemes)
library(sp)


# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title= "Proyecto Loreto",
  navbarMenu("Variables",
             tabPanel("Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("com1","Comunidades Autónomas:",choices = c(nombrescom[4])),
                          selectInput("edad1","Edades:",choices = c(años1[2])),
                          radioButtons("sexo1","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres")),
                          selectInput("años1","Año:", choices = c(años))
                        ),
                        
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title = "En funcion de cumunidades",
                                     h3("Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos"),
                                     h5( "Seleccione la comunidad autonoma, la edad y el sexo"),
                                     h4("Grafico"), 
                                     plotlyOutput("graf1"),
                                     h4("Valores del gráfio"), 
                                     tableOutput("dat1")
                            ),
                            tabPanel(title ="En función del año", 
                                     h3("Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos"),
                                     h5( "Seleccione el año, el sexo y la edad "),
                                     h4("Grafico"),
                                     plotlyOutput("tab1"),
                                     h4("Valores del gráfio"), 
                                     tableOutput("dattab1")
                            )
                            
                          )
                        )
                      )),
             
             tabPanel("Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("com2","Comunidades Autónomas:",choices = c(nombrescom[4])),
                          selectInput("edad2","Edades:",choices = c(años2[2])),
                          radioButtons("sexo2","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres")),
                          selectInput("años2","Año:", choices = c(años))
                          
                        ),
                        
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title = "En funcion de cumunidades",
                                     h3("Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad"),
                                     h5( "Seleccione la comunidad autonoma, la edad y el sexo "),
                                     h4("Grafico"), 
                                     plotlyOutput("graf2"),
                                     h4("Valores del gráfio"), 
                                     tableOutput("dat2")
                            ),
                            tabPanel(title ="En función del año", 
                                     h3("Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada comunidad"),
                                     h5( "Seleccione el año, el sexo y la edad"),
                                     h4("Grafico"),
                                     plotlyOutput("tabplot2"),
                                     h4("Valores del gráfio"), 
                                     tableOutput("tab2")
                            )
                            
                          )
                        )
                      )),
             
             tabPanel("Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("com3","Comunidades Autónomas:",choices = c(nombrescom[4])),
                          selectInput("edad3","Edades:",choices = c(años3[2])),
                          radioButtons("sexo3","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres")),
                          selectInput("años3","Año:", choices = c(años))
                          
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title = "En funcion de cumunidades",
                                     h3("Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma"),
                                     h5( "Seleccione la comunidad autonoma, la edad y el sexo"),
                                     h4("Grafico"), 
                                     plotlyOutput("graf3"),
                                     h4("Valores del gráfio"), 
                                     tableOutput("dat3")
                            ),
                            tabPanel(title ="En función del año", 
                                     h3("Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma"),
                                     h5( "Seleccione la comunidad autonoma, la edad y el sexo"),
                                     h4("Grafico"),
                                     plotlyOutput("tabplot3"),
                                     h4("Valores del gráfio"),
                                     tableOutput("tab3")
                            )
                            
                          )
                        )
                      )),
             
             tabPanel(" Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("com4","Comunidades Autónomas:",choices = c(nombrescom[4])),
                          radioButtons("sexo4","Sexo:",choices = c("Ambos sexos","Hombres","Mujeres")),
                          radioButtons("edad4","Edades:",choices = c("Total","De 16 a 64 años")),
                          selectInput("años4","Año:", choices = c(años))
                          
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title = "En funcion de cumunidades",
                                     h3("Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma "),
                                     h5( "Seleccione la comunidad autonoma, la edad y el sexo"),
                                     h4("Grafico"), 
                                     plotlyOutput("graf4"), 
                                     h4("Valores del gráfio"), 
                                     tableOutput("dat4")
                            ), 
                            tabPanel(title ="En función del año", 
                                     h3("Tasas de actividad de la población de 16 y más años y de la población de 16 a 64 años por sexo y comunidad autónoma "),
                                     h5( "Seleccione la comunidad autonoma, la edad y el sexo"),
                                     h4("Grafico"),
                                      plotlyOutput("tabplot4"),
                                     h4("Valores del gráfio"),
                                     tableOutput("tab4")
                            )
                            
                          )
                        )
                      )
             )
  ),
  #   tabPanel("Protocolo ",includeHTML("/Users/Loreto/Documents/Universidad /Economia mundial, española y regional/Intento final/Portoclo.html")
  #),
  #tabPanel("Informe "
  #),
  
  tabPanel("Descargas",
           selectInput("dataset", "Escoge una Variable:",width ='1000px',
                       choices = c("Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos", 
                                   "Activos por grupo de edad, sexo y comunidad autónoma. 
                                        Porcentajes respecto del total de cada comunidad",
                                   "Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma",
                                   "Variable Tasas de actividad de la población de 16 y más años 
                                          y de la población de 16 a 64 años por sexo y comunidad autónoma")),
           fluidRow(
             
             column(5, 
                    wellPanel(
                      h4("Descargar Variable seleccionada en Rdata"),
                      downloadButton("descargarRdat", label = "Descargar")
                    )
             ),
             column(5,
                    wellPanel(
                      h4("Descargar la variable seleccionada en csv"),
                      downloadButton("descargarVariables", label = "Descargar")
                    )
             ),
             column(5,
                    wellPanel(
                      h4("Descargar el código"),
                      downloadButton("descargarCodigo", label = "Descargar")
                    )
             ),
             
             column(5,
                    wellPanel(
                      h4("Descargar el protocolo"),
                      downloadButton("descargarProtocolo", label = "Descargar")
                    )
             ),
             column(5,
                    wellPanel(
                      h4("Descargar el informe"),
                      downloadButton("descargarInforme", label = "Descargar")
                    )
             )
             
           )
           
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #############VARIABLE 1  #############
  
  
  output$dat1 <- renderTable({
    tabla<-activos_valores_Abs[activos_valores_Abs$Edad==input$edad1 & activos_valores_Abs$Sexo==input$sexo1 & activos_valores_Abs$Comunidades==input$com1 ,];
    tabla<-subset(tabla, select=c("Periodo", "Valor"))
    
  })
  output$dattab1 <- renderTable({
    tabla<-activos_valores_Abs[activos_valores_Abs$Edad==input$edad1 & activos_valores_Abs$Sexo==input$sexo1 & activos_valores_Abs$Periodo==input$años1 ,];
    tabla<-subset(tabla, select=c("Comunidades", "Valor"))
    
  })
  output$graf1 <- renderPlotly({
    graf<-activos_valores_Abs[activos_valores_Abs$Edad==input$edad1 & activos_valores_Abs$Sexo==input$sexo1 & activos_valores_Abs$Comunidades==input$com1 ,];
    graf<-as.data.frame(graf);
    graf1<-t(t(graf[5]));
    años1<-t(t(años))
    
    df1 <- data.frame(años1,
                      graf1)
    ggplot(data=df1, aes(x=años1, y=graf1, group=1))+
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Valores absolutos ")+
      theme_classic()
    
  })
  
  output$tab1 <- renderPlotly({
    tabla1<-activos_valores_Abs[activos_valores_Abs$Edad==input$edad1 & activos_valores_Abs$Sexo==input$sexo1 & activos_valores_Abs$Periodo==input$años1 ,];
    tabla1<-tabla1[-1, ]
    tabla1<-as.data.frame(tabla1);
    dat1<-t(t(tabla1[5]));
    com1<-t(t(tabla1[3]));
    media1<-mean(dat1)
    
    
    df11 <- data.frame(com1,
                       dat1)
    ggplot(data=df11, aes(x=com1, y=dat1, group=1))+
      geom_bar(stat="identity",fill="#4f94cd")+
      xlab("Comunidades")+
      ylab("Valores absolutos ")+
       geom_hline(yintercept=media1, linetype="dashed", color = "coral")+
      theme_classic()+
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    
    
  })
  
  
  #############VARIABLE 2  #############
  
  
  output$dat2 <- renderTable({
    tabla<-Activos_Porcentajes_respecto_total[Activos_Porcentajes_respecto_total$Edad==input$edad2 & Activos_Porcentajes_respecto_total$Sexo==input$sexo2 & Activos_Porcentajes_respecto_total$Comunidades==input$com2 ,];
    tabla<-subset(tabla, select=c("Periodo", "Valor"))
    
  })
  
  output$tab2 <- renderTable({
    tabla<-Activos_Porcentajes_respecto_total[Activos_Porcentajes_respecto_total$Edad==input$edad2 & Activos_Porcentajes_respecto_total$Sexo==input$sexo2 & Activos_Porcentajes_respecto_total$Periodo==input$años2 ,];
    tabla<-subset(tabla, select=c("Comunidades", "Valor"))
    
  })
  
  output$graf2 <- renderPlotly({
    graf<-Activos_Porcentajes_respecto_total[Activos_Porcentajes_respecto_total$Edad==input$edad2 & Activos_Porcentajes_respecto_total$Sexo==input$sexo2 & Activos_Porcentajes_respecto_total$Comunidades==input$com2 ,];
    graf<-as.data.frame(graf);
    graf2<-t(t(graf[5]));
    años2<-t(t(años))
    
    df2 <- data.frame(años2,
                      graf2)
    ggplot(data=df2, aes(x=años2, y=graf2, group=1))+
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Valores absolutos ")+
      theme_classic()
    
  })
  
  output$tabplot2 <- renderPlotly({
    tabla2<-Activos_Porcentajes_respecto_total[Activos_Porcentajes_respecto_total$Edad==input$edad2 & Activos_Porcentajes_respecto_total$Sexo==input$sexo2 & Activos_Porcentajes_respecto_total$Periodo==input$años2 ,];
    tabla2<-as.data.frame(tabla2);
    dat2<-t(t(tabla2[5]));
    com2<-t(t(tabla2[3]));
    media2<-mean(dat2)
    
    df21 <- data.frame(com2,
                       dat2)
    ggplot(data=df21, aes(x=com2, y=dat2, group=1))+
      geom_bar(stat="identity",fill="#4f94cd")+
      xlab("Comunidades")+
      ylab("Valores absolutos ")+
      geom_hline(yintercept=media2, linetype="dashed", color = "coral")+
      theme_classic()+
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    
    
  })
  
  
  #############VARIABLE 3  #############
  
  output$dat3 <- renderTable({
    tabla<-Tasas_actividad[Tasas_actividad$Edad==input$edad3 & Tasas_actividad$Sexo==input$sexo3 & Tasas_actividad$Comunidades==input$com3 ,];
    tabla<-subset(tabla, select=c("Periodo", "Valor"))
    
  })
  output$tab3 <- renderTable({
    tabla<-Tasas_actividad[Tasas_actividad$Edad==input$edad3 & Tasas_actividad$Sexo==input$sexo3 & Tasas_actividad$Periodo==input$años3 ,];
    tabla<-subset(tabla, select=c("Comunidades", "Valor"))
    
  })
  
  output$graf3 <- renderPlotly({
    graf<-Tasas_actividad[Tasas_actividad$Edad==input$edad3 & Tasas_actividad$Sexo==input$sexo3 & Tasas_actividad$Comunidades==input$com3 ,];
    graf<-as.data.frame(graf);
    graf3<-t(t(graf[5]));
    años3<-t(t(años))
    
    df3 <- data.frame(años,
                      graf)
    ggplot(data=df3, aes(x=años3, y=graf3, group=1))+
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Valores absolutos ")+
      theme_classic()
    
  })
  
  output$tabplot3 <- renderPlotly({
    tabla3<-Tasas_actividad[Tasas_actividad$Edad==input$edad3 & Tasas_actividad$Sexo==input$sexo3 & Tasas_actividad$Periodo==input$años3 ,];
    tabla3<-as.data.frame(tabla3);
    dat3<-t(t(tabla3[5]));
    com3<-t(t(tabla3[3]));
    media3<-mean(dat3)
    
    df31 <- data.frame(com3,
                       dat3)
    ggplot(data=df31, aes(x=com3, y=dat3, group=1))+
      geom_bar(stat="identity",fill="#4f94cd")+
      xlab("Comunidades")+
      ylab("Valores absolutos ")+
      geom_hline(yintercept=media3, linetype="dashed", color = "coral")+
      theme_classic()+
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    
    
  })
  
  
  
  
  
  #############VARIABLE 4  #############
  
  
  output$graf4 <- renderPlotly({
    graf<-activos_vabs[activos_vabs$Edad==input$edad4 & activos_vabs$Sexo==input$sexo4 & activos_vabs$Comunidades==input$com4 ,];
    graf<-as.data.frame(graf);
    graf4<-t(t(graf[5]));
    años4<-t(t(años))
    
    df4 <- data.frame(años4,
                      graf4)
    ggplot(data=df4, aes(x=años4, y=graf4, group=1))+
      geom_line(color="#4f94cd")+
      geom_point(color="#00688b")+
      xlab("Años")+
      ylab("Valores absolutos ")+
      theme_classic()
    
  })
  
  
  
  output$dat4 <- renderTable({
    tabla<-activos_vabs[activos_vabs$Edad==input$edad4 & activos_vabs$Sexo==input$sexo4 & activos_vabs$Comunidades==input$com4 ,];
    tabla<-subset(tabla, select=c("Periodo", "Valor"))
    
  })
  
  output$tab4 <- renderTable({
    tabla<-activos_vabs[activos_vabs$Edad==input$edad4 & activos_vabs$Sexo==input$sexo4 & activos_vabs$Periodo==input$años4 ,];
    tabla<-subset(tabla, select=c("Comunidades", "Valor"))
    
  })
  

  output$tabplot4 <- renderPlotly({
    tabla4<-activos_vabs[activos_vabs$Edad==input$edad4 & activos_vabs$Sexo==input$sexo4 & activos_vabs$Periodo==input$años4 ,];
    tabla4<-as.data.frame(tabla4);
    dat4<-t(t(tabla4[5]));
    com4<-t(t(tabla4[4]));
    media4<-mean(dat4)
    
    df41 <- data.frame(com4,
                       dat4)
    ggplot(data=df41, aes(x=com4, y=dat4, group=1))+
      geom_bar(stat="identity",fill="#4f94cd")+
      xlab("Comunidades")+
      ylab("Valores absolutos ")+
      geom_hline(yintercept=media4, linetype="dashed", color = "coral")+
      theme_classic()+
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    
    
  })
  
  
  
  
  ######### DESCARGAS ########
  
  
  output$descargarRdat <- downloadHandler(
    filename = "variable.Rdata",
    content = function(file) {
      if (input$dataset == "Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos"){
        write.table(activos_valores_Abs[], file, row.names = FALSE)
      }
      if (input$dataset == "Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada
                              comunidad"){
        write.table(Activos_Porcentajes_respecto_total[], file, row.names = FALSE)
      }
      if (input$dataset == "Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma"){
        write.table(Tasas_actividad[], file, row.names = FALSE)
      }
      if (input$dataset == "Variable Tasas de actividad de la población de 16 y más años 
                                          y de la población de 16 a 64 años por sexo y comunidad autónoma"){
        write.table(activos_vabs[], file, row.names = FALSE)
      }
      
    }
  )
  
  #Aqui hay que poner la direccion donde se encuentra el codigo en el ordenador 
  output$descargarCodigo <- downloadHandler(
    filename = "codigo.R",
    content = function(file) {
      file.copy("/Users/Loreto/Documents/Universidad /Economia mundial, española y regional/Intento final/Loreto.Rmd", file)
    }
  )
  #Aqui hay que poner la direccion donde se encuentra el codigo en el ordenador 
  output$ descargarProtocolo <- downloadHandler(
    filename = "protocolo.html",
    content = function(file) {
      file.copy("/Users/Loreto/Documents/Universidad /Economia mundial, española y regional/Intento final/Portoclo.html", file)
    }
  )
  #Aqui hay que poner la direccion donde se encuentra el codigo en el ordenador 
  output$ descargarInforme <- downloadHandler(
    filename = "informe.pdf",
    content = function(file) {
      file.copy("/Users/Loreto/Documents/Universidad /Economia mundial, española y regional/Intento final/informe.pdf", file)
    }
  )
  
  
  output$descargarVariables <- downloadHandler(
    filename = "variable.csv",
    content = function(file) {
      if (input$dataset == "Activos por grupo de edad, sexo y comunidad autónoma. Valores absolutos"){
        write.csv(activos_valores_Abs[], file, row.names = FALSE)
      }
      if (input$dataset == "Activos por grupo de edad, sexo y comunidad autónoma. Porcentajes respecto del total de cada
                              comunidad"){
        write.csv(Activos_Porcentajes_respecto_total[], file, row.names = FALSE)
      }
      if (input$dataset == "Tasas de actividad por distintos grupos de edad, sexo y comunidad autónoma"){
        write.csv(Tasas_actividad[], file, row.names = FALSE)
      }
      if (input$dataset == "Variable Tasas de actividad de la población de 16 y más años 
                                          y de la población de 16 a 64 años por sexo y comunidad autónoma"){
        write.csv(activos_vabs[], file, row.names = FALSE)
      }
      
    }
  )
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


```






