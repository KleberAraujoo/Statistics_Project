
#Importação das bibliotecas 
library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(hrbrthemes)

#Ler o dataset
master_df <- read.csv("google.csv")
column_list <- c("Open","High","Low","Close","Adj Close", "Volume")

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Date <- strptime(master_df$Date, format="%Y-%m-%d")

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando séries', tabName = 'comp', icon = icon('chart-bar'))
    )
)
#Base do dashboard
body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('column', 'Tipos de Dados', column_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre os dados escolhidos", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Gráfico de linha", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
                fluidRow(
                    box(title = "Histograma", width = 12, solidHeader = TRUE,
                        plotOutput('hist')
                    )
                ),
                fluidRow(
                    box(title = "BoxPlot", width = 12, solidHeader = TRUE,
                        plotOutput('box')
                    )
                ),
                fluidRow(
                    box(
                        status = "primary",
                        width = 12,
                        tags$b("Participantes:"),
                        tags$ul(
                        tags$li("Fabriely Luana Pinheiro dos Santos (flps)"),
                        tags$li("Danilo José Coutinho de Oliveira (djco)"),
                        tags$li("Kleberson de Araújo Bezerra(kab2)"),
                        )
                    )
                )
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('column_comp', 'Dados', column_list, multiple=TRUE),
                        uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),  
                fluidRow(
                    box(title = "Correlação entre as colunas", width = 12, solidHeader = TRUE,
                        DTOutput('info_comp')
                    )
                ),
                fluidRow(
                    box(title = "Gráficos de linha", width = 12, solidHeader = TRUE,
                        plotOutput('comp_sh1'),
                        plotOutput('comp_sh2')
                    )
                ),
                fluidRow(
                    box(title = "Grafico de barra das medias", width = 12, solidHeader = TRUE,
                        plotOutput('bar_med')
                    )
                ),
                fluidRow(
                    box(title = "Scatterplot", width = 12, solidHeader = TRUE,
                        plotOutput('scatter')
                    )
                )
        )
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)

server <- function(input, output) {
    ##Input
    select_column <- eventReactive(input$go, {
        df_column <- master_df 
        return(df_column)
    })
    
    select_column_comp <- eventReactive(input$go_comp, {
        df_column <- master_df 
        return(df_column)
    })
    
    output$timedate <- renderUI({
        
        column_name <- input$column
        
        df <- master_df
        
        min_time <- min(df$Date)
        min_time
        max_time <- max(df$Date)
        dateRangeInput("true_date", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate_comp <- renderUI({
        
        column_name <- input$column_comp
        
        df <- master_df
        
        maxmin_time <- df %>% 
            summarise(MD = min(Date)) %>% 
            .$MD %>% 
            max()
        
        minmax_time <- df %>% 
            summarise(MD = max(Date)) %>% 
            .$MD %>% 
            min()
        
        min_time <- maxmin_time
        max_time <- minmax_time
        
        dateRangeInput("true_date_comp", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min    = min_time,
                       max    = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
#Para criar a função 
    getmode <- function(v){
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
##Output
    Info_DataTable <- eventReactive(input$go,{
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        vetor <- datacut[, c(column_name)]
        
        Media <- mean(vetor)
        Mediana <- median(vetor)
        DP <- sd(vetor)
        Moda <- getmode(vetor)
        
        Dados <- input$column
        
        df_tb <-  data.frame(Dados, Media, Mediana, DP, Moda)
        df_tb <- as.data.frame(t(df_tb))
        
        
        return(df_tb)
    })
    
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
    
##OutputComp
    Info_DataTable_COMP <- eventReactive(input$go_comp,{
        df <- select_column_comp()
        column_names <- input$column_comp
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        vetor_1 <- datacut[, c(column_names[1])]
        vetor_2 <- datacut[, c(column_names[2])]
        
        correlacao <- cor(vetor_1, vetor_2)
        
        colunas <- paste(column_names[1], column_names[2], sep = ' x ')
        
        df_tb <-  data.frame(colunas, correlacao)
        df_tb <- as.data.frame(t(df_tb))
        
        return(df_tb)
    })
    
    output$info_comp <- renderDT({
        column_names <- input$column_comp
        if (length(column_names)>1){
            Info_DataTable_COMP() %>%
                as.data.frame() %>% 
                DT::datatable(options=list(
                    language=list(
                        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                    )
                ))
        }
    })
    
    output$sh <- renderPlot({
        #Todos os inputs
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        aux <- datacut[, column_name] %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        datacut$Date <- ymd(datacut$Date)
        a <- datacut %>% 
            ggplot(aes_string("Date", toString(column_name), group='1')) +
            geom_path(color="#069808", size=1, alpha=0.8) +
            ylab(toString(column_name)) +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        a
        
    })
    output$hist <- renderPlot({
        #Todos os inputs
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        
        aux <- datacut[, column_name] %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        datacut$Date <- ymd(datacut$Date)
        p <- datacut %>% 
            ggplot(aes_string(toString(column_name))) + 
            geom_histogram(color="black", aes(fill=..count..))+
            theme_bw() +
            labs(x="Frequência", toString(column_name))
        
        p
    })
    output$box <- renderPlot({
        #Todos os inputs
        df <- select_column()
        column_name <- input$column
        twin <- input$true_date
        
        df[, "month"] <- as.factor(format(df[,"Date"], "%Y-%m"))

        datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
        b <- datacut %>% 
            ggplot(aes_string(x="month", y=toString(column_name))) + 
            geom_boxplot(color="black",fill="#F35704", size=0.2, alpha=0.9)
        b
    })
    
    output$comp_sh1 <- renderPlot({
        #Todos os inputs
        column_names <- input$column_comp
        if (!is.null(column_names)){
            df <- select_column_comp()
            twin <- input$true_date_comp
            
            datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
            
            aux <- datacut[, column_names[1]] %>% na.omit() %>% as.numeric()
            aux1 <- min(aux)
            aux2 <- max(aux)
            datacut$Date <- ymd(datacut$Date)
            
            a <- datacut %>% 
                ggplot(aes_string("Date", toString(column_names[1]), group='1')) +
                geom_path(color="#DA0606", size=1, alpha=2) +
                ylab(toString(column_names[1])) +
                coord_cartesian(ylim = c(aux1, aux2)) +
                theme_bw() +
                scale_x_date(date_labels = "%Y-%m-%d")
            
            a
        }
    })
    
    output$comp_sh2 <- renderPlot({
        #Todos os inputs
        column_names <- input$column_comp
        if (length(column_names) > 1){
            df <- select_column_comp()
            twin <- input$true_date_comp
            
            datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
            
            aux <- datacut[, column_names[2]] %>% na.omit() %>% as.numeric()
            aux1 <- min(aux)
            aux2 <- max(aux)
            datacut$Date <- ymd(datacut$Date)
            
            a <- datacut %>% 
                ggplot(aes_string("Date", toString(column_names[2]), group='1')) +
                geom_path(color="#FF4000", size=1, alpha=0.9) +
                ylab(toString(column_names[2])) +
                coord_cartesian(ylim = c(aux1, aux2)) +
                theme_bw() +
                scale_x_date(date_labels = "%Y-%m-%d")
            
            a
        }
    })
    output$bar_med <- renderPlot({
        #Todos os inputs
        column_names <- input$column_comp
        if (length(column_names)>1){
            df <- select_column_comp()
            twin <- input$true_date_comp
            
            datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
            
            datacut$Date <- ymd(datacut$Date)
            
            vetor_1 <- datacut[, c(column_names[1])]
            vetor_2 <- datacut[, c(column_names[2])]
            
            Media_1 <- mean(vetor_1)
            Media_2 <- mean(vetor_2)
            
            columns <- c(column_names[1], column_names[2])
            medians <- c(Media_1, Media_2)
            coul <- brewer.pal(2, "Set2")
            aux <- data.frame(columns, medians)
             
            
            b <- barplot(height=aux$medians, names=aux$column, col=coul)
            b   
        }
    })
    output$scatter <- renderPlot({
        #Todos os inputs
        column_names <- input$column_comp
        if (length(column_names)>1){
            df <- select_column_comp()
            twin <- input$true_date_comp
            
            datacut <- df[df$Date >= twin[1] & df$Date <= twin[2],]
            
            datacut$Date <- ymd(datacut$Date)
            
            cor <- rgb(0,1,0)
            
            s <- datacut %>% 
                ggplot(aes_string(x=column_names[1], y=column_names[2])) + 
                geom_point(size=2,color="#05579A")
            s
        }
    })
}

shinyApp(
  ui = ui,
  server = server
)

