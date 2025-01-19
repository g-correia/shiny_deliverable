#pacotes e diretório
setwd("~/Documents/IC Fiocruz")
library(shapefiles)
library(foreign)
library(tidyverse)
library(shiny)
library(shinythemes)


#Base e tratamentos
da = read.dbf("NOTIPO17.DBF")
d = da$dbf
d = select(d, -c(GESTANTE1, NIV_ESCO_1, ESQUEMA_1))

d$DelayDays <- difftime(as.Date(as.character(d$DT_DIGIT),format="%Y-%m-%d"), 
                        as.Date(as.character(d$DT_NOTIF),format="%Y-%m-%d"),
                        units = "days")


d$UF_NOTIF = d$UF_NOTIF %>% 
  factor(labels = c("Rondônia", "Acre", "Amazonas", 
                    "Roraima", "Pará", "Amapá", "Tocantins",
                    "Maranhão", "Mato Grosso"))
d = d %>% rename(UF = UF_NOTIF)

#Transformando município em fator
d$MUN_RESI = factor(d$MUN_RESI)

#Tabela do total de casos dos 10 municípios com maior número de casos
casos_municipio = d %>% 
  group_by(MUN_RESI) %>% 
  summarise(casos = n()) %>% 
  arrange(desc(casos)) %>% 
  top_n(10)

#Definindo tema dos gráficos ggplot para parecerem com o primeiro
tema = theme_classic()+
  theme(plot.title = element_text(color="black", size=14, face="bold", 
                                  hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        panel.background = element_rect(colour = "black"))

#APP
ui <- fluidPage(
  # theme = shinytheme("united"),
  
  tabsetPanel(
    tabPanel( 
      "Geral",
      sidebarLayout(sidebarPanel("Número de casos por Unidade de Federação:",
                                 selectInput("UF", "UF da notificação:", c("--- Selecione ---", "Rondônia", "Acre", "Amazonas", 
                                                                           "Roraima", "Pará", "Amapá", "Tocantins",
                                                                           "Maranhão", "Mato Grosso")), 
                                 sliderInput(inputId = "intervalo2",
                                             label = "Tempo (em semana epidemiológica):",
                                             min = 1,
                                             max = 52,
                                             value = c(1,52),
                                             dragRange = F)
      ),
      
      mainPanel(
        plotOutput("casos_UF"))
      )
    ),
    tabPanel("Municipal", sidebarLayout(sidebarPanel("Os dez municípios com maior número de casos:",
                                                     selectInput(inputId = "municipio",
                                                                 label = "Município:",
                                                                 choices = c("---- Selecione ----", casos_municipio[1]))),
                                        mainPanel(plotOutput("municipios_casos"))
                                        
    )
    ),
    tabPanel( 
      "Dados Faltantes",
      sidebarLayout(sidebarPanel("Análise dos dados faltantes em 'data do sintoma' versus o número de casos, para o ano de 2017",
                                 selectInput(inputId = "UF_NA",
                                             label = "Unidade de Federação:",
                                             choices = c("--- Selecione ---", "Rondônia", "Acre", "Amazonas", 
                                                         "Roraima", "Pará", "Amapá", "Tocantins",
                                                         "Maranhão", "Mato Grosso"))),
                    mainPanel(plotOutput("grafico_UF_NA"), 
                              tableOutput("tabela_UF_NA")))
      
    )
  )
)


server <- function(input, output){
  
  output$casos_UF <- renderPlot({
    
    if(input$UF == "--- Selecione ---"){
      
    } else { 
      
      dados_casos_UF <- d %>% 
        filter(UF == input$UF) %>% 
        group_by(SEM_NOTI) %>% 
        summarise(casos_semana = n())
      
      #####Removendo a primeira semana de 2018:
      dados_casos_UF = dados_casos_UF[-2,]
      
      ggplot(data = dados_casos_UF %>% 
               filter(as.numeric(SEM_NOTI) %in% c(input$intervalo2[1]:(input$intervalo2[2]+1))), 
             aes(x = as.numeric(SEM_NOTI), 
                 y = casos_semana)) + 
        geom_line() +
        labs(x = "Tempo (em semanas)",
             y = "Número de casos",
             title = "Gráfico do número de casos") +
        tema
      
    }
  })
  
  output$municipios_casos <- renderPlot({ 
    
    if(input$municipio == "--- Selecione ---"){
      
    } else { 
      
      dados_casos_muni <- d %>% 
        filter(MUN_NOTI == input$municipio) %>% 
        group_by(SEM_NOTI) %>% 
        summarise(casos_semana = n())
      
      #####Removendo a primeira semana de 2018:
      dados_casos_muni = dados_casos_muni[-2,]
      
      ggplot(data = dados_casos_muni,
             aes(x = as.numeric(SEM_NOTI),
                 y = casos_semana)) +
        geom_line() + 
        labs(x = "Tempo (em semanas)",
             y = "Casos",
             title = "Gráfico do número de casos") +
        tema
      
    }
  })
  
  output$grafico_UF_NA <- renderPlot({
    
    if(input$UF_NA == "--- Selecione ---"){
    } else { 
      
      base = d %>% 
        filter(UF == input$UF_NA) %>% 
        group_by(SEM_NOTI) %>% 
        summarise(NA_DT_SINTO_SEMANA = sum(if_else(is.na(DT_SINTO), 1, 0)),
                  casos_semana_UF_NA = n()) %>% 
        select(NA_DT_SINTO_SEMANA, casos_semana_UF_NA)
      
      ggplot(base, 
             aes(x = casos_semana_UF_NA, 
                 y = NA_DT_SINTO_SEMANA)) + 
        geom_point() +
        labs(x = "Casos por semana",
             y = "NAs por semana", 
             title = " Gráfico: NAs em 'data do sintoma' versus número de casos") +
        tema
    }
  })
  
  output$tabela_UF_NA <- renderTable({
    
    if(input$UF_NA == "--- Selecione ---"){
      
      
    } else { 
      
      title <- "Tabela de dados"
      resumo_dados <- d %>% 
        filter(UF == input$UF_NA) %>% 
        filter(!is.na(UF), !is.na(DelayDays)) %>% 
        summarise(`média` = mean(DelayDays),
                  `desvio padrão` = sd(DelayDays), 
                  mediana = median(DelayDays),
                  `1º quartil` = quantile(DelayDays, probs = 0.25, na.rm = T),
                  `3º quartil` = quantile(DelayDays, probs = 0.75, na.rm = T))
    }
  }, striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'm', digits = 2, align = 'c', width = '100%')
  
}

shinyApp(ui = ui, server = server)
