# packages 
library(shapefiles)
library(foreign)
library(tidyverse)
library(shiny)
library(shinythemes)

# data processing
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

# municipalities as factors
d$MUN_RESI = factor(d$MUN_RESI)

# defining ggplot theme
tema = theme_classic()+
  theme(plot.title = element_text(color="black", size=14, face="bold", 
                                  hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        panel.background = element_rect(colour = "black"))

# APP
ui <- fluidPage(
  tabsetPanel(
    tabPanel( 
      "General",
      sidebarLayout(
        sidebarPanel("Cases per Federative Unit (State):",
                     selectInput("UF", "FU of Notification of Disease:", c("--- Select ---", "Rondônia", "Acre", "Amazonas", 
                                                                           "Roraima", "Pará", "Amapá", "Tocantins",
                                                                           "Maranhão", "Mato Grosso")), 
                     sliderInput(inputId = "intervalo2",
                                 label = "Time (epidemiologic week):",
                                 min = 1,
                                 max = 52,
                                 value = c(1,52),
                                 dragRange = F)
        ),
        mainPanel(
          plotOutput("casos_UF")
        )
      )
    ),
    tabPanel( 
      "Missing Data",
      sidebarLayout(
        sidebarPanel("Analysis of missing data in variable 'symptom date' vs cases, for the year 2017",
                     selectInput(inputId = "UF_NA",
                                 label = "Federative Unit:",
                                 choices = c("--- Select ---", "Rondônia", "Acre", "Amazonas", 
                                             "Roraima", "Pará", "Amapá", "Tocantins",
                                             "Maranhão", "Mato Grosso"))),
        mainPanel(
          plotOutput("grafico_UF_NA"), 
          tableOutput("tabela_UF_NA")
        )
      )
    )
  )
)

server <- function(input, output){
  
  output$casos_UF <- renderPlot({
    if(input$UF == "--- Select ---"){
      
    } else { 
      
      dados_casos_UF <- d %>% 
        filter(UF == input$UF) %>% 
        group_by(SEM_NOTI) %>% 
        summarise(casos_semana = n())
      
      ##### Removing the first week of 2018
      dados_casos_UF = dados_casos_UF[-2,]
      
      ggplot(data = dados_casos_UF %>% 
               filter(as.numeric(SEM_NOTI) %in% c(input$intervalo2[1]:(input$intervalo2[2]+1))), 
             aes(x = as.numeric(SEM_NOTI), 
                 y = casos_semana)) + 
        geom_line() +
        labs(x = "Time (week)",
             y = "Cases",
             title = "Time Series of the Malaria Cases") +
        tema
      
    }
  })
  
  output$grafico_UF_NA <- renderPlot({
    if(input$UF_NA == "--- Select ---"){
    } else { 
      
      base = d %>% 
        filter(UF == input$UF_NA) %>% 
        group_by(SEM_NOTI) %>% 
        summarise(
          NA_DT_SINTO_SEMANA = sum(if_else(is.na(DT_SINTO), 1, 0)),
          casos_semana_UF_NA = n()
        ) %>% 
        select(NA_DT_SINTO_SEMANA, casos_semana_UF_NA)
      
      ggplot(base, 
             aes(x = casos_semana_UF_NA, 
                 y = NA_DT_SINTO_SEMANA)) + 
        geom_point() +
        labs(x = "Cases per week",
             y = "NAs per week", 
             title = "NAs in 'symptom date' vs cases") +
        tema
    }
  })
  
  output$tabela_UF_NA <- renderTable({
    if(input$UF_NA == "--- Select ---"){
    } else { 
      
      title <- "Tabela de dados"
      resumo_dados <- d %>% 
        filter(UF == input$UF_NA) %>% 
        filter(!is.na(UF), !is.na(DelayDays)) %>% 
        summarise(
          mean = mean(DelayDays),
          SD = sd(DelayDays), 
          median = median(DelayDays),
          `1st quartile` = quantile(DelayDays, probs = 0.25, na.rm = T),
          `3rd quartile` = quantile(DelayDays, probs = 0.75, na.rm = T)
        )
    }
  }, striped = TRUE, bordered = TRUE,  
  hover = TRUE, spacing = 'm', digits = 2, align = 'c', width = '100%')
  
}

shinyApp(ui = ui, server = server)
