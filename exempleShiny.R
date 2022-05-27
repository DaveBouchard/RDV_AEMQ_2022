setwd("C:/Users/h.levesque/Desktop/AEMQ")

options(scipen = 999)
options(max.print = 99999)

list.of.packages <- c("rio", "tidyverse", "lubridate", "leaflet", "shiny", "shinyjs", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)
install_formats()

`%notin%` <- Negate(`%in%`)

ventes <- import("ventes.xlsx") %>% mutate(ANNEE_VENTE=substr(DATE_VENTE,1,4)) %>% filter(ANNEE_VENTE<=year(now()), NO_ENREGISTREMENT!='26276752', NO_ENREGISTREMENT!='27015291', NO_ENREGISTREMENT!='26840197')

ui <- navbarPage(
  title = 'AEMQ',
  tabPanel('Ventes', 
           sidebarPanel(width = 2,
                        useShinyjs(),
                        radioButtons("sortie", "Choisir:", c("Liste", "Statistiques descriptives", "Leaflet"), selected = "Liste"),
                        selectInput("select_desc_num", "Variable:", choices = c("AIRE_ETAGES", "AIRE_TERRAIN", "ANNEE_CONSTRUCTION","PRIX_VENTE", "erreur", "erreur_abs"), selected="PRIX_VENTE"),
                        div(style="display:inline-block;float: left;",actionButton("go_btn","Charger"),width=5),
                        tags$br(),
                        tags$br()),
           mainPanel(dataTableOutput("ventes_tbl"),
                     verbatimTextOutput("sortie_1_desc_num"),
                     fluidRow(column(width = 4, plotOutput("sortie_2_desc_num")), column(width = 6, plotOutput("sortie_3_desc_num"))),
                     leafletOutput("sortie_4_desc_num", width = "100%", height = 800)),
           
           ),
  tabPanel('Régression', 
           sidebarPanel(width = 3,
                        useShinyjs(),
                        selectizeInput("regVars_1","Variable dépendante",choices=c("PRIX_VENTE"), multiple = FALSE, selected = "PRIX_VENTE"),
                        selectizeInput("regVars_2","Variables numériques",choices=c("AIRE_ETAGES", "AIRE_TERRAIN", "ANNEE_CONSTRUCTION"), multiple = TRUE, selected = c("AIRE_ETAGES", "AIRE_TERRAIN", "ANNEE_CONSTRUCTION")),
                        selectizeInput("regVars_3","Variables catégoriques",choices=c("ANNEE_VENTE","GENRE","LIEN"), multiple = TRUE, selected = c("ANNEE_VENTE","GENRE","LIEN")),
                        actionButton("go_btn_2","Actualiser"),
                        tags$br(),
                        tags$br(),
                        tags$h3("PISA"),
                        numericInput("pisa_iter", "Nombre d'itérations:", 10, min = 1, max = 500),
                        numericInput("pisa_cluster", "Nombre de groupes:", length(unique(ventes$UV)), min = 1, max = 1000),
                        actionButton("go_btn_3","Exécuter la PISA"),
                        actionButton("refresh", "Redémarrer Shiny")),
           mainPanel(verbatimTextOutput("sortie_reg")),
           
  )
  )

server <- function(input, output, session) {

      observeEvent(c(input$go_btn,input$go_btn_2), {
      shinyjs::disable("go_btn")
      shinyjs::disable("go_btn_2")
      if(!is.null(input$regVars_3)) {
        output$sortie_reg <- renderPrint ({
          summary(lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2, str_c("as.character(", input$regVars_3, ")")), collapse=" + "), sep=" ~ ")), data = ventes))
        })
        ventes <- ventes %>% mutate(erreur=round(resid(lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2, str_c("as.character(", input$regVars_3, ")")), collapse=" + "), sep=" ~ ")), data = ventes)),0), erreur_abs=abs(erreur), estimation=round(lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2, str_c("as.character(", input$regVars_3, ")")), collapse=" + "), sep=" ~ ")), data = ventes)$fitted.values,0))
        model <- lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2, str_c("as.character(", input$regVars_3, ")")), collapse=" + "), sep=" ~ ")), data = ventes)
        output$ventes_tbl <- renderDataTable({datatable(ventes, options=list(pageLength = 10, searching = TRUE), escape = FALSE, select = 'multiple')})
      } else {
        output$sortie_reg <- renderPrint ({
          summary(lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2), collapse=" + "), sep=" ~ ")), data = ventes))
        })
        ventes <- ventes %>% mutate(erreur=round(resid(lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2), collapse=" + "), sep=" ~ ")), data = ventes)),0), erreur_abs=abs(erreur), estimation=round(lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2), collapse=" + "), sep=" ~ ")), data = ventes)$fitted.values,0))
        model <- lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2), collapse=" + "), sep=" ~ ")), data = ventes)
      }
      output$ventes_tbl <- renderDataTable({datatable(ventes, options=list(pageLength = 10, searching = TRUE), escape = FALSE, select = 'multiple')})
      
      shinyjs::show("ventes_tbl")
      shinyjs::hide("sortie_1_desc_num")
      shinyjs::hide("sortie_2_desc_num")
      shinyjs::hide("sortie_3_desc_num")
      shinyjs::hide("sortie_4_desc_num")
    if(input$sortie == "Statistiques descriptives") {
      output$sortie_1_desc_num <- renderPrint ({summary(ventes[input$select_desc_num])})
      output$sortie_2_desc_num <- renderPlot ({boxplot(ventes[input$select_desc_num])})
      output$sortie_3_desc_num <- renderPlot ({ggplot(ventes, aes_string(x = input$select_desc_num)) + geom_histogram(bins=200) + labs(x = input$select_desc_num, y = "fréquence")})
      shinyjs::hide("ventes_tbl")
      shinyjs::show("sortie_1_desc_num")
      shinyjs::show("sortie_2_desc_num")
      shinyjs::show("sortie_3_desc_num")
      shinyjs::hide("sortie_4_desc_num")
    }
    if(input$sortie == "Leaflet") {
        output$sortie_4_desc_num <- renderLeaflet ({
          qpal <<- colorQuantile(c("#0000FF", "#0080FF", "#FFFFFF", "#FF8000", "#FF0000"), ventes[input$select_desc_num], n = 20)
          leaflet(data = ventes) %>% 
            addTiles() %>% 
            addCircleMarkers(lng =  ~ ventes$LONGITUDE, lat =  ~ ventes$LATITUDE, color = qpal(ventes[[input$select_desc_num]]), stroke = FALSE, fillOpacity = 0.75, radius = 10, popup = paste("ENR ", ventes$NO_ENREGISTREMENT, ": ",ventes[[input$select_desc_num]])) %>% 
            addLegend("bottomright", pal = qpal, values = ~ventes[[input$select_desc_num]], title = "Légende", opacity = 1)
        })
        shinyjs::hide("ventes_tbl")
        shinyjs::hide("sortie_1_desc_num")
        shinyjs::hide("sortie_2_desc_num")
        shinyjs::hide("sortie_3_desc_num")
        shinyjs::show("sortie_4_desc_num")
    }
      shinyjs::enable("go_btn")
      shinyjs::enable("go_btn_2")
    })

  
  observeEvent(input$go_btn_3, {
    shinyjs::disable("select_desc_num")
    shinyjs::disable("regVars_2")
    shinyjs::disable("regVars_3")
    shinyjs::disable("pisa_cluster")
    shinyjs::disable("pisa_iter")
    shinyjs::disable("go_btn")
    shinyjs::disable("go_btn_2")
    shinyjs::disable("go_btn_3")
    ventes <<- import("ventes.xlsx") %>% mutate(ANNEE_VENTE=substr(DATE_VENTE,1,4)) %>% filter(ANNEE_VENTE<=year(now()),NO_ENREGISTREMENT!='26276752', NO_ENREGISTREMENT!='27015291', NO_ENREGISTREMENT!='26840197')
    for(m in seq_along(1:input$pisa_iter)){
      ventes[[str_c("nom")]] <<- str_c("cl_",kmeans(tibble(x = c(scale(ventes$LONGITUDE)), y = c(scale(ventes$LATITUDE))), input$pisa_cluster, iter.max = 200)$cluster)
      pisa <<- lm(as.formula(paste(input$regVars_1, paste(c(input$regVars_2, str_c("as.character(", input$regVars_3, ")"), "nom"), collapse=" + "), sep=" ~ ")), data = ventes)
      coef_cste <<- pisa$coefficients[[1]]
      coef_clusters <<- tibble(nom = str_replace_all(names(pisa$coefficients), "nom", ""))
      coef_clusters[[str_c("iter_",m)]] <<- pisa$coefficients + coef_cste
      coef_clusters <<- coef_clusters %>% filter(grepl("cl_", nom))
      ventes <<- left_join(ventes, coef_clusters, by = "nom")
      ventes[[str_c("iter_",m)]]<<-scale(replace_na(ventes[[str_c("iter_",m)]],coef_cste))
      ventes <<- ventes %>% select(-nom)
    }
    ventes <<- cbind(ventes, pisa_iphl = unname(rowMeans(ventes %>% select(contains("iter_")), na.rm = T)))
    ventes <<- ventes %>% mutate(pisa_iphl=scale(pisa_iphl))
    clean_column <<- colnames(ventes %>% select(contains("iter_")))
    ventes <<- ventes %>% select(-one_of(clean_column))
    output$ventes_tbl <- renderDataTable({datatable(ventes, options=list(pageLength = 10, searching = TRUE), escape = FALSE, select = 'multiple')})
    updateSelectInput(session, "select_desc_num", "Variable:", choices = c("AIRE_ETAGES", "AIRE_TERRAIN", "ANNEE_CONSTRUCTION","PRIX_VENTE", "erreur", "erreur_abs", "pisa_iphl"), selected="PRIX_VENTE")
    updateSelectizeInput(session, "regVars_2","Variables numériques",choices=c("AIRE_ETAGES", "AIRE_TERRAIN", "ANNEE_CONSTRUCTION", "pisa_iphl"), selected = c("AIRE_ETAGES", "AIRE_TERRAIN", "ANNEE_CONSTRUCTION"))
    shinyjs::enable("select_desc_num")
    shinyjs::enable("regVars_2")
    shinyjs::enable("regVars_3")
    shinyjs::enable("go_btn")
    shinyjs::enable("go_btn_2")
    })
  
  observeEvent(input$refresh, {
    session$reload()
  })
  
}

shinyApp(ui, server)