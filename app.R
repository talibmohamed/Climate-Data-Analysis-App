library(shiny)
library(shinythemes)
library(lubridate)
library(rlist)
library(xts)
library(plotrix)
library(shinyjs)
library(bslib)  # Add bslib library for theming
library(thematic)  # Add thematic library for real-time themingins
library(ggplot2)
library(rmarkdown)

css <- HTML("

  
  
  /* Overall Body Styling */
  body {
    font-family: 'poppins', sans-serif;
    line-height: 1.6;
    margin: 0;
    padding: 0;
    background-color: #E5D6DF;
  }
  
  /* Sidebar Styling */
  .well {
    background-color: #03071e;
    color: #fff;
    padding: 15px;
    border-radius: 5px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
  }
  
  /* Navigation Styling */
  .nav > li.active > a,
  .nav > li.active > a:hover,
  .nav > li.active > a:focus {
    background-color: #662424;
    color: black;
    font-weight: bold;
    width: 224px;
    text-align: center;


  }
  
  /* Unselected Tab Styling */
  .nav > li:not(.active) > a {
    background-color: #662424; /* Your desired background color for unselected tabs */
    color: #cce3de; /* Change the text color for unselected tabs */
    border-radius: 4px; /* Optional: Add rounded corners to tabs */
    transition: background-color 0.3s ease, color 0.3s ease; /* Add smooth transition effect */
    font-weight: bold;
    width: 224px;
    text-align: center;

  }
  
  /* Unselected Tab Hover and Focus Styling */
  .nav > li:not(.active) > a:hover,
  .nav > li:not(.active) > a:focus {
    background-color: #662424; 
    color: black; 
    font-weight: bold;
    width: 224px;
    text-align: center;


  }
  
  /* Typography */
  h1, h2, h3, h4, h5, h6 {
    font-family: Arial, sans-serif;
    font-weight: bold;
  }
  
  /* Table Styling */
  table {
    width: 100%;
    border-collapse: collapse;
    margin-bottom: 20px;
  }
  
  th, td {
    border: 1px solid #ddd;
    padding: 8px;
    text-align: left;
  }
  
  th {
    background-color: #f2f2f2;
  }
  
  /* Progress Bar Styling */
  .progress {
    height: 20px;
    margin-bottom: 20px;
  }
  
  .progress-bar {
    background-color: #662424; /* Change this color to your desired color */
  }
  
  .custom-table {
      max-width: 100%;  /* Set the maximum width to 100% */
      overflow-x: auto; /* Add horizontal scrolling when necessary */
  }
      .titlePanel h1 {
    font-size: 30px; /* Adjust the font size as needed */
      }
      

            
      

")





# Define the UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(css)),
  theme = shinytheme("united"),
  div(class = "cloud-container"),
  br(), br(),
  tags$h1("Outil d'évaluation des données climatiques", style = "font-size: 40px;"),
  br(), br(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Chargement des données"),
      fileInput("simul", "Sélectionnez le fichier pour les simulations:"),
      fileInput("obs", "Sélectionnez le fichier pour l'observation:"),
      actionButton("submitBtn", "Soumettre", 
                   style = "color: white; background-color: #662424; border-color: #662424",
                   title = "Soumettre!",
                   onclick = "Shiny.setInputValue('btnClicked', true);"),
      downloadButton("download", "Download PDF Report", style = "display: none;"),
      br(),
      br(),
      verbatimTextOutput("report_generation_status"),
      conditionalPanel(
        condition = "input.tabs == 'Calcules métriques'",
        width = 3, style = "margin-top: 30px;",
        checkboxInput("display_rr", "RR", value = FALSE),
        checkboxInput("display_tmax", "Tmax", value = FALSE),
        checkboxInput("display_tmin", "Tmin", value = FALSE),
        checkboxInput("display_tmoy", "Tmoy", value = FALSE),
        conditionalPanel(
          condition = "input.display_rr || input.display_tmax || input.display_tmin || input.display_tmoy",
          downloadButton("export_combined_metrics_btn", "Download Combined Metrics")
        )        
      ),
      conditionalPanel(
        condition = "input.tabs == 'Representations graphiques'",
        width = 3, style = "margin-top: 30px;",
        h4("Les parametre:"),
        checkboxInput("display_moyRRa", "Representation graphique de RR", value = FALSE),
        checkboxInput("display_moyTmax", "Representation graphique de Tmax", value = FALSE),
        checkboxInput("display_moyTmin", "Representation graphique de Tmin", value = FALSE),
        checkboxInput("display_moyTmoy", "Representation graphique de Tmoy", value = FALSE),
        br(),
        h4("Les representation"),
        checkboxInput("cycle", "cycle saisonnier", value = FALSE),
        checkboxInput("qq", "quantile-quantile", value = FALSE),
        checkboxInput("taylor", "taylor", value = FALSE)
      )
    ),
    mainPanel(
      style = "padding: 0; margin: 0;  overflow: hidden;",
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Qualité de données",
          br(),
          fluidRow(
            column(width = 6,
                   fluidRow(
                     conditionalPanel(
                       condition = "input.submitBtn > 0",
                       h3("Contrôle de qualité des observations"),
                       h4("Données d'observation manquantes (si elles existent)"),
                       br()
                     )
                   ),
                   fluidRow(
                     column(width = 12, 
                            uiOutput("obsout_header"),
                            conditionalPanel(
                              condition = "input.submitBtn > 0",
                              tags$div(
                                tableOutput("obsout"),
                                style = "border: 1px solid #B3B3B3; padding: 10px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); overflow-x: auto; width: 100%;" 
                              )
                            )
                     )
                   )
            ),
            column(width = 6, 
                   fluidRow(
                     conditionalPanel(
                       condition = "input.submitBtn > 0",
                       h3("Contrôle de qualité des simulations"),
                       h4("Dates de simulation manquantes (si elles existent)"),
                       br()
                     )
                   ),
                   fluidRow(
                     column(width = 12, 
                            uiOutput("simulout_header"),
                            conditionalPanel(
                              condition = "input.submitBtn > 0",
                              div(
                                class = "styled-text-box",
                                uiOutput("simulout")  # Display missing dates here
                              )
                            )
                     )
                   )
            )
          )
        ),
        
        tabPanel(
          "Calcules métriques",
          fluidRow(
            br(),
            column(
              width = 7,
              conditionalPanel(
                condition = "input.display_rr",
                h4("Les calcules métriques de RR"),
                verbatimTextOutput("rmse_output_rr"),
                verbatimTextOutput("biais_output_rr"),
                verbatimTextOutput("Corr_output_rr"),
                verbatimTextOutput("cv_simuloutput_rr"),
                verbatimTextOutput("cv_obsoutput_rr"),
              ),
              br(),
              conditionalPanel(
                condition = "input.display_tmax",
                h4("Les calcules métriques de Tmax"),
                verbatimTextOutput("rmse_output_tmax"),
                verbatimTextOutput("biais_output_tmax"),
                verbatimTextOutput("Corr_output_tmax"),
                verbatimTextOutput("cv_simuloutput_tmax"),
                verbatimTextOutput("cv_obsoutput_tmax"),
                
              ),
              br(),
              conditionalPanel(
                condition = "input.display_tmin",
                h4("Les calcules métriques de Tmin"),
                verbatimTextOutput("rmse_output_tmin"),
                verbatimTextOutput("biais_output_tmin"),
                verbatimTextOutput("Corr_output_tmin"),
                verbatimTextOutput("cv_simuloutput_tmin"),
                verbatimTextOutput("cv_obsoutput_tmin"),
                
              ),
              br(),
              conditionalPanel(
                condition = "input.display_tmoy",
                h4("Les calcules métriques de Tmoy"),
                verbatimTextOutput("rmse_output_tmoy"),
                verbatimTextOutput("biais_output_tmoy"),
                verbatimTextOutput("Corr_output_tmoy"),
                verbatimTextOutput("cv_simuloutput_tmoy"),
                verbatimTextOutput("cv_obsoutput_tmoy"),
                
              )
            )
          )
        ),
        tabPanel(
          "Representations graphiques",
          br(),
          fluidRow(
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyRRa && input.cycle",
                plotOutput("moyRRa_plot"),
                downloadButton("download_moyRRa_plot", "Download MoyRRa Plot")
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmax && input.cycle",
                plotOutput("moyTmax_plot"),
                downloadButton("download_moyTmax_plot", "Download MoyRRa Plot")
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmin && input.cycle",
                plotOutput("moyTmin_plot"),
                downloadButton("download_moyTmin_plot", "Download MoyRRa Plot")
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmoy && input.cycle",
                plotOutput("moyTmoy_plot"),
                downloadButton("download_moyTmoy_plot", "Download MoyRRa Plot")
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyRRa && input.qq",
                plotOutput("qqRR_plot"),
                downloadButton("download_qqRR_plot", "Download MoyRRa Plot")
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmax && input.qq",
                plotOutput("qqTmax_plot"),
                downloadButton("download_qqTmax_plot", "Download MoyRRa Plot")
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmin && input.qq",
                plotOutput("qqTmin_plot"),
                downloadButton("download_qqTmin_plot", "Download MoyRRa Plot")
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmoy && input.qq",
                plotOutput("qqTmoy_plot"),
                downloadButton("download_qqtmoy_plot", "Download MoyRRa Plot")
              )
            ), column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyRRa && input.taylor",
                plotOutput("taylorRR_plot"),
                downloadButton("download_taylorRR_plot", "Download MoyRRa Plot")
              )
            ), column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmax && input.taylor",
                plotOutput("taylorTmax_plot"),
                downloadButton("download_taylorTmax_plot", "Download MoyRRa Plot")
              )
            ), column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmin && input.taylor",
                plotOutput("taylorTmin_plot"),
                downloadButton("download_taylorTmin_plot", "Download MoyRRa Plot")
              )
            ), column(
              width = 4,
              conditionalPanel(
                condition = "input.display_moyTmoy && input.taylor",
                plotOutput("taylorTmoy_plot"),
                downloadButton("download_taylorTmoy_plot", "Download MoyRRa Plot")
              )
            )
          )
        ),
        tabPanel("Guide d'Utilisation",
                 h1("Manuel d'utilisation d'Outil d'évaluation des simulations générées par les modèles climatiques par rapport aux données observées "),
                 h2("Bienvenue dans l'Outil d'Évaluation des modèles climatiques !"),
                 p("Cet outil vous permet d'évaluer les données issues des modèles climatiques en effectuant différentes analyses et en créant des représentations graphiques. Vous trouvez dans ce guide les différentes étapes à suivre pour bien utiliser cet outil."),
                 p("Il convient de noter que cette version constitue une première itération du projet. Par conséquent, l'évaluation se concentrera exclusivement sur les paramètres liés à la température, à savoir : la température maximale, la température minimale et la température moyenne, ainsi que les cumuls de précipitations."),
                 p("Afin que les fichiers puissent être considérés comme des entrées valides pour notre outil, les critères suivants doivent être respectés :"),
                 HTML("<ul>
    <li>Les données relatives à chacun des paramètres évoqués précédemment doivent être fournies avec une fréquence régulière (Des données journalières).</li>
    <li>Les fichiers doivent être au format texte (.txt) et leur structure doit suivre le modèle suivant : Année mois jour CumulPrécipitations (RR) TempératureMaximale(Tmax) TempératureMinimale(Tmin)</li>
    <br>
<h5>Exemple:</h5>
<p>1979 1 1 8 27.4 9.2</p>
        <p>1979 1 2 1 23.6 6.2</p>

    <p>1979 1 3 0 24.4 4.2</p>

  </ul>"),
                 p("Nous tenons à souligner qu'en raison de la nature exploratoire de cette version, notre analyse sera circonscrite aux aspects mentionnés ci-dessus."),
                 
                 h3("1. Chargement des Données :"),
                 p("a. Cliquez sur le bouton \"Sélectionnez le fichier pour les simulations\" pour télécharger le fichier de données de simulation (Données Modèle)."),
                 p("b. Cliquez sur le bouton \"Sélectionnez le fichier pour l'observation\" pour télécharger le fichier de données d'observation."),
                 p("Cliquez sur le bouton \"Soumettre\" une fois les fichiers téléchargés."),
                 
                 h3("2. Qualité des Données :"),
                 p("Cette section aborde initialement l'évaluation de la qualité des données observées en calculant les pourcentages de données manquantes ainsi que les jours correspondants. Ensuite, dans un deuxième volet, en tenant compte du fait que certains modèles climatiques attribuent systématiquement 30 jours à chaque mois, nous avons jugé opportun d'examiner cette anomalie par le biais d'une évaluation de la qualité des données simulées."),
                 p("Dans cette section, vous verrez un tableau affichant la qualité des données d'observation et de simulation. Les données manquantes sont signalées et les dates associées sont affichées."),
                 
                 h3("3. Les Calculs Métriques :"),
                 p("Veuillez cocher les cases correspondant aux paramètres que vous souhaitez évaluer (RR, Tmax, Tmin, Tmoy)."),
                 p("Les métriques de performance, telles que la RMSE (racine carrée de l'erreur quadratique moyenne), le biais, la corrélation et le coefficient de variation, seront calculées pour chaque paramètre sélectionné."),
                 
                 h3("4. Représentation Graphique :"),
                 p("Cochez les cases correspondantes pour sélectionner les représentations graphiques que vous souhaitez afficher (cycle saisonnier, quantile-quantile, diagramme de Taylor) pour les paramètres que vous avez choisis."),
                 p("Une gamme de graphiques illustrant les variations saisonnières et les comparaisons sera générée en fonction de vos sélections."),
                 p("Veuillez noter que vous avez la possibilité de télécharger les différentes représentations graphiques en utilisant le bouton suivant :"),
                 
                 h3("5. Téléchargement des Données :"),
                 p("Une fois les analyses effectuées, des boutons de téléchargement seront disponibles pour obtenir les données traitées, incluant les valeurs moyennes calculées (Tmoy)."),
                 
                 h3("6. Cycle Saisonnier :"),
                 p("Vous verrez un graphique montrant le cycle saisonnier des précipitations (RR), de la température maximale (Tmax), de la température minimale (Tmin) et de la température moyenne (Tmoy) pour les données d'observation et de simulation. Ce graphique mettra en comparaison les données d'observation et les données de simulation."),
                 
                 h3("7. Les diagrammes Quantile-Quantile (qqplot) :"),
                 p("Les représentations graphiques de type Quantile-Quantile illustrent une comparaison détaillée entre les données observées et les données simulées. Ce type de graphique permet de mettre en évidence la concordance entre les deux en affichant les quantiles des deux ensembles de données. Lorsque les points s'alignent près de la ligne diagonale, cela indique une correspondance étroite entre les distributions."),
                 
                 h3("8. Diagramme de Taylor :"),
                 p("Le diagramme de Taylor affiche la dispersion et la corrélation entre les données d'observation et de simulation. Une dispersion minimale et une corrélation élevée indiquent une bonne concordance."),
                 
                 p("L'outil offrira également la possibilité de créer un rapport global comprenant les résultats spécifiques de chaque section traitée. Ce rapport sera généré au format PDF, offrant ainsi une présentation professionnelle et structurée des informations recueillies.")
        ),
        
        tabPanel("À Propos",
                 h3("À Propos de l'Application d'Analyse de Données Climatiques : Comparaison simulations climatiques - Observations"),
                 p("Nous sommes ravis de vous présenter une première version d’un outil de pointe dédiée à l'analyse de données climatiques. Conçue pour les chercheurs, les météorologues et les professionnels du domaine, cette application essentielle offre des fonctionnalités sophistiquées pour une comparaison détaillée entre les données d'observation et les simulations climatiques générées par des modèles climatiques. Cette plateforme facilite grandement l'évaluation et l'interprétation des tendances climatiques."),
                 p("Il est important de souligner que cet outil peut être appliqué à toute forme de comparaison entre modèle et observation, dans n'importe quel domaine, et ne se limite pas aux domaines mentionnés précédemment."),
                 h3("Fonctionnalités Distinctives :"),
                 tags$ul(
                   tags$li("Importation fluide de jeux de données de simulations et d'observations."),
                   tags$li("Vérification de la qualité des données, mettant en évidence les valeurs manquantes."),
                   tags$li("Calcul précis de métriques essentielles telles que le RMSE (Root Mean Square Error), le biais moyen, la corrélation et le coefficient de variation, offrant une évaluation rigoureuse des performances des simulations."),
                   tags$li("Représentations graphiques avancées, incluant des graphiques des cycles saisonniers, des analyses quantiles-quantiles et des diagrammes de Taylor, offrant une perspective visuelle approfondie des caractéristiques climatiques."),
                   tags$li("Exportation aisée des données et des visualisations pour des études ultérieures."),
                   tags$li("Création d’un rapport global comprenant les résultats spécifiques de chaque section traitée. Ce rapport sera généré au format PDF.")
                 ),
                 p("Cette application incarne l'union entre l'expertise en climatologie et en développement. Qu'il s'agisse d'une analyse exhaustive des données climatiques ou d'une exploration préliminaire des schémas saisonniers, notre application offre une expérience inégalée."),
                 p("N'hésitez pas à nous contacter pour toute question, suggestion ou observation. Nous sommes honorés de compter des esprits dévoués à la recherche et à l'exploration climatiques parmi notre communauté d'utilisateurs."),
                 p("Nous tenons à remercier toute l'équipe pour leur contribution à la réalisation de cette application destinée à répondre aux besoins analytiques en matière de comparaison modèle-observation."),
                 p("Vous pouvez trouver le code source de cette application sur notre ", 
                   tags$a("profil GitHub", href = "https://github.com/talibmohamed/Climate-Data-Analysis-App")),
                 
        )
        
        
      )
    )
  )
)



# Define the server
server <- function(input, output, sessio) {
  # Load the shinyjs package
  shinyjs::useShinyjs()
  
  # Define a reactiveValues variable to track report generation status
  rv <- reactiveValues(reportGenerated = FALSE)
  output_file <- NULL
  
  
  NA_percentage <- function(input_vector) {
    missing_values <- is.na(input_vector)
    percentage_missing <- sum(missing_values) / length(input_vector) * 100
    
    result <- list(percentage_missing, missing_values)
    
    return(result)
  }
  
  process_OBS <- function(filename) {
    data1 <- read.table(filename, header = FALSE)
    dates_data <- as.Date(paste(data1$V1, data1$V2, data1$V3, sep = "-"))
    
    min_dat <- min(dates_data)
    max_dat <- max(dates_data)
    dates_data1 <- seq(from = min_dat, to = max_dat, by = "day")
    
    RR_data1 <- data1$V4
    Tmax_data1 <- data1$V5
    Tmin_data1 <- data1$V6
    
    # Data 1: Percentage of missing values and corresponding dates for each parameter
    percentage_missing_RR_data1 <- NA_percentage(RR_data1)[[1]]
    percentage_missing_Tmax_data1 <- NA_percentage(Tmax_data1)[[1]]
    percentage_missing_Tmin_data1 <- NA_percentage(Tmin_data1)[[1]]
    
    missing_dates_RR_data1 <- dates_data1[NA_percentage(RR_data1)[[2]]]
    missing_dates_Tmax_data1 <- dates_data1[NA_percentage(Tmax_data1)[[2]]]
    missing_dates_Tmin_data1 <- dates_data1[NA_percentage(Tmin_data1)[[2]]]
    
    # Prepare the result to return
    result <- data.frame(
      parameter = c("RR", "Tmax", "Tmin"),
      percentage = c(percentage_missing_RR_data1, percentage_missing_Tmax_data1, percentage_missing_Tmin_data1),
      corresponding_day = I(list(
        paste(missing_dates_RR_data1, collapse = ", "),
        paste(missing_dates_Tmax_data1, collapse = ", "),
        paste(missing_dates_Tmin_data1, collapse = ", ")
      ))
    )
    
    return(result)
  }
  
  fill_missing_dates <- function(data) {
    # Convert year, month, and day columns to Date object
    cc <- as.Date(paste(data$V1, data$V2, data$V3, sep = "-"))
    new_data <- data.frame(Date = cc, RR = data$V4, Tmax = data$V5, Tmin = data$V6)
    # Step 1: Create a full date vector
    v1_dates <- cc
    min_date <- min(v1_dates)
    max_date <- max(v1_dates)
    full_date_vector <- seq(from = min_date, to = max_date, by = "day")
    
    # Step 2: Find the missing dates
    missing_dates <- full_date_vector[!(full_date_vector %in% v1_dates)]
    
    # Step 3: Replace corresponding values with NA for missing dates
    
    missing_data <- data.frame(
      Date = missing_dates,
      par1 = rep(NA, length(missing_dates)),
      par2 = rep(NA, length(missing_dates)),
      par3 = rep(NA, length(missing_dates))
    )
    
    data <- merge(new_data, missing_data, all = TRUE)
    
    # Sort the data by Date
    data <- data[order(data$Date), ]
    dt1 <- data[, 1:4]
    return(data[, 1:4])
  }
  
  obs_format <- function(data) {
    cc <- as.Date(paste(data$V1, data$V2, data$V3, sep = "-"))
    new_data <- data.frame(Date = cc, RR = data$V4, Tmax = data$V5, Tmin = data$V6)
    return(new_data)
  }
  
  filter_complete_cases <- function(simul_data, obs_data, parameter) {
    simul_param <- simul_data[[parameter]]
    obs_param <- obs_data[[parameter]]
    
    complete_cases <- complete.cases(obs_param, simul_param)
    simul_complete <- simul_data[[parameter]][complete_cases]
    obs_complete <- obs_param[complete_cases]
    
    list(simul_complete = simul_complete, obs_complete = obs_complete)
  }
  
  observeEvent(input$submitBtn, {
    if (!is.null(input$obs$datapath)) {
      df_obs <- process_OBS(input$obs$datapath)
      output$obsout <- renderTable(df_obs)
    }
    
    if (!is.null(input$simul$datapath)) {
      data1 <- read.table(input$simul$datapath, header = FALSE)
      df_simul <- fill_missing_dates(data1)
      df_simul$Date <- as.Date(df_simul$Date)
      added_dates <- subset(df_simul, is.na(RR) | is.na(Tmax) | is.na(Tmin))
      added_dates$Date <- format(added_dates$Date, "%Y-%m-%d")
      missing_dates11 <- added_dates$Date
      
      output$simulout <- renderText({
        if (length(missing_dates11) > 0) {
          paste("<b>Dates manquantes :</b>", paste(missing_dates11, collapse = ", "))
        } else {
          "Aucune date manquante."
        }
      })
    }
    
    
    
    
    
    
    
    
    if (!is.null(input$obs$datapath) && !is.null(input$simul$datapath)) {
      obs_formated <- obs_format(read.table(input$obs$datapath, header = FALSE))
      data1 <- read.table(input$simul$datapath, header = FALSE) # Load the data from simulation file
      df_simul <- fill_missing_dates(data1)
      
      rr_data <- filter_complete_cases(df_simul, obs_formated, "RR")
      tmax_data <- filter_complete_cases(df_simul, obs_formated, "Tmax")
      tmin_data <- filter_complete_cases(df_simul, obs_formated, "Tmin")
      
      # Calculate Tmoy using Tmax and Tmin
      tmoy_data_simul <- (tmax_data$simul_complete + tmin_data$simul_complete) / 2
      tmoy_data_obs <- (tmax_data$obs_complete + tmin_data$obs_complete) / 2
      
      
      rmse_rr <- sqrt(mean((rr_data$simul_complete - rr_data$obs_complete)^2))
      biais_rr <- mean(rr_data$simul_complete - rr_data$obs_complete)
      corr_rr <- cor(rr_data$simul_complete, rr_data$obs_complete)
      cv_simulrr <- ((sd(rr_data$simul_complete, na.rm = TRUE) / mean(rr_data$simul_complete, na.rm = TRUE)) * 100)
      cv_obsrr <- ((sd(rr_data$obs_complete, na.rm = TRUE) / mean(rr_data$obs_complete, na.rm = TRUE)) * 100)
      
      
      
      rmse_tmax <- sqrt(mean((tmax_data$simul_complete - tmax_data$obs_complete)^2))
      biais_tmax <- mean(tmax_data$simul_complete - tmax_data$obs_complete)
      corr_tmax <- cor(tmax_data$simul_complete, tmax_data$obs_complete)
      cv_simultmax <- ((sd(tmax_data$simul_complete, na.rm = TRUE) / mean(tmax_data$simul_complete, na.rm = TRUE)) * 100)
      cv_obstmax <- ((sd(tmax_data$obs_complete, na.rm = TRUE) / mean(tmax_data$obs_complete, na.rm = TRUE)) * 100)
      
      
      rmse_tmin <- sqrt(mean((tmin_data$simul_complete - tmin_data$obs_complete)^2))
      biais_tmin <- mean(tmin_data$simul_complete - tmin_data$obs_complete)
      corr_tmin <- cor(tmin_data$simul_complete, tmin_data$obs_complete)
      cv_simultmin <- ((sd(tmin_data$simul_complete, na.rm = TRUE) / mean(tmin_data$simul_complete, na.rm = TRUE)) * 100)
      cv_obstmin <- ((sd(tmin_data$obs_complete, na.rm = TRUE) / mean(tmin_data$obs_complete, na.rm = TRUE)) * 100)
      
      rmse_tmoy <- sqrt(mean((tmoy_data_simul - tmoy_data_obs)^2))
      biais_tmoy <- mean(tmoy_data_simul - tmoy_data_obs)
      corr_tmoy <- cor(tmoy_data_simul, tmoy_data_obs)
      cv_simultmoy <- ((sd(tmoy_data_simul, na.rm = TRUE) / mean(tmoy_data_simul, na.rm = TRUE)) * 100)
      cv_obstmoy <- ((sd(tmoy_data_obs, na.rm = TRUE) / mean(tmoy_data_obs, na.rm = TRUE)) * 100)
      
      
      
      # RR output
      output$rmse_output_rr <- renderText({
        if (input$display_rr) {
          sprintf("RMSE de RR: %.2f mm", rmse_rr)
        }
      })
      
      output$biais_output_rr <- renderText({
        if (input$display_rr) {
          sprintf("Biais de RR: %.2f mm", biais_rr)
        }
      })
      
      output$Corr_output_rr <- renderText({
        if (input$display_rr) {
          sprintf("Correlation de RR: %.2f", corr_rr)
        }
      })
      
      output$cv_simuloutput_rr <- renderText({
        if (input$display_rr) {
          sprintf("Coefficient de Variation (CV) de RR pour les simulations: %.2f%%", cv_simulrr)
        }
      })
      output$cv_obsoutput_rr <- renderText({
        if (input$display_rr) {
          sprintf("Coefficient de Variation (CV) de RR pour les observations: %.2f%%", cv_obsrr)
        }
      })
      
      # Tmax output
      output$rmse_output_tmax <- renderText({
        if (input$display_tmax) {
          sprintf("RMSE de Tmax: %.2f °C", rmse_tmax)
        }
      })
      
      output$biais_output_tmax <- renderText({
        if (input$display_tmax) {
          sprintf("Biais de Tmax: %.2f °C", biais_tmax)
        }
      })
      
      output$Corr_output_tmax <- renderText({
        if (input$display_tmax) {
          sprintf("Correlation de Tmax: %.2f", corr_tmax)
        }
      })
      
      output$cv_simuloutput_tmax <- renderText({
        if (input$display_tmax) {
          sprintf("Coefficient de Variation (CV) de Tmax pour les simul: %.2f%%", cv_simultmax)
        }
      })
      output$cv_obsoutput_tmax <- renderText({
        if (input$display_tmax) {
          sprintf("Coefficient de Variation (CV) de Tmax pour les observations: %.2f%%", cv_obstmax)
        }
      })
      
      # Tmin output
      output$rmse_output_tmin <- renderText({
        if (input$display_tmin) {
          sprintf("RMSE de Tmin: %.2f °C", rmse_tmin)
        }
      })
      
      output$biais_output_tmin <- renderText({
        if (input$display_tmin) {
          sprintf("Biais de Tmin: %.2f °C", biais_tmin)
        }
      })
      
      output$Corr_output_tmin <- renderText({
        if (input$display_tmin) {
          sprintf("Correlation de Tmin: %.2f", corr_tmin)
        }
      })
      
      output$cv_simuloutput_tmin <- renderText({
        if (input$display_tmin) {
          sprintf("Coefficient de Variation (CV) de Tmin pour les simulations: %.2f%%", cv_simultmin)
        }
      })
      output$cv_obsoutput_tmin <- renderText({
        if (input$display_tmin) {
          sprintf("Coefficient de Variation (CV) de Tmin pour les observations: %.2f%%", cv_obstmin)
        }
      })
      
      # Tmoy output
      output$rmse_output_tmoy <- renderText({
        if (input$display_tmoy) {
          sprintf("RMSE de Tmoy: %.2f °C", rmse_tmoy)
        }
      })
      
      output$biais_output_tmoy <- renderText({
        if (input$display_tmoy) {
          sprintf("Biais de Tmoy: %.2f °C", biais_tmoy)
        }
      })
      
      output$Corr_output_tmoy <- renderText({
        if (input$display_tmoy) {
          sprintf("Correlation de Tmoy: %.2f", corr_tmoy)
        }
      })
      
      output$cv_simuloutput_tmoy <- renderText({
        if (input$display_tmoy) {
          sprintf("Coefficient de Variation (CV) de Tmoy pour les observations: %.2f%%", cv_simultmoy)
        }
      })
      output$cv_obsoutput_tmoy <- renderText({
        if (input$display_tmoy) {
          sprintf("Coefficient de Variation (CV) de Tmoy pour les simulations: %.2f%%", cv_obstmoy)
        }
      })
      
      # Export metrics button
      # Create a reactive expression to determine if the combined metrics download button should be shown
      show_combined_metrics_button <- reactive({
        input$display_rr || input$display_tmax || input$display_tmin || input$display_tmoy
      })
      
      # Create a combined metrics data frame
      combined_metrics_data <- reactive({
        metrics <- data.frame(
          Metric = character(0),
          Parameter = character(0),
          Value = numeric(0)
        )
        
        if (input$display_rr) {
          metrics <- rbind(metrics, data.frame(Metric = "RMSE", Parameter = "RR", Value = rmse_rr))
          metrics <- rbind(metrics, data.frame(Metric = "Biais", Parameter = "RR", Value = biais_rr))
          metrics <- rbind(metrics, data.frame(Metric = "Correlation", Parameter = "RR", Value = corr_rr))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variation des simulations", Parameter = "RR", Value = cv_simulrr))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variation des observations", Parameter = "RR", Value = cv_obsrr))
          
        }
        
        if (input$display_tmax) {
          metrics <- rbind(metrics, data.frame(Metric = "RMSE", Parameter = "Tmax", Value = rmse_tmax))
          metrics <- rbind(metrics, data.frame(Metric = "Biais", Parameter = "Tmax", Value = biais_tmax))
          metrics <- rbind(metrics, data.frame(Metric = "Correlation", Parameter = "Tmax", Value = corr_tmax))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variation des simulations", Parameter = "Tmax", Value = cv_simultmax))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variation des observations", Parameter = "Tmax", Value = cv_obstmax))
          
        }
        
        if (input$display_tmin) {
          metrics <- rbind(metrics, data.frame(Metric = "RMSE", Parameter = "Tmin", Value = rmse_tmin))
          metrics <- rbind(metrics, data.frame(Metric = "Biais", Parameter = "Tmin", Value = biais_tmin))
          metrics <- rbind(metrics, data.frame(Metric = "Correlation", Parameter = "Tmin", Value = corr_tmin))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variation des simulations", Parameter = "Tmin", Value = cv_simultmin))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variation des observations", Parameter = "Tmin", Value = cv_obstmin))
          
        }
        
        if (input$display_tmoy) {
          metrics <- rbind(metrics, data.frame(Metric = "RMSE", Parameter = "Tmoy", Value = rmse_tmoy))
          metrics <- rbind(metrics, data.frame(Metric = "Biais", Parameter = "Tmoy", Value = biais_tmoy))
          metrics <- rbind(metrics, data.frame(Metric = "Correlation", Parameter = "Tmoy", Value = corr_tmoy))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variationdes des simulations", Parameter = "Tmoy", Value = cv_simultmoy))
          metrics <- rbind(metrics, data.frame(Metric = "Coefficient de Variationdes des observations", Parameter = "Tmoy", Value = cv_obstmoy))
          
        }
        
        return(metrics)
      })
      
      # Download combined metrics
      output$export_combined_metrics_btn <- downloadHandler(
        filename = function() {
          "combined_metrics.csv"
        },
        content = function(file) {
          write.csv(combined_metrics_data(), file, row.names = FALSE)
        }
      )
      
      # creating the download files with Tmoy
      # Read observation and simulation data
      #obs_data <- read.table(input$obs$datapath, header = FALSE)
      #simul_data <- read.table(input$simul$datapath, header = FALSE)
      
      # Process data and calculate Tmoy
      #processed_obs_data <- process_data(obs_data)
      #processed_simul_data <- process_data(simul_data)
      
      # Combine data with Tmoy and convert to text
      #obs_with_tmoy <- create_output_text(processed_obs_data)
      #simul_with_tmoy <- create_output_text(processed_simul_data)
      
      # Render download buttons for observation and simulation data with Tmoy
      # output$download_obs_button <- renderUI({
      #  downloadButton("export_obs_with_tmoy_btn", "Download Observation Data with Tmoy")
      #})
      
      #output$download_simul_button <- renderUI({
      #  downloadButton("export_simul_with_tmoy_btn", "Download Simulation Data with Tmoy")
      # })
      
      # Create files to be downloaded
      #output$export_obs_with_tmoy_btn <- downloadHandler(
      #  filename = function() {
      #    "obs_with_tmoy.txt"
      #  },
      #  content = function(file) {
      #     writeLines(obs_with_tmoy, con = file)
      #  }
      # )
      
      #output$export_simul_with_tmoy_btn <- downloadHandler(
      #  filename = function() {
      #    "simul_with_tmoy.txt"
      #  },
      #   content = function(file) {
      #    writeLines(simul_with_tmoy, con = file)
      #  }
      # )
      
      obs_org <- read.table(input$obs$datapath, header = FALSE)
      
      year_d <- obs_org[, 1][1]
      year_f <- obs_org[, 1][length(obs_org[, 1])]
      month_d <- obs_org[, 2][1]
      month_f <- obs_org[, 2][length(obs_org[, 1])]
      day_d <- obs_org[, 3][1]
      day_f <- obs_org[, 3][length(obs_org[, 1])]
      
      debut <- paste(year_d, month_d, day_d, sep = "/")
      fin <- paste(year_f, month_f, day_f, sep = "/")
      
      dat <- seq(as.Date(debut), as.Date(fin), "days")
      year <- as.numeric(format(dat, "%Y"))
      mon <- as.numeric(format(dat, "%m"))
      dayy <- as.numeric(format(dat, "%d"))
      
      RRo <- xts(obs_org[, 4], order.by = dat)
      moyRRm_obs <- matrix(round(apply.monthly(RRo, sum, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyRRm_obs) <- seq(year_d, year_f)
      colnames(moyRRm_obs) <- month.abb
      moyRRa_obs <- round(colMeans(moyRRm_obs), 2)
      
      RRs <- xts(df_simul[, 2], order.by = dat)
      moyRRm_simul <- matrix(round(apply.monthly(RRs, sum, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyRRm_simul) <- seq(year_d, year_f)
      colnames(moyRRm_simul) <- month.abb
      moyRRa_simul <- round(colMeans(moyRRm_simul), 2)
      
      moyRRa_plot <- NULL
      
      output$moyRRa_plot <- renderPlot({
        
        month_names <- month.abb
        
        # Create the base R plot
        plot(moyRRa_obs, type = "n", xlab = "Mois", ylab = "cumul des precipitations en (mm)", main = "cycle Saisonnier de RR", ylim = c(0, 80), xaxt = "n")
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f5f0")
        lines(moyRRa_obs, type = "o", col = "red", lwd = 2, pch = 8)
        lines(moyRRa_simul, type = "o", col = "blue", pch = 8)
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        legend("topright", legend = c("observation", "simulation"), col = c("red", "blue"), pch = 8, cex = 1)
        axis(1, at = 1:12, labels = month_names)
        
        # Store the base R plot
        moyRRa_plot <<- recordPlot()
        
      })
      
      # Download button functionality
      output$download_moyRRa_plot <- downloadHandler(
        filename = function() {
          paste("moyRRa_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)
          replayPlot(moyRRa_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      Tmaxo <- xts(obs_org[, 5], order.by = dat)
      moyTmaxm_obs <- matrix(round(apply.monthly(Tmaxo, mean, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyTmaxm_obs) <- seq(year_d, year_f)
      colnames(moyTmaxm_obs) <- month.abb
      moyTmaxa_obs <- round(colMeans(moyTmaxm_obs), 2)
      
      Tmaxs <- xts(df_simul[, 3], order.by = dat)
      moyTmaxm_simul <- matrix(round(apply.monthly(Tmaxs, mean, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyTmaxm_simul) <- seq(year_d, year_f)
      colnames(moyTmaxm_simul) <- month.abb
      moyTmaxa_simul <- round(colMeans(moyTmaxm_simul), 2)
      
      moyTmax_plot <- NULL
      
      output$moyTmax_plot <- renderPlot({
        
        month_names <- month.abb
        
        # Create the base R plot for moyTmax_plot
        plot(moyTmaxa_obs, type = "n", xlab = "Mois", ylab = "Temperature Maximal (c°)", main = "cycle Saisonnier de Tmax", col.main = "black", ylim = c(0, 50), xaxt = "n")
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f5f0")
        lines(moyTmaxa_obs, type = "o", col = "red", lwd = 2, pch = 8)
        lines(moyTmaxa_simul, type = "o", col = "blue", pch = 8)
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        legend("topright", legend = c("observation", "simulation"), col = c("red", "blue"), pch = 8)
        axis(1, at = 1:12, labels = month_names)
        
        # Store the base R plot for moyTmax_plot
        moyTmax_plot <<- recordPlot()
        
      })
      
      # Download button functionality for moyTmax_plot
      output$download_moyTmax_plot <- downloadHandler(
        filename = function() {
          paste("moyTmax_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(moyTmax_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      Tmino <- xts(obs_org[, 6], order.by = dat)
      moyTminm_obs <- matrix(round(apply.monthly(Tmino, mean, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyTminm_obs) <- seq(year_d, year_f)
      colnames(moyTminm_obs) <- month.abb
      moyTmina_obs <- round(colMeans(moyTminm_obs), 2)
      
      Tmins <- xts(df_simul[, 4], order.by = dat)
      moyTminm_simul <- matrix(round(apply.monthly(Tmins, mean, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyTminm_simul) <- seq(year_d, year_f)
      colnames(moyTminm_simul) <- month.abb
      moyTmina_simul <- round(colMeans(moyTminm_simul), 2)
      
      moyTmin_plot <- NULL
      
      output$moyTmin_plot <- renderPlot({
        
        month_names <- month.abb
        
        # Create the base R plot for moyTmin_plot
        plot(moyTmina_obs, type = "n", xlab = "Mois", ylab = "Temperature Minimal (c°)", main = "cycle Saisonnier de Tmin", col.main = "black", ylim = c(0, 50), xaxt = "n")
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f5f0")
        lines(moyTmina_obs, type = "o", col = "red", lwd = 2, pch = 8)
        lines(moyTmina_simul, type = "o", col = "blue", pch = 8)
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        legend("topright", legend = c("observation", "simulation"), col = c("red", "blue"), pch = 8)
        axis(1, at = 1:12, labels = month_names)
        
        # Store the base R plot for moyTmin_plot
        moyTmin_plot <<- recordPlot()
        
      })
      
      # Download button functionality for moyTmin_plot
      output$download_moyTmin_plot <- downloadHandler(
        filename = function() {
          paste("moyTmin_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(moyTmin_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      Tmoyo <- xts((obs_org[, 5] + obs_org[, 6]) / 2, order.by = dat)
      moyTmoym_obs <- matrix(round(apply.monthly(Tmoyo, mean, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyTmoym_obs) <- seq(year_d, year_f)
      colnames(moyTmoym_obs) <- month.abb
      moyTmoya_obs <- round(colMeans(moyTmoym_obs), 2)
      
      Tmoys <- xts((df_simul[, 3] + df_simul[, 4]) / 2, order.by = dat)
      moyTmoym_simul <- matrix(round(apply.monthly(Tmoys, mean, na.rm = TRUE), 2), 27, 12, byrow = T)
      row.names(moyTmoym_simul) <- seq(year_d, year_f)
      colnames(moyTmoym_simul) <- month.abb
      moyTmoya_simul <- round(colMeans(moyTmoym_simul), 2)
      
      moyTmoy_plot <- NULL
      
      output$moyTmoy_plot <- renderPlot({
        
        month_names <- month.abb
        
        # Create the base R plot for moyTmoy_plot
        plot(moyTmoya_obs, type = "n", xlab = "Mois", ylab = "Temperature moyenne (c°)", main = "cycle Saisonnier de Tmoy", col.main = "black", ylim = c(0, 50), xaxt = "n")
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f5f0")
        lines(moyTmoya_obs, type = "o", col = "red", lwd = 2, pch = 8)
        lines(moyTmoya_simul, type = "o", col = "blue", pch = 8)
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        legend("topright", legend = c("observation", "simulation"), col = c("red", "blue"), pch = 8)
        axis(1, at = 1:12, labels = month_names)
        
        # Store the base R plot for moyTmoy_plot
        moyTmoy_plot <<- recordPlot()
        
      })
      
      # Download button functionality for moyTmoy_plot
      output$download_moyTmoy_plot <- downloadHandler(
        filename = function() {
          paste("moyTmoy_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(moyTmoy_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      qqRR_plot <- NULL
      
      obs_rr_qq = obs_org[, 4]
      simul_rr_qq = df_simul[, 2]
      
      output$qqRR_plot <- renderPlot({
        qqplot(obs_rr_qq, simul_rr_qq, xlim = c(0, 50), ylim = c(0, 50), xlab = "observation", ylab = "simulation", main = "qqplot de RR", col = ("steelblue"))
        abline(a = 0, b = 1, lwd = 2, col = "black")
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        
        # Store the base R plot for qqRR_plot
        qqRR_plot <<- recordPlot()
      })
      
      # Download button functionality for qqRR_plot
      output$download_qqRR_plot <- downloadHandler(
        filename = function() {
          paste("qqRR_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1920, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(qqRR_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      qqTmax_plot <- NULL
      
      obs_tmax_qq = obs_org[, 5]
      simul_tmax_qq = df_simul[, 3]
      
      output$qqTmax_plot <- renderPlot({
        qqplot(obs_tmax_qq, simul_tmax_qq, xlim = c(0, 50), ylim = c(0, 50), xlab = "observation", ylab = "simulation", main = "qqplot de Tmax", col = ("steelblue"))
        abline(a = 0, b = 1, lwd = 2, col = "black")
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        
        # Store the base R plot for qqTmax_plot
        qqTmax_plot <<- recordPlot()
      })
      
      # Download button functionality for qqTmax_plot
      output$download_qqTmax_plot <- downloadHandler(
        filename = function() {
          paste("qqTmax_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(qqTmax_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      qqTmin_plot <- NULL
      
      obs_tmin_qq = obs_org[, 6]
      simul_tmin_qq = df_simul[, 4]
      
      output$qqTmin_plot <- renderPlot({
        qqplot(obs_tmin_qq, simul_tmin_qq, xlim = c(0, 50), ylim = c(0, 50), xlab = "observation", ylab = "simulation", main = "qqplot de Tmin", col = ("steelblue"))
        abline(a = 0, b = 1, lwd = 2, col = "black")
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        
        # Store the base R plot for qqTmin_plot
        qqTmin_plot <<- recordPlot()
      })
      
      # Download button functionality for qqTmin_plot
      output$download_qqTmin_plot <- downloadHandler(
        filename = function() {
          paste("qqTmin_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(qqTmin_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      qqTmoy_plot <- NULL
      
      obs_tmoy_qq = (obs_org[, 5] + obs_org[, 6]) / 2
      simul_tmoy_qq = (df_simul[, 3] + df_simul[, 4]) / 2
      
      output$qqTmoy_plot <- renderPlot({
        qqplot(obs_tmoy_qq, simul_tmoy_qq, xlim = c(0, 50), ylim = c(0, 50), xlab = "observation", ylab = "simulation", main = "qqplot de Tmoy", col = ("steelblue"))
        abline(a = 0, b = 1, lwd = 2, col = "black")
        grid(nx = NULL, ny = NULL,
             lty = 2, col = "gray", lwd = 2)
        
        # Store the base R plot for qqTmoy_plot
        qqTmoy_plot <<- recordPlot()
      })
      
      # Download button functionality for qqTmoy_plot
      output$download_qqtmoy_plot <- downloadHandler(
        filename = function() {
          paste("qqTmoy_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(qqTmoy_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      taylorRR_plot <- NULL
      
      output$taylorRR_plot <- renderPlot({
        taylor.diagram(
          obs_rr_qq, simul_rr_qq,
          add = FALSE, col = "steelblue",
          pch = 16,
          pos.cor = TRUE,
          xlab = "Standard Deviation",
          ylab = "",
          main = "Taylor Diagram de RR",
          show.gamma = TRUE,
          ngamma = 6,
          gamma.col = "#A356D0",
          sd.arcs = pi,
          ref.sd = TRUE,
          sd.method = "sample",
          grad.corr.lines = c(0.1, 0.3, 0.5, 0.7, 0.9),
          pcex = 1.2,
          cex.axis = 1.2,
          normalize = FALSE,
          mar = c(5, 4, 4, 2),
        )
        
        # Store the base R plot for taylorRR_plot
        taylorRR_plot <<- recordPlot()
      })
      
      # Download button functionality for taylorRR_plot
      output$download_taylorRR_plot <- downloadHandler(
        filename = function() {
          paste("taylorRR_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(taylorRR_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      taylorTmax_plot <- NULL
      
      output$taylorTmax_plot <- renderPlot({
        taylor.diagram(
          obs_tmax_qq, simul_tmax_qq,
          add = FALSE, col = "steelblue",
          pch = 16,
          pos.cor = TRUE,
          xlab = "Standard Deviation",
          ylab = "",
          main = "Taylor Diagram de Tmax",
          show.gamma = TRUE,
          ngamma = 6,
          gamma.col = "#A356D0",
          sd.arcs = pi,
          ref.sd = TRUE,
          sd.method = "sample",
          grad.corr.lines = c(0.1, 0.3, 0.5, 0.7, 0.9),
          pcex = 1.2,
          cex.axis = 1.2,
          normalize = FALSE,
          mar = c(5, 4, 4, 2),
        )
        
        # Store the base R plot for taylorTmax_plot
        taylorTmax_plot <<- recordPlot()
      })
      
      # Download button functionality for taylorTmax_plot
      output$download_taylorTmax_plot <- downloadHandler(
        filename = function() {
          paste("taylorTmax_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(taylorTmax_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      taylorTmin_plot <- NULL
      
      output$taylorTmin_plot <- renderPlot({
        taylor.diagram(
          obs_tmin_qq, simul_tmin_qq,
          add = FALSE, col = "steelblue",
          pch = 16,
          pos.cor = TRUE,
          xlab = "Standard Deviation",
          ylab = "",
          main = "Taylor Diagram de Tmin",
          show.gamma = TRUE,
          ngamma = 6,
          gamma.col = "#A356D0",
          sd.arcs = pi,
          ref.sd = TRUE,
          sd.method = "sample",
          grad.corr.lines = c(0.1, 0.3, 0.5, 0.7, 0.9),
          pcex = 1.2,
          cex.axis = 1.2,
          normalize = FALSE,
          mar = c(5, 4, 4, 2),
        )
        
        # Store the base R plot for taylorTmin_plot
        taylorTmin_plot <<- recordPlot()
      })
      
      # Download button functionality for taylorTmin_plot
      output$download_taylorTmin_plot <- downloadHandler(
        filename = function() {
          paste("taylorTmin_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(taylorTmin_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
      taylorTmoy_plot <- NULL
      
      output$taylorTmoy_plot <- renderPlot({
        taylor.diagram(
          obs_tmoy_qq, simul_tmoy_qq,
          add = FALSE, col = "steelblue",
          pch = 16,
          pos.cor = TRUE,
          xlab = "Standard Deviation",
          ylab = "",
          main = "Taylor Diagram de Tmoy",
          show.gamma = TRUE,
          ngamma = 6,
          gamma.col = "#A356D0",
          sd.arcs = pi,
          ref.sd = TRUE,
          sd.method = "sample",
          grad.corr.lines = c(0.1, 0.3, 0.5, 0.7, 0.9),
          pcex = 1.2,
          cex.axis = 1.2,
          normalize = FALSE,
          mar = c(5, 4, 4, 2),
        )
        
        # Store the base R plot for taylorTmoy_plot
        taylorTmoy_plot <<- recordPlot()
      })
      
      # Download button functionality for taylorTmoy_plot
      output$download_taylorTmoy_plot <- downloadHandler(
        filename = function() {
          paste("taylorTmoy_plot_", Sys.time(), ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1920, height = 1080, units = "px", res = 300)  # Adjust dimensions as needed
          replayPlot(taylorTmoy_plot)  # Replay and save the stored plot
          dev.off()
        }
      )
      
    } else {
      # If either or both files are not uploaded, render empty placeholders
      output$download_obs_button <- renderUI({})
      output$download_simul_button <- renderUI({})
    }
    
    
    observeEvent(input$submitBtn, {
      
      output_file <- tempfile(fileext = ".pdf")
      # Add the code to render the R Markdown report
      rmarkdown::render(
        "dynamic_report_template.Rmd",
        output_file = output_file,
        params = list(
          rmse_rr_report = rmse_rr,
          biais_rr_report = biais_rr,
          corr_rr_report = corr_rr,
          cv_simulrr_report = cv_simulrr,
          cv_obsrr_report = cv_obsrr,
          
          
          rmse_tmax_report = rmse_tmax,
          biais_tmax_report = biais_tmax,
          corr_tmax_report = corr_tmax,
          cv_simultmax_report = cv_simultmax,
          cv_obstmax_report = cv_obstmax,
          
          rmse_tmin_report = rmse_tmin,
          biais_tmin_report = biais_tmin,
          corr_tmin_report = corr_tmin,
          cv_simultmin_report = cv_simultmin,
          cv_obstmin_report = cv_obstmin,
          
          rmse_tmoy_report = rmse_tmoy,
          biais_tmoy_report = biais_tmoy,
          corr_tmoy_report = corr_tmoy,
          cv_simultmoy_report = cv_simultmoy,
          cv_obstmoy_report = cv_obstmoy,
          
          obs_table = df_obs,
          missing_dates11 = missing_dates11,
          
          month.abb = month.abb,
          moyRRa_obs = moyRRa_obs,
          moyRRa_simul= moyRRa_simul,
          
          moyTmaxa_obs = moyTmaxa_obs,
          moyTmaxa_simul = moyTmaxa_simul,
          
          moyTmina_obs = moyTmina_obs,
          moyTmina_simul = moyTmina_simul,
          
          moyTmoya_obs = moyTmoya_obs,
          moyTmoya_simul = moyTmoya_simul,
          
          obs_rr_qq = obs_rr_qq,
          simul_rr_qq = simul_rr_qq,
          
          obs_tmax_qq = obs_org[, 5],
          simul_tmax_qq = df_simul[, 3],
          
          obs_tmin_qq = obs_tmax_qq,
          simul_tmin_qq = simul_tmin_qq,
          
          obs_tmoy_qq = obs_tmoy_qq,
          simul_tmoy_qq = simul_tmoy_qq
          
          
        ),
        output_format = "pdf_document"
      )
      shinyjs::show("download")
      # Update the reportGenerated status
      rv$reportGenerated <- TRUE      
      
      output$download <- downloadHandler(
        filename = function() {
          "Raport_d'evaluation_global.pdf"
        },
        content = function(file) {
          file.copy(output_file, file)
        },
        contentType = "application/pdf"
      )
      
      output$report_generation_status <- renderText({
        "PDF report has been generated."
      })
    })
    
  })
}

# Process data and calculate Tmoy
process_data <- function(data) {
  data$Tmoy <- (data[, 5] + data[, 6]) / 2
  
  return(data)
}

# Create output text with Tmoy
create_output_text <- function(data) {
  output_text <- capture.output(
    write.table(data, sep = " ", row.names = FALSE, col.names = FALSE)
  )
  return(output_text)
}

# Run the app
shinyApp(ui, server)
