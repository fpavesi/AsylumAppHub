ui <- fluidPage(
  
  # App title ----
  titlePanel(p("BRIEF ANALYSIS OF ASYLUMS", 
               style="text-align: center;"), 
             windowTitle = "Brief Analysis of Asylums"),
  setBackgroundColor(
    color = "#E0FFFF",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  
  br(),
  br(),
  
  
    mainPanel(
      width = 12,
      align = "center",
      
      # barplot ui--------------------------------------
      fluidRow(
        column(10,
               style = "background-color:#E6EEFF;",
               tabsetPanel(type = "tabs",
                           tabPanel("RR - First istance", 
                                    plotlyOutput(outputId = "histogram1", height = 600),
                                    htmlOutput("rrfirst", style="text-align: justify;"),
                                    p("In the bar plot we represent the recognition rate (RR) defined
                                      as the number of positive decisions over the total decisions
                                      on asylum. The ratio in this case refers to first istance decision of a specific destination country and
                                      it is calculated for each provenience country of the asylum seekers", style="text-align: justify;"),
                                    p("The user can select the year, the country, the number of decisions threshold for
                                      a provenience country to appear, the sex and the age of the applicants.", style="text-align: justify;"), 
                                    p("Data comes from eurostat database migr_asydcfsta.", style="text-align: justify;")
                                    ),
                           tabPanel("RR - Definitive decision", 
                                    plotlyOutput(outputId = "histogram2", height = 600),
                                    htmlOutput("rrdef", style="text-align: justify;"),
                                    p("In the bar plot we represent the recognition rate (RR) defined
                                      as the number of positive decisions over the total decisions
                                      on asylum. The ratio in this case refers to definitive decision of a specific destination country and
                                      it is calculated for each provenience country of the asylum seekers", style="text-align: justify;"),
                                    p("The user can select the year, the country, the number of decisions threshold for
                                      a provenience country to appear, the sex and the age of the applicants.", style="text-align: justify;"), 
                                    p("Data comes from eurostat database migr_asydcfina.", style="text-align: justify;")
                                    )
                           )
               ),
        column(2,
               style = "background-color:#E6EEFF;",
               sliderInput(inputId = "timefra",
                           label = "Year",
                           min = 2008,
                           max = 2020,
                           value = 2020,
                           sep = ""),
               sliderInput(inputId = "threshold",
                           label = "Number of decisions threshold",
                           min = min(asylum$Applications, na.rm = T),
                           max = max(asylum$Applications, na.rm = T),
                           value= c(range(asylum$Applications, na.rm = T))),
               selectInput(inputId = "geol",
                           label = "Select country",
                           choices = unique(asylum$geo)[order(unique(asylum$geo))],
                           selected = "Belgium"),
               selectInput(inputId = "sex",
                           label = "Sex",
                           choices = unique(asylum$sex),
                           selected = "T"),
               selectInput(inputId = "age",
                           label = "Age",
                           choices = unique(asylum$age),
                           selected = "TOTAL"))
      ),
      
      br(),
      
      # first piechar ui --------------------------------------------
      fluidRow(
        column(10,
               style = "background-color:#E6EEFF;",
               tabsetPanel(type = "tabs",
                           tabPanel("Applications", 
                                    plotlyOutput(outputId = "applpie"),
                                    htmlOutput("appltext", style="text-align: justify;"),
                                    p("In this pie chart we represent the number of applications a country recieved 
                                      in a determinate time window with informations about the birth country of applicants.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a provenience country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"), 
                                    p("Data comes from eurostat database migr_asyappctza.", style="text-align: justify;")),
                           tabPanel("Approval by first istance",
                                    plotlyOutput(outputId = "firstpie"),
                                    htmlOutput("firsttext", style="text-align: justify;"),
                                    p("In this pie chart we represent the number of positive decisions by first istance on asylum a country granted 
                                      in a determinate time window with informations about the birth country of applicants.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a provenience country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfsta.", style="text-align: justify;")),
                           tabPanel("Approval by definitive decision",
                                    plotlyOutput(outputId = "defpie"),
                                    htmlOutput("deftext", style="text-align: justify;"),
                                    p("In this pie chart we represent the number of positive decisions by definitive decision on asylum a country granted 
                                      in a determinate time window with informations about the birth country of applicants.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a provenience country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfina.", style="text-align: justify;"))
                           )
               ),
        column(2, 
               style = "background-color:#E6EEFF;",
               sliderInput(inputId = "range",
                        label = "Time window you want to consider",
                        min = 2008,
                        max = 2020,
                        value = c(2008,2020),
                        sep = ""),
               sliderInput(inputId = "threshold_1",
                           label = "Number of applications threshold",
                           min = min(asylum$Applications, na.rm = T),
                           max = max(asylum$Applications, na.rm = T),
                           value= c(range(asylum$Applications, na.rm = T))),
                selectInput(inputId = "geol_1",
                        label = "Select country",
                        choices = unique(asylum$geo)[order(unique(asylum$geo))],
                        selected = "Belgium"),
                selectInput(inputId = "sex_1",
                        label = "Sex",
                        choices = unique(asylum$sex),
                        selected = "T"),
                selectInput(inputId = "age_1",
                        label = "Age",
                        choices = unique(asylum$age),
                        selected = "TOTAL")
               )
            ),
      
      
      br(),
      
      #second piechart ui ----------------------------------------------------
      fluidRow(
        column(10,
               style = "background-color:#E6EEFF;",
               tabsetPanel(type = "tabs",
                           tabPanel("Applications", 
                                    plotlyOutput(outputId = "applpie_co"),
                                    htmlOutput("appltext_co", style="text-align: justify;"),
                                    p("In this pie chart we represent the number of asylum applicants from a certain country 
                                      in a determinate time window with informations about the countries to which they sent the application.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a destination country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asyappctza.", style="text-align: justify;")),
                           tabPanel("Approval by first istance",
                                    plotlyOutput(outputId = "firstpie_co"),
                                    htmlOutput("firsttext_co", style="text-align: justify;"),
                                    p("In this pie chart we represent the number of positive decisions by first istance on asylum applicants from 
                                    a certain coyntry recieved in a determinate time window with informations about the destination countries.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a destination country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfsta.", style="text-align: justify;")),
                           tabPanel("Approval by definitive decision",
                                    plotlyOutput(outputId = "defpie_co"),
                                    htmlOutput("deftext_co", style="text-align: justify;"),
                                    p("In this pie chart we represent the number of positive decisions by definitive decision on asylum applicants from 
                                    a certain coyntry recieved in a determinate time window with informations about the destination countries.", style="text-align: justify;"),
                                    p("The user can select the time window, the country, the number of applications threshold for
                                      a destination country to appear or be grouped in 'Other', the sex and the age of the applicants.", style="text-align: justify;"),
                                    p("Data comes from eurostat database migr_asydcfina.", style="text-align: justify;"))
               )
        ),
        column(2, 
               style = "background-color:#E6EEFF;",
               sliderInput(inputId = "range_2",
                           label = "Time window you want to consider",
                           min = 2008,
                           max = 2020,
                           value = c(2008,2020),
                           sep = ""),
               sliderInput(inputId = "threshold_2",
                           label = "Number of applications threshold",
                           min = min(asylum$Applications, na.rm = T),
                           max = max(asylum$Applications, na.rm = T),
                           value= c(range(asylum$Applications, na.rm = T))),
               selectInput(inputId = "citizen_2",
                           label = "Select country",
                           choices = unique(asylum$citizen)[order(unique(asylum$citizen))],
                           selected = "Syria"),
               selectInput(inputId = "sex_2",
                           label = "Sex",
                           choices = unique(asylum$sex),
                           selected = "T"),
               selectInput(inputId = "age_2",
                           label = "Age",
                           choices = unique(asylum$age),
                           selected = "TOTAL")
        )
      ),
      br(),
      br(),
      br(),
      br()
      
    )
)

