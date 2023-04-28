library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
library(caret)

###################################
####           Main            ####
###################################

ui <- navbarPage(
  "Lumber Market Analysis in Shiny",
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "TimberIndustryUS",
      img(src = "forest.jpg", width = "100%", class = "bg")
    ))
    ),
    
  tags$br(),
    
    
  #####################################
  ####           tabs              ####
  #####################################
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        
        #################
        #### Summary ####
        #################
        
        ### DATA CBP ###
        
        # --------------------------- #
        # ranking $ pie chart section #
        # --------------------------- #
        
        sidebarLayout(
          sidebarPanel(
            h2("Lumber Organization"),
            h3("Data by Year"),
            tags$br(),
            selectInput(
              "checkYear",
              "Select Year",
              choices = unique(data_cbp$YEAR),
              selected = list("2020"="2020","2019"="2019","2018"="2018", "2017"="2017", "2016"="2016",
                              "2015"="2015", "2014"="2014")
            )
          ),
          mainPanel(
            tags$br(),
            tabsetPanel(
              type = "tabs",
              tabPanel("Ranking", tableOutput("datahead")),
              tabPanel("Number of Establishments", plotOutput(outputId = "piePlot"))
            )
          )
        ),
        tags$hr(),
        
        sidebarLayout(
          sidebarPanel(
            
            # ---------------------- #
            #    overview section    #
            # ---------------------- #
            
            h3("Data Overview"),
            tags$br(),
            setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
            sliderInput(
              "payrollRange",
              label = "Annual Payroll Range",
              min = 0,
              max = 6822044,
              value = c(0, 6822044)
            ),
            # setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
            sliderInput(
              "paidEmpRange",
              label = "Paid Employees Range",
              min = 0,
              max = 8261264,
              value = c(0, 8261264)
            ),
            selectInput(
              "checkYearGroup",
              "Select Year",
              choices = list("2020","2019","2018", "2017", "2016",
                             "2015", "2014"),
              selected = list("2020"="2020","2019"="2019","2018"="2018", "2017"="2017", "2016"="2016",
                              "2015"="2015", "2014"="2014"),
              multiple = TRUE
            ),
            actionButton("actionDT", "Filter", class = "btn btn-warning")
          ),
          mainPanel(
            h3("Browse All"),
            tags$br(),
            dataTableOutput("myTable"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
      
      ### DATA PARTNER ###
      
      # --------------------------- #
      # ranking $ pie chart section #
      # --------------------------- #
      
      sidebarLayout(
        sidebarPanel(
          h2("US Export"),
          h3("Data by Year"),
          tags$br(),
          selectInput(
            "checkYearPartner",
            "Select Year",
            choices = unique(data_partner$Year),
            selected = "2020",
            multiple = TRUE
          ),
          selectInput(
            "selectPartnerName",
            "Select State",
            choices = unique(data_partner$Partner.Name)[1:20],
            multiple = TRUE
          )
          
        ),
        
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel("Ranking", tableOutput("dataheadPartner")),
            tabPanel("Number of US Export.", plotOutput(outputId = "piePlotPartnerA")),
            tabPanel("Number of Product Export Share.", plotOutput(outputId = "piePlotPartnerB"))
          ),
          tags$br(),
          tags$br(),
          tags$hr()
        )
      ),
      tags$br(),
      tags$br(),
      tags$hr(),
      
      sidebarLayout(
        sidebarPanel(
          
          # ---------------------- #
          #    overview section    #
          # ---------------------- #
          
          h3("Data Overview"),
          tags$br(),
          setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
          sliderInput(
            "exportRange",
            label = "Annual US Export Range",
            min = 0.01,
            max = 59750917,
            value = c(0.01, 59750917)
          ),
          # setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
          sliderInput(
            "productRange",
            label = "Product Share Range",
            min = 0,
            max = 99.76,
            value = c(0, 99.76)
          ),
          selectInput(
            "checkYearGroupPartner",
            "Select Year",
            choices = unique(data_partner$Year),
            selected = "2020",
            multiple = TRUE
          ),
          actionButton("actionPartner", "Filter", class = "btn btn-warning")
        ),
        mainPanel(
          h3("Browse All"),
          tags$br(),
          dataTableOutput("myTablePartner"),
          tags$br(),
          tags$br()
        )
      ),
      
      ### DATA FUEL ###
      
      # --------------------------- #
      # ranking $ pie chart section #
      # --------------------------- #
      
      sidebarLayout(
        sidebarPanel(
          h2("US Forest Fuel"),
          h3("Data by Year"),
          tags$br(),
          selectInput(
            "checkYearFuel",
            "Select Year",
            choices = unique(data_fuel$INVYR),
            selected = "2020",
            multiple = TRUE
          ),
          selectInput(
            "selectState",
            "Select State",
            choices = unique(data_fuel$STATE),
            selected = "CA",
            multiple = TRUE
          )
          
        ),
        
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel("Ranking", tableOutput("dataheadFuel")),
            tabPanel("Number of US Export.", plotOutput(outputId = "piePlotFuelA")),
            tabPanel("Number of Product Export Share.", plotOutput(outputId = "piePlotFuelB"))
          ),
          tags$br(),
          tags$br(),
          tags$hr()
        )
      ),
      tags$br(),
      tags$br(),
      tags$hr(),
      
      sidebarLayout(
        sidebarPanel(
          
          # ---------------------- #
          #    overview section    #
          # ---------------------- #
          
          h3("Data Overview"),
          tags$br(),
          setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
          sliderInput(
            "plotRange",
            label = "Annual Forest Fuel Range",
            min = 8,
            max = 99997,
            value = c(8, 99997)
          ),
          sliderInput(
            "liveHerbRange",
            label = "Live Herb Range",
            min = 0,
            max = 47,
            value = c(0, 47)
          ),
          selectInput(
            "checkYearGroupFuel",
            "Select Year",
            choices = unique(data_fuel$INVYR),
            selected = "2020",
            multiple = TRUE
          ),
          actionButton("actionFuel", "Filter", class = "btn btn-warning")
        ),
        mainPanel(
          h3("Browse All"),
          tags$br(),
          dataTableOutput("myTableFuel"),
          tags$br(),
          tags$br()
        )
      ),
      tags$hr()
    ),
    
      
      ###############################################
      ####             Plots                     ####
      ###############################################
      
      tabPanel(
        
        "Visual Comparison",
        
        ### DATA FUEL ###
        
        # --------------------------- #
        #    density plot section     #
        # --------------------------- #
        
        sidebarLayout(
          sidebarPanel(
            h3("FUEL Density Plot Panel"),
            tags$br(),
            selectInput(
              "selectvar_fuel",
              label = "Choose a variable to display",
              choices = c(
                "Live shrub code" = "LVSHRBCD",
                "Dead shrub code" = "DSHRBCD",
                "Live herb code" = "LVHRBCD",
                "Dead herb code" = "DHRBCD",
                "Litter code" = "LITTERCD",
                "Live shrub height" = "LVSHRBHT",
                "Dead shrub height" = "DSHRBHT",
                "Live herb height" = "LVHRBHT",
                "Dead herb height" = "DHRBHT"
              ),
              selected = "LITTERCD"
            ),
            
            checkboxGroupInput(
              "checkGroup_fuel",
              label = "Select Year",
              choices = unique(data_fuel$INVYR),
              selected = "2005"
            )
          ),
          mainPanel(
            h3("Distribution"),
            plotlyOutput(outputId = "densityPlot_fuel"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        ### DATA PARTNER ###
        
        # --------------------------- #
        #    density plot section     #
        # --------------------------- #
        
        sidebarLayout(
          sidebarPanel(
            h3("Density Plot Panel Partner"),
            tags$br(),
            selectInput(
              "selectvar_partner",
              label = "Choose a variable to display",
              choices = c(
                "Number of export" = "Export..US..Thousand.",
                "Export product share" = "Export.Product.Share...."
              ),
              selected = c(
                "Number of export" = "Export..US..Thousand.",
                "Export product share" = "Export.Product.Share...."
              )
            ),
            
            selectInput(
              "checkGroup_partner",
              label = "Select partner",
              choices = unique(data_partner$Partner.Name),
              selected = "San Marino",
              multiple = TRUE
            )
          ),
          mainPanel(
            h3("Distribution"),
            plotlyOutput(outputId = "densityPlot_partner"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------------- #
        #   regression plot section   #
        # --------------------------- #
        
        ### DATA PARTNER ###
        
        sidebarLayout(
          sidebarPanel(
            h3("Regression Plot Panel Partner"),
            tags$br(),
            selectInput(
              "selectvar_partner_lr_x",
              label = "X-axis variable",
              choices = c(
                "Number of export" = "Export..US..Thousand.",
                "Export product share" = "Export.Product.Share....",
                "Partner Name" = "Partner.Name"
              ),
              selected = "Export..US..Thousand."
            ),
            selectInput(
              "selectvar_partner_lr_y",
              label = "Y-axis variable",
              choices = c(
                "Number of export" = "Export..US..Thousand.",
                "Export product share" = "Export.Product.Share....",
                "Partner Name" = "Partner.Name"
              ),
              selected = "Partner.Name"
            ),
            selectInput(
              "checkGroup_partner_lr",
              label = "Select partner",
              choices = unique(data_partner$Partner.Name),
              selected = "San Marino",
              multiple = TRUE
            )
          ),
          mainPanel(
            h3("Regression"),
            plotlyOutput(outputId = "regressionPlot_partner"),
            tags$br(),
            tags$br(),
            tableOutput(outputId = "coefficientsTable"),
            tags$br(),
            h3("P-Values Table"),
            tableOutput(outputId = "pValuesTable")
          )
        ),
        
        tags$hr(),
        
        # ------------------------------------------------------------- #
        #     multiple regression with dummy variables plot section     #
        # ------------------------------------------------------------- #
        
        sidebarLayout(
          sidebarPanel(
            h3("Multiple Regression with Dummy Variables"),
            selectInput(
              "xvars", 
              "Select independent variables", 
              choices = names(data_partner)[c(1,2,3,4)],
              selected = "Partner.Name"
            ),
            selectInput(
              "yvar", 
              "Select dependent variable", 
              choices = names(data_partner)[c(1,2,3,4)],
              selected = "Export..US..Thousand."
            )
          ),
          mainPanel(
            plotlyOutput("plot_dm"),
            verbatimTextOutput("summary_dm")
          )
        ),
        tags$hr(),
        
        # --------------------------------- #
        #    random forest plot section     #
        # --------------------------------- #
        
        sidebarLayout(
          sidebarPanel(
            h3("Random Forest"),
            fileInput("file_rm", "Upload RDS File",
                      accept = ".rds"),
            selectInput("y_var_rm", "Select Categorical (Y) Variable", choices = NULL),
            selectInput("x_var_rm", "Select Independent (X) Variable", choices = NULL, multiple = TRUE),
            numericInput("ntree", "Number of trees:", value = 500, min = 1),
            numericInput("min_node_size", "Minimum node size:", value = 1, min = 1),
            selectInput("importance_method", "Variable importance method:", choices = c("none", "impurity", "permutation")),
            actionButton("run_analysis", "Run Analysis")
          ),
          mainPanel(
            plotOutput("category_plot"),
            textOutput("oob_error_rate"),
            verbatimTextOutput("model_summary"),
            downloadButton("download_var_imp", "Download Variable Importance")
          )
        ),
        tags$hr(),
        
        # -------------------- #
        #   box plot section   #
        # -------------------- #

        ### DATA CBP ###
        
        sidebarLayout(
          sidebarPanel(
            h3("US Timber Industry Box Plot Panel"),
            tags$br(),
            checkboxGroupInput(
              "checkGroupbox",
              label = "Select Size of Estab",
              choices = unique(data_cbp$EMPSZES_LABEL),
              selected = list("All establishments"="All establishments",
                              "Establishments with 1 to 4 employees"="Establishments with 1 to 4 employees",       
                              "Establishments with 5 to 9 employees"="Establishments with 5 to 9 employees",
                              "Establishments with 10 to 19 employees"="Establishments with 10 to 19 employees",     
                              "Establishments with 20 to 49 employees"="Establishments with 20 to 49 employees",
                              "Establishments with 50 to 99 employees"="Establishments with 50 to 99 employees",     
                              "Establishments with 100 to 249 employees"="Establishments with 100 to 249 employees",
                              "Establishments with 250 to 499 employees"="Establishments with 250 to 499 employees",   
                              "Establishments with 500 to 999 employees"="Establishments with 500 to 999 employees",
                              "Establishments with 1,000 employees or more"="Establishments with 1,000 employees or more",
                              "Establishments with less than 5 employees"="Establishments with less than 5 employees")
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Median ESTAB Comparison (aggregate)"),
            plotlyOutput(outputId = "boxPlot"),
            tags$br(),
            tags$br(),
            tags$br()
          )
        ),
        
        ### DATA COARSE ###
        
        sidebarLayout(
          sidebarPanel(
            h3("Coarse Wood Box Plot Panel"),
            tags$br(),
            checkboxGroupInput(
              "checkGroupboxCoarse",
              label = "Select State",
              choices = unique(data_coarse$STATE),
              selected = "ME"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Median State Hollow Comparison (aggregate)"),
            plotlyOutput(outputId = "boxPlotCoarse"),
            tags$br(),
            tags$br(),
            tags$br()
          )
        )
    ),
      
      
      ################################################
      ####             Details                    ####
      ################################################
    
      ### ------------------------------------------------ ###
      ## Details By Organization and Establishments by Year ##
      ### ------------------------------------------------ ###
    
      tabPanel(
        "Details By Organization and Establishments by Year",
        h3("Type of Organization and Size of Establishments by Year", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(
                4,
                selectInput(
                  "detailOrganization",
                  label = "Select Type of Organizations",
                  choices = unique(data_cbp$LFO_LABEL),
                  selected = "Partnerships",
                  width = 400
                )
              ),
              column(
                4,
                selectInput(
                  "detailEstablishment",
                  "Select Size of Establishments",
                  choices = unique(data_cbp$EMPSZES_LABEL),
                  selected = list("All establishments"="All establishments",
                                  "Establishments with 1 to 4 employees"="Establishments with 1 to 4 employees",       
                                  "Establishments with 5 to 9 employees"="Establishments with 5 to 9 employees",
                                  "Establishments with 10 to 19 employees"="Establishments with 10 to 19 employees",     
                                  "Establishments with 20 to 49 employees"="Establishments with 20 to 49 employees",
                                  "Establishments with 50 to 99 employees"="Establishments with 50 to 99 employees",     
                                  "Establishments with 100 to 249 employees"="Establishments with 100 to 249 employees",
                                  "Establishments with 250 to 499 employees"="Establishments with 250 to 499 employees",   
                                  "Establishments with 500 to 999 employees"="Establishments with 500 to 999 employees",
                                  "Establishments with 1,000 employees or more"="Establishments with 1,000 employees or more",
                                  "Establishments with less than 5 employees"="Establishments with less than 5 employees"),
                  width = 800
                )
              ),
              column(4,
                     column(
                       8,
                       selectInput(
                         "detailYear",
                         "Select Year",
                         choices = list("2020","2019","2018", "2017", "2016",
                                        "2015", "2014"),
                         selected = list("2020"="2020","2019"="2019","2018"="2018", "2017"="2017", "2016"="2016",
                                         "2015"="2015", "2014"="2014"),
                         multiple = TRUE,
                         width = 900
                       )
                     ),
                     column(
                       4,
                       tags$br(),
                       actionButton("detailFilter", "Filter", class = "btn btn-warning btn-sm")
                     ))
            )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        
        fluidRow(
          column(4, tableOutput("detailTable")),
          column(4, h5("Annual Number of Establishments", align="center"), plotlyOutput(outputId = "detailPlot", height = "400px")),
          column(4, h5("Annual Payroll", align="center"), plotlyOutput(outputId = "detailPlotem", height = "400px"))
        ),
        tags$hr(),
        tags$br()
      ),
      
      ### ------------------------------------ ###
      ## Details Of US Export By State and Year ##
      ### ------------------------------------ ###
    
      tabPanel(
        "Details Of US Export By State and Year",
        h3("Details Of US Export By State and Year", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(
                4,
                selectInput(
                  "detailStatePartner",
                  label = "Select State",
                  choices = unique(data_partner$Partner.Name),
                  selected = "Uruguay",
                  multiple = TRUE,
                  width = 400
                )
              ),
              column(
                4,
                selectInput(
                  "detailYearPartner",
                  "Select Year",
                  choices = NULL,
                  selected = "2018",
                  multiple = TRUE,
                  width = 400
                )
              ),
              column(
                4,
                tags$br(),
                actionButton("detailFilterPartner", "Filter", class = "btn btn-warning btn-sm")
              )
              
            )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        
        fluidRow(
          column(4, tableOutput("detailTablePartner")),
          column(4, h5("Annual US Export", align="center"), plotlyOutput(outputId = "detailPlotPartner", height = "400px")),
          column(4, h5("Annual Export Product Share", align="center"), plotlyOutput(outputId = "detailPlotemPartner", height = "400px"))
        ),
        tags$hr(),
        tags$br()
      ),
    
    ### ----------------------------------------- ###
    ## Details Of US Forest Fuel By State and Year ##
    ### ----------------------------------------- ###
    
    tabPanel(
      "Details Of US Forest Fuel By State and Year",
      h3("Details Of US Forest Fuel By State and Year", align = "center"),
      br(),
      div(style = "display:vertical-align:center;center-align",
          fluidRow(
            column(
              4,
              selectInput(
                "detailStateFuel",
                label = "Select State",
                choices = unique(data_fuel$STATE),
                selected = "CA",
                multiple = TRUE,
                width = 400
              )
            ),
            column(
              4,
              selectInput(
                "detailYearFuel",
                "Select Year",
                choices = unique(data_fuel$INVYR),
                selected = "2005",
                multiple = TRUE,
                width = 400
              )
            ),
            column(
              4,
              tags$br(),
              actionButton("detailFilterFuel", "Filter", class = "btn btn-warning btn-sm")
            )
            
          )),
      
      tags$br(),
      tags$br(),
      tags$hr(),
      tags$br(),
      
      fluidRow(
        column(4, tableOutput("detailTableFuel")),
        column(4, h5("Annual US Live Herb", align="center"), plotlyOutput(outputId = "detailPlotFuel", height = "400px")),
        column(4, h5("Annual US Live Herb Height", align="center"), plotlyOutput(outputId = "detailPlotemFuel", height = "400px"))
      ),
      tags$hr(),
      tags$br()
    )
    )
)









