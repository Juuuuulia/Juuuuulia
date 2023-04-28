###################################
####           Main            ####
###################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(kableExtra)
library(ggthemes)
library(plotly)
library(GGally)
library(rsconnect)
library(randomForest)
library(shinyWidgets)
library(shinythemes)
library(caret)

#################################
####    DATASETS             ####
#################################

data_cbp <- readRDS("/Users/stella/Desktop/test/data/cbp.rds")
data_partner <- readRDS("/Users/stella/Desktop/test/data/data_partner.rds")
data_coarse<- readRDS("/Users/stella/Desktop/test/data/data_coarse.rds")

#################################
####    DATATABLE VIEW       ####
#################################

opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(
    targets = c(1, 6), searchable = FALSE
  ))
)


server <- function(session, input, output) {
  
  #####################################
  ####         Summary             ####
  #####################################
  
  # ---------------- #
  # summary section  #
  # ---------------- #
  
  ##DATA CBP##
  
  output$datahead <- renderTable({
    data_cbp %>%
      filter(YEAR == input$checkYear) %>%
      group_by(LFO_LABEL) %>%
      select(LFO_LABEL,
             ESTAB,
             EMP) %>%
      summarise_all(funs(mean)) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(ESTAB))
  }, 
  row.names = FALSE,
  col.names = c(
    "Type of Organization",
    "Average Size of Establishments (%)",
    "Average Paid Employees for Pay Period"
  ),
  class = "table table-striped"
  )
  

  # ---------------- #
  # Pie Plot section #
  # ---------------- #
  
  output$piePlot <- renderPlot({
    colmap <-
      c(
        "#bdb2ff",
                 # NUS
                 "#ffc6ff",
                 # NTU
                 "#fffffc",
                 # SMU
                 "#33658A",
                 # SIT
                 "#3a506b",
                 # SUTD
                 "#577590",
                 # SUSS
                 "#43aa8b",
                 # NIE
                 "#90be6d",
                 # SP
                 "#f9c74f",
                 # NP
                 "#f8961e",
                 # TP
                 "#f3722c",
                 # NAYANG POLY
                 "#f94144",
                 # RP
                 "#ffadad",
                 # NAFA DEG
                 "#ffd6a5",
                 # LAS DEG
                 "#fdffb6",
                 # NAFA DIP
                 "#caffbf",
                 # NAFA DEG
                 "#a8dadc"  # ITE
      )
    
    data_cbp %>%
      filter(YEAR == input$checkYear) %>%
      group_by(LFO_LABEL) %>%
      tally(ESTAB) %>%
      ggplot(aes(x = "", y = n, fill = LFO_LABEL)) +
      geom_bar(stat = "identity", width = 1, 
               color = "black", size = 1) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(colmap)) +
      labs(title = "Annual Organizations by Number of Establishments")
    
  })
  
 
  # ------------------ #
  # Data Table Section #
  # ------------------ #
  
  # checkgroup:
  
  yearGroup <- reactive({
    input$actionDT
    isolate(return(data_cbp[data_cbp$YEAR %in% input$checkYearGroup, ]))
  })
  
  
  filtered_DT <- reactive({
    input$actionDT
    isolate({
      minPayroll <- input$payrollRange[1]
      maxPayroll <- input$payrollRange[2]
      minEmploy <- input$paidEmpRange[1]
      maxEmploy <- input$paidEmpRange[2]
    })
    
    yearGroup() %>%
      filter(EMP > minPayroll,
             EMP < maxPayroll) %>%
      filter(PAYANN > minEmploy,
             PAYANN < maxEmploy) %>%
      select(1, 2, 3, 4, 5, 6, 7, 8)
  })
  
  # render data table:
  
  output$myTable <- renderDataTable({
    filtered_DT() %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T),
        colnames = c(
          "NAME",
          "LFO_LABEL",
          "EMPSZES_LABEL",
          "YEAR",
          "ESTAB",
          "EMP",
          "PAYANN"
        )
      )
    
  })
  
  ### DATA PARTNER ###
  
  # ---------------- #
  # summary section #
  # ---------------- #
  
  ##render data table
  data_partner$Year <- as.numeric(as.character(data_partner$Year))
  
  output$dataheadPartner <- renderTable({
    data_partner %>%
      filter(Year %in% input$checkYearPartner) %>%
      group_by(Year) %>%
      select(Year,
             Export.Product.Share....,
             Export..US..Thousand.) %>%
      summarise_all(funs(mean)) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(Export.Product.Share....))
  }, 
  row.names = FALSE,
  col.names = c(
    "LFO_LABEL",
    "Product Share (Avg %)",
    "US Export Avg"
  ),
  class = "table table-striped"
  )
  
  # ---------------- #
  # pie plot section #
  # ---------------- #
  
  ### us export ###
  output$piePlotPartnerA <- renderPlot({
    colmap <- colorRampPalette(brewer.pal(12, "Set3"))(20)
    data_partner %>%
      filter(Year %in% input$checkYearPartner, Partner.Name %in% input$selectPartnerName) %>%
      group_by(Partner.Name) %>%
      tally(Export..US..Thousand.) %>%
      ggplot(aes(x = "", y = n, fill = Partner.Name)) +
      geom_bar(stat = "identity", width = 1, 
               color = "black", size = 1) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(colmap)) +
      labs(title = "US Export Thousand ($) by Year")
    
  })
  
  ### product share ###
  
  output$piePlotPartnerB <- renderPlot({
    colmap <- colorRampPalette(brewer.pal(12, "Set3"))(20)
    data_partner %>%
      filter(Year %in% input$checkYearPartner, Partner.Name %in% input$selectPartnerName) %>%
      group_by(Partner.Name) %>%
      tally(Export.Product.Share....) %>%
      ggplot(aes(x = "", y = n, fill = Partner.Name)) +
      geom_bar(stat = "identity", width = 1, 
               color = "black", size = 1) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(colmap)) +
      labs(title = "Export Product Share by Year (%)")
    
  })
  
  # ------------------ #
  # data table section #
  # ------------------ #
  
  # checkgroup input:
  
  yearGroupPartner <- reactive({
    input$actionPartner
    isolate(return(data_partner[data_partner$Year %in% input$checkYearGroupPartner, ]))
  })
  
  
  filtered_Partner <- reactive({
    input$actionPartner
    isolate({
      minExport <- input$exportRange[1]
      maxExport <- input$exportRange[2]
      minProduct <- input$productRange[1]
      maxProduct <- input$productRange[2]
    })
    
    yearGroupPartner() %>%
      filter(Export..US..Thousand. > minExport,
             Export..US..Thousand. < maxExport) %>%
      filter(Export.Product.Share.... > minProduct,
             Export.Product.Share.... < maxProduct) %>%
      select(1, 2, 3, 4)
  })
  
  # render Partner:
  
  output$myTablePartner <- renderDataTable({
    filtered_Partner() %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T),
        colnames = c(
          "Partner.Name",
          "Year",
          "Export..US..Thousand.",
          "Export.Product.Share...."
        )
      )
  })
  
  
  ### DATA FUEL ###
  
  # ---------------- #
  # summary section #
  # ---------------- #
  
  data_fuel$INVYR <- as.numeric(as.character(data_fuel$INVYR))
  
  output$dataheadFuel <- renderTable({
    data_fuel %>%
      dplyr::filter(INVYR %in% input$checkYearFuel) %>%
      group_by(INVYR) %>%
      select(INVYR,
             LVSHRBHT,
             LVHRBHT) %>%
      summarise_all(funs(mean)) %>%
      mutate_if(is.numeric, round, 0) %>%
      arrange(desc(LVHRBHT))
  }, 
  row.names = FALSE,
  col.names = c(
    "Year",
    "Live Shrub Height (Avg %)",
    "Live Herb Height Avg"
  ),
  class = "table table-striped"
  )
  
  # ---------------- #
  # pie plot section #
  # ---------------- #
  
  # live herb height
  
  output$piePlotFuelA <- renderPlot({
    colmap <- colorRampPalette(brewer.pal(12, "Set3"))(20)
    data_fuel %>%
      filter(INVYR %in% input$checkYearFuel, STATE %in% input$selectState) %>%
      group_by(STATE) %>%
      tally(LVHRBHT) %>%
      ggplot(aes(x = "", y = n, fill = STATE)) +
      geom_bar(stat = "identity", width = 1, 
               color = "black", size = 1) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(colmap)) +
      labs(title = "Live Herb Height by Year")
    
  })
  
  #live herb
  
  output$piePlotFuelB <- renderPlot({
    colmap <- colorRampPalette(brewer.pal(12, "Set3"))(20)
    data_fuel %>%
      filter(INVYR %in% input$checkYearFuel, STATE %in% input$selectState) %>%
      group_by(STATE) %>%
      tally(LVHRBCD) %>%
      ggplot(aes(x = "", y = n, fill = STATE)) +
      geom_bar(stat = "identity", width = 1, 
               color = "black", size = 1) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(colmap)) +
      labs(title = "Live Herb by Year (%)")
    
  })
  
  # ------------------ #
  # data table section #
  # ------------------ #
  
  # checkgroup input:
  
  yearGroupFuel <- reactive({
    input$actionFuel
    isolate(return(data_fuel[data_fuel$INVYR %in% input$checkYearGroupFuel, ]))
  })
  
  filtered_Fuel <- reactive({
    input$actionFuel
    isolate({
      minPlot <- input$plotRange[1]
      maxPlot <- input$plotRange[2]
      minLiveHerb <- input$liveHerbRange[1]
      maxLiveHerb <- input$liveHerbRange[2]
    })
    
    yearGroupFuel() %>%
      dplyr::filter(PLOT > minPlot,
                    PLOT < maxPlot) %>%
      dplyr::filter(LVHRBHT > minLiveHerb,
                    LVHRBHT < maxLiveHerb) %>%
      select(2,5,7,8,9,10,11,12,13,14,15)
  })
  
  # render fuel:
  
  output$myTableFuel <- renderDataTable({
    filtered_Fuel() %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T),
        colnames = c(
          "Year",
          "Plot Number",
          "Live Shrub",
          "Dead Shrub",
          "Live Herb",
          "Dead Herb",
          "Litter",
          "Live Shrub Height",
          "Dead Shrub Height",
          "State"
        )
      )
    
  })
  
  ####################################
  ####          Plots             ####
  ####################################
  
  # -------------------- #
  # density plot section #
  # -------------------- #
  
  ### DATA CBP ###
  
  # checkgroup input:
  
  dent <-  reactive({
    return(data_fuel[data_fuel$INVYR %in% input$checkGroup, ])
    
  })

  # CBP density plot
  
  output$densityPlot <- renderPlotly({
    colmap <- c(
      "#2c3e50",
               "#e67e22",
               "#f1c40f",
               "#e74c3c",
               "#F97F51",
               "#27ae60",
               "#2980b9",
               "#86BBD8",
               "#8e44ad",
               "#95a5a6",
               "#f39c12",
               "#d35400",
               "#c0392b",
               "#bdc3c7",
               "#D6A2E8",
               "#25CCF7",
               "#16a085"
    )
                         
    ggplotly(
      ggplot(data = dent(), aes_string(x = input$selectvar)) +
        geom_density(aes(fill = INVYR), size = 1, alpha=0.75) +
        theme(legend.position = "bottom") + labs(x = input$selectvar) +
        scale_fill_manual(values = colmap) +
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    ) %>% layout(legend = list(orientation = "h",
                               y = 0, x = 0))
  })
  
  ### DATA PARTNER ###
  
  # checkgroup input:
  
  dent_partner <-  reactive({
    return(data_partner[data_partner$Partner.Name %in% input$checkGroup_partner, ])
    
  })
  
  # render density plot
  
  output$densityPlot_partner <- renderPlotly({
    colmap <- c(
      "#2c3e50",
               "#e67e22",
               "#f1c40f",
               "#e74c3c",
               "#F97F51",
               "#27ae60",
               "#2980b9",
               "#86BBD8",
               "#8e44ad",
               "#95a5a6",
               "#f39c12",
               "#d35400",
               "#c0392b",
               "#bdc3c7",
               "#D6A2E8",
               "#25CCF7",
               "#16a085"
    )
    
    ggplotly(
      ggplot(data = dent_partner(), aes_string(x = input$selectvar_partner)) +
        geom_density(aes(fill = Partner.Name), size = 1, alpha=0.75) +
        theme(legend.position = "bottom") + labs(x = input$selectvar_partner) +
        scale_fill_manual(values = colmap) +
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    ) %>% layout(legend = list(orientation = "h",
                               y = 0, x = 0))
    
  })
  
  ### DATA FUEL ###
  
  # checkgroup input:
  
  dent_fuel <-  reactive({
    return(data_fuel[data_fuel$INVYR %in% input$checkGroup_fuel, ])
  })
  
  
  # render fuel density plot
  
  output$densityPlot_fuel <- renderPlotly({
    colmap <- c(
      "#2c3e50",
               "#e67e22",
               "#f1c40f",
               "#e74c3c",
               "#F97F51",
               "#27ae60",
               "#2980b9",
               "#86BBD8",
               "#8e44ad",
               "#95a5a6",
               "#f39c12",
               "#d35400",
               "#c0392b",
               "#bdc3c7",
               "#D6A2E8",
               "#25CCF7",
               "#16a085"
    )
    
    ggplotly(
      ggplot(data = dent_fuel(), aes_string(x = input$selectvar_fuel)) +
        geom_density(aes(fill = STATE), size = 1, alpha=0.75) +
        theme(legend.position = "bottom") + labs(x = input$selectvar_fuel) +
        scale_fill_manual(values = colmap) +
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    ) %>% layout(legend = list(orientation = "h",
                               y = 0, x = 0))
    
  })
  
  # ----------------------- #
  # regression plot section #
  # ----------------------- #
  
  ### DATA PARTNER ###
  
  # checkgroup input:

  dent_partner_lr <- reactive({
    return(data_partner[data_partner$Partner.Name %in% input$checkGroup_partner_lr,])
  })
  
  output$regressionPlot_partner <- renderPlotly({
    colmap <-
      c(
        "#bdb2ff",
                 # NUS
                 "#ffc6ff",
                 # NTU
                 "#fffffc",
                 # SMU
                 "#33658A",
                 # SIT
                 "#3a506b",
                 # SUTD
                 "#577590",
                 # SUSS
                 "#43aa8b",
                 # NIE
                 "#90be6d",
                 # SP
                 "#f9c74f",
                 # NP
                 "#f8961e",
                 # TP
                 "#f3722c",
                 # NAYANG POLY
                 "#f94144",
                 # RP
                 "#ffadad",
                 # NAFA DEG
                 "#ffd6a5",
                 # LAS DEG
                 "#fdffb6",
                 # NAFA DIP
                 "#caffbf",
                 # NAFA DEG
                 "#a8dadc"  # ITE
      )
    
    df_lr <- dent_partner_lr()
    
    # Convert state variable to a factor
    df_lr$Partner.Name <- as.factor(df_lr$Partner.Name)
    
    if (input$selectvar_partner_lr_y == "Partner.Name") {
      # If Y variable is categorical, generate logistic regression model and plot it
      formula_lr <- as.formula(paste(input$selectvar_partner_lr_y, "~", input$selectvar_partner_lr_x))
      model_lr <- glm(formula_lr, data = df_lr, family = "binomial")
      plot_lr <- plot_ly(x = df_lr[, input$selectvar_partner_lr_x], y = model_lr$fitted.values, type = "scatter", mode = "lines",
                         line = list(color = "#16a085"))
      p_value <- sprintf("p-value: %.4f", summary(model_lr)$coefficients[2, 4])
    } else {
      # If Y variable is numerical, generate linear regression model and plot it
      formula_lr <- as.formula(paste(input$selectvar_partner_lr_y, "~", input$selectvar_partner_lr_x))
      model_lr <- lm(formula_lr, data = df_lr)
      plot_lr <- plot_ly(x = df_lr[, input$selectvar_partner_lr_x], y = model_lr$fitted.values, type = "scatter", mode = "lines",
                         line = list(color = "#16a085"))
      p_value <- sprintf("p-value: %.4f", summary(model_lr)$coefficients[2, 4])
    }
    
    # Customize plot layout
    plot_lr <- plot_lr %>% layout(
      title = "Regression Plot",
      xaxis = list(title = input$selectvar_partner_lr_x),
      yaxis = list(title = ifelse(input$selectvar_partner_lr_y == "Partner.Name", "Probability", "Export (US Thousand)")),
      annotations = list(
        x = 0.95, y = 1.05,
        text = p_value,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 14)
      )
    )
    
    # Add markers for each data point
    plot_lr <- plot_lr %>% add_markers(x = df_lr$Partner.Name, y = df_lr[, input$selectvar_partner_lr_y],
                                       marker = list(color = colmap[1:length(unique(df_lr$Partner.Name))], size = 10))
    
    plot_lr
  })
  
    # Display coefficients of the logistic regression model or linear regression model
  output$coefficientsTable <- renderTable({
    df_lr <- dent_partner_lr()
    
    # at least 2 levels for the factor variable
    if (input$selectvar_partner_lr_x == "Partner.Name" && nlevels(df_lr$Partner.Name) < 2) {
      return(NULL)
    }
    
    if (input$selectvar_partner_lr_y == "Partner.Name") {
      # If Y variable is categorical, generate logistic regression model
      formula_lr <- as.formula(paste(input$selectvar_partner_lr_y, "~", input$selectvar_partner_lr_x))
      model_lr <- glm(formula_lr, data = df_lr, family = "binomial")
      coefficients_table <- summary(model_lr)$coefficients
    } else {
      # If Y variable is numerical, generate linear regression model
      formula_lr <- as.formula(paste(input$selectvar_partner_lr_y, "~", input$selectvar_partner_lr_x))
      model_lr <- lm(formula_lr, data = df_lr)
      coefficients_table <- summary(model_lr)$coefficients
    }
    
    if (nrow(coefficients_table) >= 2) {
      rownames(coefficients_table) <- c("(Intercept)", input$selectvar_partner_lr_x)
    }
    coefficients_table
  })
  
  
  output$pValuesTable <- renderTable({
    df_lr <- dent_partner_lr()
    p_values <- data.frame(Partner = character(), P_value = numeric())
    
    for (partner in input$checkGroup_partner_lr) {
      df_temp <- df_lr[df_lr$Partner.Name == partner,]
      if (input$selectvar_partner_lr_y == "Partner.Name") {
        # If Y variable is categorical, generate logistic regression model
        formula_lr <- as.formula(paste(input$selectvar_partner_lr_y, "~", input$selectvar_partner_lr_x))
        model_lr <- glm(formula_lr, data = df_temp, family = "binomial")
        p_value <- summary(model_lr)$coefficients[2, 4]
      } else {
        # If Y variable is numerical, generate linear regression model
        formula_lr <- as.formula(paste(input$selectvar_partner_lr_y, "~", input$selectvar_partner_lr_x))
        model_lr <- lm(formula_lr, data = df_temp)
        p_value <- summary(model_lr)$coefficients[2, 4]
      }
      
      p_values <- rbind(p_values, data.frame(Partner = partner, P_value = p_value))
    }
    
    p_values
  })

  # ---------------------------------------- #
  # multiple regression with dummy variables #
  # ---------------------------------------- #
  
  ### DATA PARTNER ###
  
  # Generate dummy variables
  df_dm <- reactive({
    
    df_dm <- data_partner
    
    # Convert categorical variable to factor
    df_dm$Partner.Name <- as.factor(df_dm$Partner.Name)
    
    # Create dummy variables
    dummies <- model.matrix(~ Partner.Name - 1, data = df_dm)
    colnames(dummies) <- paste0("Partner_", levels(df_dm$Partner.Name))
    df_dm <- cbind(df_dm, dummies)
    
    return(df_dm)
  })
  
  # Generate multiple linear regression model
  model_dm <- reactive({
    # Get selected variables
    xvars <- input$xvars
    yvar <- input$yvar
    
    # Create formula for regression model
    formula_dm <- as.formula(paste(yvar, "~", xvars))
    
    # Create model
    lm(formula_dm, data = df_dm())
  })
  
  # Generate plot
  output$plot_dm <- renderPlotly({
    colmap <-
      c(
        "#bdb2ff",
                 "#ffc6ff",
                 "#fffffc",
                 "#33658A",
                 "#3a506b",
                 "#577590",
                 "#43aa8b",
                 "#90be6d",
                 "#f9c74f",
                 "#f8961e",
                 "#f3722c",
                 "#f94144",
                 "#ffadad",
                 "#ffd6a5",
                 "#fdffb6",
                 "#caffbf",
                 "#a8dadc",
                 "#6a4c93",
                 "#183446",
                 "#ff6e40",
                 "#889d9e",
                 "#7f2d2d",
                 "#596f62",
                 "#a9cba4",
                 "#4a4a4a",
                 "#b3a394",
                 "#8f8c89"
      )
    # Create scatterplot
    plot_dm <- plot_ly(df_dm(), x = input$xvars, y = input$yvar, color = ~ factor(Partner.Name), colors = colmap) %>% 
      add_markers()
    
    # Add regression line
    plot_dm <- plot_dm %>% 
      add_trace(x = df_dm()[, input$xvars], y = model_dm()$fitted.values, mode = "lines", line = list(color = "#FFA07A"), name = "Regression line")
    
    # Set plot layout
    plot_dm <- plot_dm %>% 
      layout(title = "Multiple Linear Regression with Dummy Variables",
             xaxis = list(title = input$xvars),
             yaxis = list(title = input$yvar))
    
    return(plot_dm)
  })
  # Generate summary
  output$summary_dm <- renderPrint({
    summary(model_dm())
  })
  
  # -------------------------- #
  # random forest plot section #
  # -------------------------- #
  
  ### UPLOAD DATASET ###
  
  # Define ntree
  ntree <- 500
  
  # Define color palette
  colmap <-
    c(
      "#bdb2ff",
               "#ffc6ff",
               "#fffffc",
               "#33658A",
               "#3a506b",
               "#577590",
               "#43aa8b",
               "#90be6d",
               "#f9c74f",
               "#f8961e",
               "#f3722c",
               "#f94144",
               "#ffadad",
               "#ffd6a5",
               "#fdffb6",
               "#caffbf",
               "#a8dadc",
               "#6a4c93",
               "#183446",
               "#ff6e40",
               "#889d9e",
               "#7f2d2d",
               "#596f62",
               "#a9cba4",
               "#4a4a4a",
               "#b3a394",
               "#8f8c89"
    )
  
  data_rm <- reactive({
    req(input$file_rm)
    readRDS(input$file_rm$datapath)
  })
  
  # Update choices for categorical and independent variable inputs
  observe({
    req(data_rm())
    updateSelectInput(session, "y_var_rm", choices = names(data_rm()))
    updateSelectInput(session, "x_var_rm", choices = names(data_rm()))
  })
  
  # Perform category analysis
  category <- eventReactive(input$run_analysis, {
    req(data_rm(), input$y_var_rm, input$x_var_rm)
    data_subset <- data_rm()[, c(input$y_var_rm, input$x_var_rm)]
    
    # Convert categorical variable to factor
    data_subset[, input$y_var_rm] <- as.factor(data_subset[, input$y_var_rm])
    
    # Check and convert non-numeric columns to numeric
    numeric_columns <- sapply(data_subset, is.numeric)
    non_numeric_columns <- input$x_var_rm[!numeric_columns[input$x_var_rm]]
    
    for (col in non_numeric_columns) {
      data_subset[, col] <- as.numeric(as.character(data_subset[, col]))
    }
    
    data_subset <- na.omit(data_subset)
    rf_model <- randomForest(as.formula(paste0(input$y_var_rm, " ~ .")), data = data_subset, ntree = 500)
    var_imp <- importance(rf_model)
    var_imp_df <- data.frame(Variable = rownames(var_imp), Importance = var_imp[, 1])
    oob_error_rate <- round(rf_model$err.rate[nrow(rf_model$err.rate), "OOB"], 3)
    list(plot = ggplot(var_imp_df, aes(x = reorder(Variable, -Importance), y = Importance)) + 
           geom_bar(stat = "identity", aes(fill = Variable)) + 
           ggtitle("Variable Importance Plot") +
           xlab("Variable") + 
           ylab("Importance") +
           scale_fill_manual(values = colmap),
         oob_error_rate = oob_error_rate,
         model_summary = summary(rf_model))
  })
  
  # Output category plot
  output$category_plot <- renderPlot({
    req(category()$plot)
    category()$plot
  })
  
  # Output OOB error rate
  output$oob_error_rate <- renderText({
    req(category()$oob_error_rate)
    paste("Out-of-bag error rate:", category()$oob_error_rate)
  })
  
  # Output model summary
  output$model_summary <- renderPrint({
    req(category()$model_summary)
    category()$model_summary
  })


  # ----------------------- #
  # box plot section #
  # ----------------------- #
  
  ### DATA CBP ###

  output$boxPlot <- renderPlotly({
    
    colmap <- c(
      "#2c3e50",
               "#e67e22",
               "#f1c40f",
               "#e74c3c",
               "#F97F51",
               "#27ae60",
               "#2980b9",
               "#86BBD8",
               "#8e44ad",
               "#95a5a6",
               "#f39c12",
               "#d35400",
               "#c0392b",
               "#bdc3c7",
               "#D6A2E8",
               "#25CCF7",
               "#16a085"
    )
    
    uniMedian <-  reactive({
      return(data_cbp[data_cbp$EMPSZES_LABEL%in%input$checkGroupbox, ])
    })
    
    # check null 
    
    if (is.null(data_cbp)) {
      return(NULL)
    } else {
      ggplotly(
        ggplot(uniMedian(),
               aes(
                 x = LFO_LABEL,
                 y = YEAR,
                 fill = EMPSZES_LABEL
               )) + geom_boxplot(
                 # custom boxes
                 
                 
                 # custom outliers
                 outlier.colour = "red",
                 outlier.fill = "red",
                 outlier.size = 3
               ) +
          scale_fill_manual(values = colmap) +
          geom_jitter(
            color = "blue",
            size = 0.4,
            alpha = 0.9
          ) +
          theme(
            legend.position = "top",
            plot.title = element_text(size = 14)
          ) +
          
          ggtitle("A boxplot with jitter")
      )
    }
  })
  
  
  ### DATA COARSE ###
  
  output$boxPlotCoarse <- renderPlotly({
    
    colmap <- c(
      "#2c3e50",
               "#e67e22",
               "#f1c40f",
               "#e74c3c",
               "#F97F51",
               "#27ae60",
               "#2980b9",
               "#86BBD8",
               "#8e44ad",
               "#95a5a6",
               "#f39c12",
               "#d35400",
               "#c0392b",
               "#bdc3c7",
               "#D6A2E8",
               "#25CCF7",
               "#16a085"
    )
    
    uniMedianCoarse <-  reactive({
      return(data_coarse[data_coarse$STATE%in%input$checkGroupboxCoarse, ])
    })
    
    if (is.null(data_coarse)) {
      return(NULL)
    } else {
      ggplotly(
        ggplot(uniMedianCoarse(),
               aes(
                 x = HOLLOWCD,
                 y = INVYR,
                 fill = STATE
               )) + geom_boxplot(
                 # custom boxes
                 # custom outliers
                 outlier.colour = "red",
                 outlier.fill = "red",
                 outlier.size = 3
               ) +
          scale_fill_manual(values = colmap) +
          geom_jitter(
            color = "blue",
            size = 0.4,
            alpha = 0.9
          ) +
          theme(
            legend.position = "top",
            plot.title = element_text(size = 14)
          ) +
          
          ggtitle("A boxplot with jitter")
      )
    }
  })
  ############################################
  ####            Details                 ####
  ############################################
  
  ### DATA CBP ###
  
  # update select for org
  
  observeEvent(
    input$detailOrganizations,
    updateSelectInput(
      session,
      "detailOrganization",
      "Select Type of Organization",
      choices = unique(data_cbp$LFO_LABEL[data_cbp$EMPSZES_LABEL == input$detailEstablishment])
    )
  )
  
  # update select for estab
  
  observeEvent(
    input$detailEstablishment,
    updateSelectInput(
      session,
      "detailYear",
      "Select Year",
      choices = unique(data_cbp$YEAR[data_cbp$EMPSZES_LABEL == input$detailEstablishment &
                                       data_cbp$LFO_LABEL == input$detailOrganization])
    )
  )
  
  # update data table
  
  detailTB <- eventReactive(input$detailYear,
                            {
                              data_cbp %>%
                                filter(
                                  EMPSZES_LABEL == input$detailEstablishment &
                                    LFO_LABEL == input$detailOrganization &
                                    YEAR == input$detailYear
                                ) %>%
                                select(c(
                                  "YEAR",
                                  "ESTAB",
                                  "PAYANN"
                                ))
                              
                            })
  
  output$detailTable <- renderTable({
    input$detailFilter
    isolate({
      detailTB() %>%
        data.frame()
        
    })
  })
  
  # median estab plot:
  
  output$detailPlot <- renderPlotly({
    input$detailFilter
    
    isolate({
      p <- ggplot(detailTB(), aes(x = YEAR, y = ESTAB)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = ESTAB),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = YEAR, y = ESTAB),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +
        
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  })
  
  # payroll
  
  output$detailPlotem <- renderPlotly({
    input$detailFilter
    
    isolate({
      p <- ggplot(detailTB(), aes(x = YEAR, y = PAYANN)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = PAYANN),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = YEAR, y = PAYANN),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +
        
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  })
  
  ### DATA PARTNER ###
  
  observe({
    updateSelectInput(session, "detailYearPartner", choices = unique(data_partner$Year[data_partner$Partner.Name %in% input$detailStatePartner]))
  })
  
  observeEvent(
    input$detailStatePartner,
    updateSelectInput(
      session,
      "detailStatePartner",
      "Select State",
      choices = unique(data_partner$Partner.Name),
      selected = input$detailStatePartner
    )
  )
  
  observeEvent(
    input$detailStatePartner,
    updateSelectInput(
      session,
      "detailYearPartner",
      "Select Year",
      choices = unique(data_partner$Year[data_partner$Partner.Name %in% input$detailStatePartner])
    )
  )
  
  detailTBPartner <- eventReactive(input$detailYearPartner, {
    data_partner %>%
      filter(
        Partner.Name %in% input$detailStatePartner &
          Year %in% input$detailYearPartner
      ) %>%
      select(c(
        "Year",
        "Partner.Name",
        "Export..US..Thousand.",
        "Export.Product.Share...."
      ))
    
  })
  
  output$detailTablePartner <- renderTable({
    input$detailFilterPartner
    isolate({
      detailTBPartner() %>%
        data.frame()
      
    })
  })
  
  # median us export plot:
  
  output$detailPlotPartner <- renderPlotly({
    input$detailFilterPartner
    
    isolate({
      p <- ggplot(detailTBPartner(), aes(x = Year, y = Export..US..Thousand.)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = Export..US..Thousand.),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = Year, y = Export..US..Thousand.),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +
        
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  })
  
  output$detailPlotemPartner <- renderPlotly({
    input$detailFilterPartner
    
    isolate({
      p <- ggplot(detailTBPartner(), aes(x = Year, y = Export.Product.Share....)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = Export.Product.Share....),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = Year, y = Export.Product.Share....),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +
        
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  })
  
  ### DATA FUEL ###
  
  observe({
    updateSelectInput(session, "detailYearFuel", choices = unique(data_fuel$INVYR[data_fuel$STATE %in% input$detailStateFuel]))
  })
  
  observeEvent(
    input$detailStateFuel,
    updateSelectInput(
      session,
      "detailStateFuel",
      "Select State",
      choices = unique(data_fuel$STATE),
      selected = input$detailStateFuel
    )
  )
  
  observeEvent(
    input$detailStateFuel,
    updateSelectInput(
      session,
      "detailYearFuel",
      "Select Year",
      choices = unique(data_fuel$INVYR[data_fuel$STATE %in% input$detailStateFuel])
    )
  )
  
  detailTBFuel <- eventReactive(input$detailYearFuel, {
    data_fuel %>%
      filter(
        STATE %in% input$detailStateFuel &
          INVYR %in% input$detailYearFuel
      ) %>%
      select(c(
        "INVYR",
        "STATE",
        "LVHRBHT",
        "LVHRBCD"
      ))
    
  })
  
  output$detailTableFuel <- renderTable({
    input$detailFilterFuel
    isolate({
      detailTBFuel() %>%
        data.frame()
      
    })
  })
  
  output$detailPlotFuel <- renderPlotly({
    input$detailFilterFuel
    
    isolate({
      p <- ggplot(detailTBFuel(), aes(x = INVYR, y = LVHRBHT)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = LVHRBHT),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = INVYR, y = LVHRBHT),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +
        
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  })
  
  output$detailPlotemFuel <- renderPlotly({
    input$detailFilterFuel 
    
    isolate({
      p <- ggplot(detailTBFuel(), aes(x = INVYR, y = LVHRBCD)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = LVHRBCD),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = INVYR, y = LVHRBCD),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +
        
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  })
}
