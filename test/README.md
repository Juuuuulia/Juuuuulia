
## Tittle and Author

Lumber market analisys in shiny app
Author: Julia Zhang

-------

## Description

The shiny app tool developed in this project will provide Shanda Group with a user-friendly and dynamic analysis of the lumber market. The tool's data visualization methods will enable the client to better understand the dynamics of the lumber market and make informed decisions related to their investment strategies.

The shiny app tool developed in this project using advanced analytics techniques, such as random forest, linear regression, multiple regression with dummy variables, box plots, density plots, and ggplot, is of great importance to Shanda Group in the timber industry. The tool's data visualization methods will enable the client to better understand the dynamics of the lumber market, making it easier to interpret the data and identify trends and patterns that may not be immediately apparent.

-------

## Data source

The CBP.CB1400CBP-2023-03-20T092427.csv dataset was collected from the US Census Bureau to examine the organization and employees in the timber industry from 2014 to 2020, as recorded in government reports. 

The WITS-Partner.csv dataset was generated by the World Integrated Trade Solution (WITS) to document the countries importing wood from the US, including the wood product shares of total imported numbers. 

Both the DWM_MICROPLOT_FUEL.csv and WM_COARSE_WOODY_DEBRIS.csv datasets were downloaded from the USDA Forest Inventory and Analysis DataMart. 

More information about the fuel table and coarse table can be found in the document named FIADB User Guide P2_9-0-1_final.pdf. 

-------

## Packages used

To use libraries in project, users need to download and install the following packages:

reshape2, reshape, purrr, RColorBrewer, viridis, tidyverse, shinyWidgets, shiny, plotly
shinythemes, DT, caret, ggplot2, dplyr, tidyr, knitr, kableExtra, ggthemes, GGally, randomForest

Users can install these packages in R by running the following command:

install.packages(c("reshape2", "reshape", "purrr", "RColorBrewer", "viridis", "tidyverse", "shinyWidgets", "shiny", "plotly", "shinythemes", "DT", "caret", "ggplot2", "dplyr", "tidyr", "knitr", "kableExtra", "ggthemes", "GGally", "randomForest"))

-------

## Manual instructions

This project includes a combination of R server and UI codes for a Shiny application. One of modules in the first page in the Shiny application allows users to view and filter data on type of organizations, with a focus on the number of establishments, employees, and payroll information. The server code consists of three main sections: the first section generates a table of summary data that shows the average size of establishments and average paid employees for a selected year, grouped by type of organization. The table is formatted using Bootstrap table styles; the second section generates a pie chart that shows the distribution of organizations by number of establishments for a selected year. The chart is formatted using ggplot2 and polar coordinates, and colors are specified using a pre-defined color map; the third section filters the data based on user inputs, including a range for annual payroll and a range for paid employees. The filtered data is then displayed in a data table. The UI code consists of two sidebar layouts: the first sidebar layout contains a dropdown menu for selecting a year, and tabs for displaying the ranking of organizations by average size and the distribution of organizations by number of establishments.; the second sidebar layout contains sliders and dropdown menus for filtering the data by year, annual payroll range, paid employee range, and a button for applying the filters. The filtered data is displayed in a data table.

While scrolling down the first page, users can view and filter data on US exports, with a focus on the products exported and the countries receiving the exports. The server code consists of three main sections: the first section converts the "Year" column of the data to numeric format; the second section generates a table of summary data that shows the average product share and US export value for a selected year, grouped by year. The table is formatted using Bootstrap table styles; the third section generates two pie charts that show the distribution of US exports by product share and by country for a selected year. The charts are formatted using ggplot2 and polar coordinates, and colors are specified using a pre-defined color map. 	The UI code consists of two sidebar layouts: the first sidebar layout contains dropdown menus for selecting a year and a country, and tabs for displaying the ranking of products by share and the distribution of US exports by country; the second sidebar layout contains sliders and dropdown menus for filtering the data by year, annual US export range, product share range, and a button for applying the filters. The filtered data is displayed in a data table.

Next, users can view datasets related to US forest fuel. The UI is split into two main sections. The first section contains a sidebar panel with two select inputs, one for selecting the year and another for selecting the state. The main panel contains a tabset panel with three tabs. The first tab displays a table with the average live herb height, the live shrub height, and year. The second tab displays a pie plot showing live herb height by year and state. The third tab displays another pie plot showing the number of live herbs by year.

The second section contains another sidebar panel for filtering the data and a main panel for displaying the data table. The sidebar panel contains two slider inputs for filtering the annual forest fuel range and live herb range. It also has a select input for choosing the year, and a filter button for applying the filters. The main panel displays a data table showing information about the forest fuel data.

The first code block defines a sidebar layout with two panels: one containing input options for data related to US forest fuel, US timber organizations, and the other containing input options for data related to US export. Each panel contains input widgets such as selectInput, checkboxGroupInput, and sliderInput. The main panel displays visualizations of the selected data. The first panel shows three tabPanels with a tableOutput and two plotOutputs, respectively. The second panel shows a dataTableOutput.

The second code block is generated in the second page named visual comparation and defines three different density plot panels: one for the primary dataset of data_fuel and the others for dataset data_partner and data_cbp. Each panel contains a sidebar panel with input options such as selectInput and checkboxGroupInput and a main panel displaying the density plot of the selected variable. The ggplot2 library is used to create the density plot, which is rendered using the plotlyOutput function. The renderPlotly function generates the plot and the ggplotly function is used to convert the ggplot object to a plotly object for rendering in the dashboard.

Finally, the code defines three different reactive data frames (dent, dent_partner, and dent_fuel) that filter the selected data based on the user inputs from the various input widgets. These data frames are used in the respective renderPlotly functions to create the density plots.

Following two main sections are about "Regression Plot Panel Partner" and "Multiple Regression with Dummy Variables".

The "Regression Plot Panel Partner" section involves generating plots and tables related to linear and logistic regression for a selected partner(s). The server function contains several reactive functions, which respond to changes in user input. dent_partner_lr is a reactive function that filters the data_partner dataset based on the selected partner(s) in checkGroup_partner_lr. The renderPlotly function generates a plot based on the selected x-axis and y-axis variables, and either a linear or logistic regression model, depending on the type of y-axis variable selected. The renderTable function generates a table of coefficients for the linear or logistic regression model, while the pValuesTable function generates a table of p-values for each selected partner.

The "Multiple Regression with Dummy Variables" section involves generating a plot and summary for a multiple linear regression model using dummy variables. The ui function contains two select input functions for the independent and dependent variables, respectively. The server function contains a reactive function df_dm that generates dummy variables for the data_partner dataset. The model_dm reactive function generates a multiple linear regression model based on the selected independent and dependent variables. The renderPlotly function generates a scatter plot with a regression line, and the renderPrint function generates a summary of the regression model. Overall, these sections of code aim to provide visualizations and insights into the relationships between variables.
While scrolling down the second tab, Shiny app performs two types of analyses: random forest classification and box plot visualization.

The server section starts by defining the number of trees for the random forest model and a color palette used for both analysis types. Then, a reactive function is defined that reads a file uploaded by the user and updates the choices for two select input controls (for categorical and independent variables) based on the uploaded data.

The next reactive function, called category, performs the random forest classification analysis. It reads in the selected data and variable choices from the user, converts the categorical variable to a factor, and checks and converts any non-numeric columns to numeric. It then fits a random forest model to the data, calculates the variable importance using the selected method, and returns a list of results that includes a ggplot object of the variable importance plot, the out-of-bag error rate, and the model summary.
The outputs of this reactive function are then displayed in the UI section, including the variable importance plot, the out-of-bag error rate, and the model summary. A download button is also included to download the variable importance plot as a PNG file.

The second part of the server section is for the box plot analysis. There are two separate sections for two different datasets, data_cbp and data_coarse. Each section has its own reactive function that creates a plotly object of a box plot with jitter. The user can select a subset of data to plot based on the size of the establishment or the state, respectively. The plot is then displayed in the UI section.

The UI section is structured using a sidebarLayout and mainPanel. The sidebar panel contains input controls for the random forest analysis and the box plot analysis, including file upload, select inputs, numeric inputs, and checkbox group inputs. The main panel displays the outputs of the analyses, including the variable importance plot, the out-of-bag error rate, the model summary, and the box plots.

The third tab panel named "Details of Organization and Establishments by Year" with three columns: the first column contains two select inputs, "Select Type of Organizations" and "Select Size of Establishments", which are used to filter the data; the second column contains a select input "Select Year" and a "Filter" action button. The user can select one or more years and filter the data accordingly, the third column displays a table of data, a plot of the annual number of establishments, and a plot of the annual payroll. These outputs are updated based on the user's selection in the previous columns. The code uses several functions from the Shiny package, such as observeEvent(), updateSelectInput(), eventReactive(), renderTable(), renderPlotly(), and plotlyOutput(). It also uses functions from the dplyr and ggplot2 packages, such as filter(), select(), aes(), geom_line(), geom_point(), geom_smooth(), theme_hc(), and ggplotly(). These codes are creating a tab panel in a shiny app with two select inputs and two plots. 

The fourth tab panel is titled "Details of US Export By State and Year".
The first select input allows the user to select a state, while the second select input allows the user to select a year. Both select inputs are dependent on each other, so the choices in the second select input change depending on the state selected in the first select input. The server code contains several reactive functions that filter the data based on the state and year selected in the select inputs. The filtered data is then used to create two plots using ggplot2 and plotly. The first plot displays the annual US export by year and state, while the second plot displays the annual export product share by year and state. Both plots are interactive and display tooltips when the user hovers over data points. The UI code contains the layout of the tab panel, including the select inputs, action button, and plots. The select inputs are formatted using the selectInput() function, while the plots are created using the plotlyOutput() and ggplotly() functions. The table is created using the tableOutput() function.

The other tab is "Details Of US Forest Fuel By State and Year”. The UI consists of a title, selectInput widgets for choosing state and year, an actionButton for filtering the data, and two plotly graphs and a table to display the filtered data. In the server code, there are several observer and reactive functions defined for each tab. The observer functions update the year selectInput options based on the chosen state, and the reactive function filters the data based on the chosen state and year. The plotly graphs are then created based on the filtered data. 

Methods of loading, cleaning, and preprocessing several datasets for use in a Shiny application are run on data-clean.R file. Here is a brief explanation of each section: library() functions are used to load several R packages into the environment that will be used later in the code; getwd() and setwd() functions are used to set the working directory to the location of the data files; read.csv() functions are used to read in CSV files for four different datasets: DATA_CBP, DATA_PARTNER, DATA_FUEL, and DATA_COARSE; DATA_CBP is cleaned by replacing instances of the string "na" with NA and removing all NA values from the dataset using drop_na(). A custom function replaceCommas() is defined and applied to several columns to remove commas from numbers. DATA_PARTNER is cleaned by replacing instances of the string "na" with NA and removing all NA values from the dataset using drop_na(). Two columns, Partner.Name and Year, are converted to factors. DATA_FUEL is cleaned by replacing instances of the string "na" with NA and removing all NA values from the dataset using drop_na(). Several columns are converted to factors or numerics. data_coarse is cleaned by replacing instances of the string "na" with NA and removing all NA values from the dataset using drop_na(). Several columns are converted to factors or numerics. saveRDS() functions are used to save each cleaned dataset as an .rds file for use in the Shiny application.

At the beginning of server, the opts object is used as a list of options used to configure the appearance and behavior of the DataTables library used in the Shiny app. Specifically, it sets the language to English, sets the page length to 30 rows, enables search highlighting, and disables searching in columns 1 and 6 of the table. The ui code creates a tab panel named "Main" that includes a title panel with an image of a forest and the app's window title set to "TimberIndustryUS". The shinytheme function is used to apply the "flatly" theme to the app.

-------

## Methods

Linear regression is a widely used technique in regression analysis and is used to model the relationship between a dependent variable and one or more independent variables. Linear regression assumes that the relationship between the variables is linear, meaning that the change in the dependent variable is proportional to the change in the independent variable. The model is based on a line that represents the relationship between the variables, with the coefficients of the independent variables representing the change in the dependent variable for a one-unit change in the independent variable. The intercept of the line represents the value of the dependent variable when all independent variables are zero. Linear regression is a powerful tool that is used in many fields, including economics, finance, and social sciences, to model and predict the behavior of dependent variables based on changes in independent variables.

Multiple regression with dummy variables is another commonly used technique in regression analysis. Multiple regression with dummy variables is used to model the relationship between a dependent variable and multiple independent variables, including both continuous and categorical variables. In this technique, dummy variables are used to represent categorical variables in the regression model. Each dummy variable represents a specific category of the categorical variable, and the inclusion of these variables in the model allows for the analysis of the effect of categorical variables on the dependent variable while controlling for the effect of other independent variables. Multiple regression with dummy variables is commonly used in social sciences, particularly in studies that investigate the effect of factors such as age, gender, and education level on an outcome variable.

Both linear regression and multiple regression with dummy variables are widely used techniques in regression analysis and are valuable tools for modeling and predicting the behavior of dependent variables based on changes in independent variables. The appropriate choice of technique depends on the research question and the nature of the data being analyzed.
Random forest is an ensemble learning method that is widely used in machine learning and data analysis. The method combines multiple decision trees to produce a more accurate and stable model than any individual tree. Each decision tree in the forest is grown independently using a randomly selected subset of the data and features. This randomness helps to reduce overfitting and makes the model more robust to noise in the data.

The decision tree is a tree-like model that consists of nodes and edges. The root node represents the entire dataset, and each subsequent node represents a subset of the data that is split based on a selected feature and threshold value. The splitting process is repeated recursively until a stopping criterion is reached, such as a maximum depth of the tree or a minimum number of samples at a node. The final output of the decision tree is the prediction made by the leaf node that contains the sample.

Random forest models also provide a measure of variable importance, which indicates how much each feature contributes to the overall prediction of the model. There are two commonly used methods for calculating variable importance: impurity-based importance and permutation-based importance. Impurity-based importance measures the decrease in impurity or entropy of the decision tree when a particular feature is used for splitting. Permutation-based importance randomly permutes the values of a feature and measures the decrease in accuracy of the model when the values are permuted. Both methods provide valuable information about the relative importance of features in the model and can help identify which features are most relevant to the outcome variable.

Density plot is a flexible and versatile data visualization method that can be used to display the distribution of a continuous variable with various levels of granularity. The kernel density estimate (KDE) is a non-parametric way to estimate the probability density function (PDF) of the data. It is based on a smoothing parameter, or bandwidth, that controls the degree of smoothing of the estimated density. A narrow bandwidth will result in a density plot with many peaks, while a wider bandwidth will result in a smoother density plot.

In addition to displaying the distribution of a single continuous variable, density plots can also be used to compare the distributions of multiple groups. Group-specific density plots can be overlaid or displayed side by side to visualize the differences and similarities between the groups. This approach can be useful for identifying patterns and trends in the data that might not be apparent in a simple summary statistic such as the mean or standard deviation.

Density plots are particularly useful when the sample size is relatively small, as they can help identify the underlying distribution of the data without requiring assumptions about its shape. However, they may not be suitable for extremely large datasets, as the computational cost of kernel density estimation can become prohibitive. Density plots can also be enhanced with additional visual elements, such as confidence intervals or reference lines. These can help provide additional information and context to the plot, and aid in interpretation.

    