## Data Dictionary Creator
## SIPS Hackathon 2018: Lisa DeBruine, Alicia Moher, and Erin M. Buchanan
## Rewritten by Erin M. Buchanan for revise and resubmit paper
## Updates by Erin M. Buchanan and DOOM Lab Team: Sarah Crain, Arielle Cunningham, Hannah Johnson, Hannah Stash

## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(haven)
library(readr)
library(tibble)
library(rio)
#library(codebook)
#library(purrr)

## interface files
source("project_interface.R")
source("help_interface.R")
source("variables_interface.R")
source("labels_interface.R")
source("output_interface.R")
source("upload_interface.R")

## Global Variables ----
rawdata <- NULL
var_data <- NULL 
level_col_data <- NULL
attribute_storage <- list()

## UI ----
ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = "Data Dictionary Creator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tags$b("1. Project Info"), tabName = "project_tab"),
      menuItem(tags$b("2. Upload Data"), tabName = "upload_tab"),
      menuItem(tags$b("3. Variables"), tabName = "variables_tab"),
      menuItem(tags$b("4. Category Labels"), tabName = "labels_tab"),
      menuItem(tags$b("5. Output"), tabName = "output_tab"),
      menuItem(tags$b("6. About"), tabName = "help_tab")
    )
  ),
  dashboardBody(
    
    ## add a custom css file
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-weight: bold;
        font-size: 16px;
      }
      .box.box-solid.box-primary>.box-header {
        color:#fff;
        background:#666666
      }
      .box.box-solid.box-primary {
        border-bottom-color:#666666;
        border-left-color:#666666;
        border-right-color:#666666;
        border-top-color:#666666;
      }'))),
    
    ## show the tab items
    tabItems(
      project_tab,
      upload_tab,
      variables_tab,
      labels_tab,
      output_tab,
      help_tab
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

## server ----
server <- function(input, output, session) { 
  ## Creators table ----
  # store 'empty' tibble
  creators_table <<-
    tibble(
      id = character(),
      givenName = character(),
      familyName = character(),
      affiliation = character(),
      email = character()
    ) 
  creators_table <<- as.data.frame(creators_table)
  creators_table <<- add_row(creators_table,
                            id = "your ORC-ID",
                            givenName = "First/Given Name",
                            familyName = "Last/Family Name",
                            affiliation = "Affiliation", 
                            email = "Email")
  
  # display creators table view
  output$creators_input <- 
    renderDT(creators_table, 
             server = FALSE, 
             editable = TRUE, 
             options = list(lengthChange = TRUE), 
             rownames = FALSE, 
             colnames = c("ORC-ID", "Given Name", 
                          "Family Name", "Affiliation", "Email"))
  
  # store a proxy of creators_table
  creators_proxy <- dataTableProxy(outputId = "creators_input", session)
  
  # each time addData is pressed, add creators_table to proxy
  observeEvent(eventExpr = input$addData, {
    creators_proxy %>% 
      addRow(creators_table[1, ])
  })
  
  ## proxy saving creators data ----
  observeEvent(input$creators_input_cell_edit,  {
    info = input$creators_input_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    creators_table[i,j+1] <<- isolate(DT::coerceValue(v, creators_table[i,j+1]))
    #replaceData(vars_proxy, var_data, resetPaging = F)
  })
  
  ## Load data ----
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    file_extension <<- tools::file_ext(inFile$datapath)
    
    if (input$qualtrics){
      headers <- read.csv(inFile$datapath, nrows = 1, header = F, as.is = T)
      metastuff <- read.csv(inFile$datapath, nrows = 1, 
                            header = F, as.is = T,
                            skip = 1)
      rawdata <<- as.data.frame(read.csv(inFile$datapath, skip = 3, header = F))
      colnames(rawdata) <<- headers
      names(rawdata) <<- headers
      attr(rawdata, "label") <<- metastuff
      
    } else if (input$qualtrics_legacy){
      headers <- read.csv(inFile$datapath, nrows = 1, header = F, as.is = T)
      metastuff <- read.csv(inFile$datapath, nrows = 1, 
                            header = F, as.is = T,
                            skip = 1)
      rawdata <<- as.data.frame(read.csv(inFile$datapath, skip = 2, header = F))
      colnames(rawdata) <<- headers
      names(rawdata) <<- headers
      attr(rawdata, "label") <<- metastuff
      
    } else { 
      #all others with rio 
      rawdata <<- as.data.frame(import(inFile$datapath))
    }
    
    #save file name as global variable for writing
    file_name <<- gsub(paste0("." , file_extension), "", inFile$name)
    
    # populate level attributes
    column_names <- names(rawdata)
    attribute_storage <<- sapply(column_names, function(x) NULL)
    
    for (theCol in column_names) {

      ## deal with labelSSS for variable labels
      labels_exist <- names(attributes(rawdata[ , theCol])$labels)
      
      #if labels exist
      if (!is.null(labels_exist)){
        vals <- attributes(rawdata[ , theCol])$labels
        desc <- as.character(labels_exist)
      } else {
        vals <- sort(unique(rawdata[,theCol]))
        desc <- as.character(vals)
      }
      
      attribute_storage[[theCol]] <<- data.frame(
        "values" = vals,
        "description" = desc,
        stringsAsFactors = F
      )
    }
  })
  
  ## output$rawdata_table ----
  output$rawdata_table <- renderDataTable({
    dat()
    datatable(rawdata, rownames = F,
              options = list(scrollX = T))
  })
  
  ## output$vars_table ----
  output$vars_table <- renderDataTable({
    column_names <- names(rawdata)
    
    ## original column names for the edit
    updateSelectInput(session, "level_col_select", 
                      label = NULL, choices = column_names,
                      selected = NULL)
    
    ## original column names for copy from
    updateSelectInput(session, "level_col_copy_from", 
                      label = NULL, choices = column_names,
                      selected = NULL)
    
    ## original column names for copy to
    updateSelectInput(session, "level_col_copy_to", 
                      label = NULL, choices = column_names,
                      selected = NULL)
    
    types <- sapply(rawdata, class)
    
    unique_val_n <- apply(rawdata, 2, function(x) { 
      length(unique(x)) 
    })
    
    missing_val_n <- apply(rawdata, 2, function(x) { 
      length(which(is.na(x))) 
    })
    
    unique_vals <- apply(rawdata, 2, function(x) {
      uv <- unique(x)
      if (length(uv) < 11) {
        paste(sort(uv), collapse = ", ")
      } else {
        "NA"
      }
    })
    
    min_vals <- apply(rawdata, 2, function(x){
      if (length(na.omit(x)) > 0){
        min(x, na.rm = T)
      } else {"No Data Present"}
    })
    max_vals <- apply(rawdata, 2, function(x){
      if (length(na.omit(x)) > 0){
        max(x, na.rm = T)
      } else {"No Data Present"}
    })
    
    #the attribute label (no S)
    description_entered <- rep("Enter Here", times = ncol(rawdata))
    for (i in 1:length(description_entered)){
      temp_label <- attributes(rawdata[ , i])$label
      if (!is.null(temp_label)){
        description_entered[i] <- temp_label
      }
    }
      
    var_data <<- data.frame(
      # not editable
      variable = column_names,
      unique_values = unique_val_n,
      missing_values = missing_val_n,
      levels = unique_vals,
      
      # editable - required
      description = description_entered,
      
      # editable - optional
      type = types,
      min = min_vals,
      max = max_vals,
      na = TRUE,
      na_values = 'NA',
      synonyms = 'NA',
      
      # arguments
      stringsAsFactors = F
    )
    
    datatable(var_data, editable = TRUE, rownames = F, 
              options = list(scrollX = T),
              colnames = c(
                'Variable',
                '# Unique Values',
                '# Missing Values',
                'Levels',
                'Description (required)',
                'Type',
                'Minimum Allowable',
                'Maximum\nAllowable',
                'Missing Allowed',
                'Missing Values =',
                'Synonyms'
              ))
  })
  
  vars_proxy = dataTableProxy('vars_table', session)
  
  ## proxy saving variable data ----
  observeEvent(input$vars_table_cell_edit,  {
    info = input$vars_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    var_data[i,j+1] <<- isolate(DT::coerceValue(v, var_data[i,j+1]))
    #replaceData(vars_proxy, var_data, resetPaging = F)
  })
  
  ## output$level_col_table ---- 
  output$level_col_table <- renderDataTable({
    theCol <- input$level_col_select
    
    level_col_data <<- attribute_storage[[theCol]]
    
    datatable(level_col_data, editable = TRUE, rownames = F,
              options = list(scrollX = T))
  })
  
  ## proxy saving level column data ----
  level_col_proxy = dataTableProxy('level_col_table', session)
  observeEvent(input$level_col_table_cell_edit,  {
    info = input$level_col_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    level_col_data[i,j+1] <<- isolate(DT::coerceValue(v, level_col_data[i,j+1]))
    #replaceData(level_col_proxy, level_col_data, resetPaging = F)
    
    # save level column data to temp storage, eventually to attributes
    attribute_storage[[input$level_col_select]] <<- level_col_data
  })
  
  ## action button copy ----
  copy_over <- eventReactive(input$copy_columns,{
    
    for (i in 1:length(input$level_col_copy_to)){
      attribute_storage[[input$level_col_copy_to[i]]] <<- attribute_storage[[input$level_col_copy_from]]
    }
    
    "Columns have been copied."
    
  })
  
  output$copied_columns <- renderText({
    copy_over()
  })

  ## output$output_csv ----
  output$output_csv <- downloadHandler(
    filename = paste0(file_name, "_metadata_", gsub("-", "", Sys.Date()), ".csv"),
    content = function(file) {
      write.csv(var_data, file, row.names = F, quote = TRUE)
    }
  )
  
  ## output$output_attributes_csv ----
  output$output_attributes <- downloadHandler(
    filename = paste0(file_name, "_categorylabels_", gsub("-", "", Sys.Date()), ".csv"),
    content = function(file) {
      temp <- do.call("rbind", attribute_storage)
      colnames(temp) = c("values", "description") 
      write.csv(temp[ , c(1,2)], file, row.names = T, quote = TRUE)
    }
  )
  
  ## output$output_Rdata & set attributes ----
  output$output_rdata <- downloadHandler(
    filename= paste0(file_name, "_combinedRdata_", gsub("-", "", Sys.Date()), ".Rdata"),
    content = function(file) {
      
      #convert missing descriptions to blank
      var_data[is.na(var_data)] <- ""
      
      #variable & value labels
      for (i in 1:ncol(rawdata)){
        attr(rawdata[,i], "label") <- var_data$Description[i]
        
        #set up value labels 
        #TO DO: Get this working; unsure how data is being set
        temp <- as.character(attribute_storage[[i]][,1])
        names(temp) <- attribute_storage[[i]][,2]
        attr(rawdata[,i], "labels") <- temp
      } #close the for loop
      save(rawdata, file=file)
    } #close content
  )
  
  ## output$output_JSON & set attributes ----
  output$output_JSON <- downloadHandler(
    filename= paste0(file_name, "_JSON_", gsub("-", "", Sys.Date()), ".JSON"),
    content = function(file) {
      
      ## only use attributes that have been entered
      attribute_levels <<- list()
      
      for (i in 1:length(attribute_storage)){
        ## check if they are all equal 
        if (!identical(as.character(attribute_storage[[i]]$values), 
                      as.character(attribute_storage[[i]]$description))){
          ## keep only mismatch
          attribute_levels[[i]] <<- attribute_storage[[i]][!match(as.character(attribute_storage[[i]]$values), 
                                                                 as.character(attribute_storage[[i]]$description), 
                                                                 nomatch = FALSE), ]
          ## give it a name
          names(attribute_levels)[i] <<- names(attribute_storage)[i]
          names(attribute_levels[[i]])[1] <<- "codeValue"
        }
      }
      
      ## create author list
      authors <<- list()
      for (i in 1:nrow(creators_table)){
        authors[[i]] <<- list(
          `@type` = "Person",
          identifier = creators_table$id[i],
          givenName = creators_table$givenName[i],
          familyName = creators_table$familyName[i],
          email = creators_table$email[i],
          affiliation = creators_table$affiliation[i]
        )
      }
      
      ## create variable information
      var_data_json <<- list()
      
      if (exists("var_data")){
      
      for (i in 1:nrow(var_data)){ 
        var_data_json[[i]] <<- list(
          `@type` = "PropertyValue",
          identifier = var_data$variable[i], # 
          unitText = var_data$type[i], #
          minValue = var_data$min[i], #
          maxValue = var_data$max[i], #
          disambiguatingDescription = list(`@type` = "Text", 
                                           uniqueValues = var_data$unique_values[i],
                                           missingValues = var_data$missing_values[i],
                                           missingValuesAllowed = var_data$na[i],
                                           missingValuesValues = var_data$na_values[i],
                                           levels = var_data$levels[i],
                                           valueLabels = list(
                                             `@type` = "CategoryCode",
                                             attribute_levels[[var_data$variable[i]]])), #
          description = var_data$description[i], #
          alternateName = var_data$synonyms[i])
        } ## close variable writing
      } ## close if it exists 
      
      #take metadata data and create JSON
      list(
        `@context` = "https://schema.org/",
        `@type` = "Dataset",
        name = if (exists("input$project_title")) { input$project_title } else {"NA"}, #title 
        fileFormat = if (exists("file_extension")) { file_extension } else {"NA"},
        fileName = if (exists("file_name")) { file_name } else {"NA"},
        description = if (exists("input$project_description")) { input$project_description } else {"NA"},
        contentUrl = if (exists("input$project_hosting")) { input$project_hosting } else {"NA"},
        datePublished = if (exists("input$datePublished")) { input$datePublished } else {"NA"},
        citation = if (exists("input$citation")) { input$citation } else {"NA"},
        keywords = if (exists("input$keywords")) { input$keywords } else {"NA"},
        license = if (exists("input$license")) { input$license } else {"NA"},
        funder = if (exists("input$funder")) { input$funder } else {"NA"},
        temporalCoverage = if (exists("input$startDate") & exists("input$endDate")) {
          paste(input$startDate, input$endDate, sep="/")
        } else if (exists("input$startDate")) { input$startDate 
          } else if (exists("input$endDate")) { input$endDate 
          } else { "NA" },
        spatialCoverage = list(
          `@type` = "Place",
          name = if (exists("input$geographicDescription")){ input$geographicDescription 
            } else {"NA"}),
        #do this map 
        author = authors,
        distribution = list(
          `@type` = "DataDownload",
          name = if (exists("input$project_title")) { input$project_title } else {"NA"},
          contentUrl = if (exists("input$project_hosting")){ input$project_hosting } else { "NA" },
          fileFormat = if (exists(file_extension)) { file_extension } else { "NA" }),
        variableMeasured = var_data_json) %>% 
        toJSON() %>%
        writeLines(file)
    }
  )
  
} # end server()

shinyApp(ui, server)