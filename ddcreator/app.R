## Data Dictionary Creator
## SIPS Hackathon 2018: Lisa DeBruine, Alicia Moher, and Erin M. Buchanan
## Updates by Erin M. Buchanan and DOOM Lab Team: Sarah Crain, Arielle Cunningham, Hannah Johnson, Hannah Stash

## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(haven)
library(readr)

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
  dashboardHeader(title = tags$b("DD Creator")),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tags$b("1. Project Info"), tabName = "project_tab"),
      menuItem(tags$b("2. Upload Data"), tabName = "upload_tab"),
      menuItem(tags$b("3. Variables"), tabName = "variables_tab"),
      menuItem(tags$b("4. Category Labels"), tabName = "labels_tab"),
      menuItem(tags$b("5. Output"), tabName = "output_tab")
    )
  ),
  dashboardBody(
    tabItems(
      project_tab,
      upload_tab,
      variables_tab,
      labels_tab,
      output_tab
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

## server ----
server <- function(input, output, session) { 
  ## Load data ----
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    file_extension <- tools::file_ext(inFile$datapath)
    if (file_extension == "csv") {
      rawdata <<- read.csv(inFile$datapath, header = input$header,
                           stringsAsFactors = F)
    } else if (file_extension %in% c("xls", "xlsx")) {
      rawdata <<- as.data.frame(readxl::read_excel(inFile$datapath,
                                                   col_names = input$header))
    } else if (file_extension %in% c("sav")) {
      rawdata <<- as.data.frame(haven::read_sav(inFile$datapath))
    } else if (file_extension %in% c("sas")) {
      rawdata <<- as.data.frame(haven::read_sas(inFile$datapath))
    } else if (file_extension %in% c("txt")) {
      rawdata <<- as.data.frame(read.delim(inFile$datapath,
                                           header = input$header))
    }
    
    #save file name as global variable for writing
    file_name <<- gsub(paste0("." , file_extension), "", inFile$name)
    
    # populate level attributes
    column_names <- names(rawdata)
    attribute_storage <<- sapply(column_names, function(x) NULL)
    
    for (theCol in column_names) {
      vals <- sort(unique(rawdata[,theCol]))
      desc <- as.character(vals)
      # TODO: get value + description labels from SPSS attributes if they exist
      
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
    
    updateSelectInput(session, "level_col_select", 
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
    
    min_vals <- apply(rawdata, 2, min, na.rm = T)
    max_vals <- apply(rawdata, 2, max, na.rm = T)
    
    var_data <<- data.frame(
      # not editable
      variable = column_names,
      unique_values = unique_val_n,
      missing_values = missing_val_n,
      levels = unique_vals,
      
      # editable - required
      # TODO: get from SPSS description column
      description = "Enter Here",
      
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
  
  # ## copy_var_to_desc_button
  # observeEvent(input$copy_var_to_desc_button, {
  #   var_data[, 'description'] <<- var_data[, 'variable']
  #   DT::replaceData(vars_proxy, var_data, resetPaging = F)
  # })
  
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
      
      #var_list <- split(var_data, seq(nrow(var_data)))
      
      #take metadata data and create JSON
      list(type = "Dataset",
        name = file_name,
        alternateName = input$project_name,
        description = input$project_description,
        datePublished = Sys.Date(),
        creator = input$project_author,
        variableMeasured = var_data,
        disambiguatingDescription = attribute_storage) %>% 
        toJSON() %>%
        writeLines(file)
    }
  )
  
} # end server()

shinyApp(ui, server)