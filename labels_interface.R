labels_tab <- tabItem(tabName = "labels_tab",
  fluidRow(
    box(
      title = tags$b("Instructions"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      # TODO:add support for multiple edit
      p("On this page, you can define a description for each individual value of a variable. 
        For example, if you use a Likert-type scale, you may wish to define the labels for the points 
        used on that scale. Additionally, you can define any abbreviations used for category labels. 
        The description column will default to the unique values, and you can double click to 
        edit those values. Do not edit the values column, so the output matches the original data.")
    ), #close box
    
    box(
      title = tags$b("Edit Category Labels"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      selectInput("level_col_select",
                  label = "Columns",
                  choices = c(),
                  multiple = FALSE), 
      DTOutput("level_col_table")
    ), #close box
    
    box(
      title = tags$b("Multiple Category Labels"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      p("In this section, you can copy column labels for any variables that have 
        the same information. Be sure to only select columns with the same labels!"),
      selectInput("level_col_copy_from",
                  label = "Columns to copy from:",
                  choices = c(),
                  multiple = FALSE), 
      selectInput("level_col_copy_to",
                  label = "Columns to copy to:",
                  choices = c(),
                  multiple = TRUE),
      actionButton("copy_columns", "Copy Column Values"),
      verbatimTextOutput("copied_columns")
    ) #close box
  ) #fluidrow
) #close page 