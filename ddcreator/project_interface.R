project_tab <- tabItem(tabName = "project_tab",
  fluidRow(
    box(
      title = tags$b("Project Information"),
      collapsible = TRUE,
      solidHeader = TRUE,
      width = 12,
      p(
        "Welcome to the Data Dictionary Creator. You can use this Shiny app to create a metadata 
        file that helps other researchers understand your dataset for future use. On this page, 
        please include the project information and description. We do not save any information 
        and delete your data from the temp directory regularly. Comments and suggestions for 
        improvements are welcome!"
      ),
      textInput("project_name", "Project Name"),
      textInput("project_author", "Project Authors"),
      textAreaInput("project_description", "Project Description")
    )
  )
)