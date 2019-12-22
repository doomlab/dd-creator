project_tab <- tabItem(tabName = "project_tab",
  fluidRow(
    box(
      title = tags$b("Instructions"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      p("Now that you have each variable described, let's describe the data collection, 
        the authors, and how to cite the data.")), #close box

# bib information box -------------------------------------------------
    box(
      title = tags$b("Bibliography Information"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      textInput("project_title", "Project Title"),
      textAreaInput("project_description", "Project Description"),
      textAreaInput("project_hosting", "Data Location (URL)"),
      textInput("datePublished", "Date Published (YYYY-MM-DD)"),
      textInput("citation", "Dataset Citation"),
      textInput("keywords", "Keywords"),
      textInput("license", "Use License"),
      textInput("funder", "Funders"),
      textInput("geographicDescription", "Geographic Description - City/State/Country of Participants"),
      textInput("startDate", "Project Start Date"),
      textInput("endDate", "Project End Date")
    ), #close box

# creators information ----------------------------------------------------

    box(
      title = tags$b("Author Information"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      uiOutput("data"), 
      actionButton("addData", "Add Data"),
      DTOutput("creators_input")
    ) #close box
  ) #close row
) #close tab
