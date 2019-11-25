project_tab <- tabItem(tabName = "project_tab",
  fluidRow(
    box(
      title = tags$b("Instructions"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      p(
        "Welcome to the Data Dictionary Creator. This app will help you walk through the steps of 
        creating files that help other researchers understand your dataset for future use. There 
        are several sections to complete, and you can close each box for better viewing by 
        clicking on the minus (-) sign in the top right corner of the box."),
      br(),
      p("First, you can fill in the project information and description on this page. Next, click on page 2. Upload 
        Data to input your data and start labeling variables. You can follow the steps in 
        order as they appear on the side panel. At the end of the steps, you will have a machine 
        readable file for your data to be indexed in dataset searches, and a human readable HTML 
        report for you to share with others for maximum reach to all audiences."),
      br(),
      p("Please note that we do not save your data, and the temporary storage of the 
        dataset is cleaned out regularly. Comments and suggestions for improvements are welcome!"
      )), #close box

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
