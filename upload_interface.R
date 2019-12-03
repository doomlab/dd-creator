upload_tab <- tabItem(tabName = "upload_tab",
  fluidRow(
    box(
      title = tags$b("Upload Data"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      p("On this page, you can upload the data file to begin making your data dictionary. 
        Currently, this app supports text files that are comma separated, Excel files,
        SPSS data files, and SAS data files. After you upload your file, you can begin 
        editing the descriptions on the next pages \"3. Variables\" and \"4. Category Labels\" 
        in the left hand menu. The data file you uploaded will be displayed below, so you can verify 
        it was read properly."),
      fileInput("inFile", "Nearly all data formats supported:", 
                multiple = FALSE, width = NULL,
                buttonLabel = "Browse...", 
                placeholder = "No file selected"
      ),
      checkboxInput("qualtrics", "Data file is Qualtrics formatted .csv.", FALSE),
      checkboxInput("qualtrics_legacy", "Data file is Legacy Qualtrics formatted .csv.", FALSE),
      DTOutput("rawdata_table")
    )
  )
)