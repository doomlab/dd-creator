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
      fileInput("inFile", "TXT/CSV/XLS(X)/SAV/SAS Data File", 
                multiple = FALSE, width = NULL,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv',
                  '.xls',
                  '.xlsx',
                  '.sav',
                  '.sas'
                ), 
                buttonLabel = "Browse...", 
                placeholder = "No file selected"
      ),
      checkboxInput("header", "Data file has a header.", TRUE),
      DTOutput("rawdata_table")
    )
  )
)