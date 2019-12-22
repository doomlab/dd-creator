upload_tab <- tabItem(tabName = "upload_tab",

  # instructions -------------------------------------------------------------                    
  
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
      p("First, let's make sure your data can be read in correctly. Then, you can follow the steps in 
        order as they appear on the side panel. At the end of the steps, you will have a machine 
        readable file for your data to be indexed in dataset searches, and a human readable HTML 
        report for you to share with others for maximum reach to all audiences."),
      br(),
      p("Please note that we do not save your data, and the temporary storage of the 
        dataset is cleaned out regularly. Comments and suggestions for improvements are welcome!")
      
    ), #close box

    # upload data -------------------------------------------------------------
    
    box(
      title = tags$b("Upload Data"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      p("On this page, you can upload the data file to begin making your data dictionary. 
        Currently, this app supports text files that are comma separated, Excel files,
        SPSS data files, and SAS data files. After you upload your file, you can begin 
        editing the descriptions on the next pages \"Variables\" and \"Category Labels\" 
        in the left hand menu. The data file you uploaded will be displayed below, so you can verify 
        it was read properly."),
      p(),
      p("First, select if the file is a Qualtrics specific format. Newer versions include two 
        extra lines of metadata that can be imported. Older \"legacy\" versions include only 
        one extra line of data."),
      checkboxInput("qualtrics", "Data file is Qualtrics formatted .csv.", FALSE),
      checkboxInput("qualtrics_legacy", "Data file is Legacy Qualtrics formatted .csv.", FALSE),
      p(),
      p("Now, upload the file that you wish to create metadata for."),
      fileInput("inFile", "Nearly all data formats supported:", 
                multiple = FALSE, width = NULL,
                buttonLabel = "Browse...", 
                placeholder = "No file selected"
      ),
      DTOutput("rawdata_table")
    )
  )
)