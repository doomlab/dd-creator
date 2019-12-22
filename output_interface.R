output_tab <- tabItem(tabName = "output_tab",
  fluidRow(
    box(
      title = tags$b("Output"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      p("On this page, you can download several versions of the information you created on 
        \"Variables\" and \"Category Labels\"."),
      p("The Variable Metadata is a CSV file that contains the metadata 
        information you entered on step \"Variables\"."),
      downloadButton("output_csv", "Download Variable Metadata"),
      p(),
      p("The Category Labels file is a CSV file that contains the metadata information 
        for category labels you entered in step \"Category Labels\"."),
      downloadButton("output_attributes", "Download Category Labels"),      
      p(),
      p("The RData format (.Rdata) is a format designed for use with R, that combines the 
        information you entered in each step into one file with the data and attributes put 
        together."),
      downloadButton("output_rdata", "Download Rdata file"),
      p(),
      p("The JSON format is a machine readable format that combines the variable and 
        category label metadata into one structured file."),
      downloadButton("output_JSON", "Download JSON file"),
      p(),
      p("The HTML format is created by running the codebook package. 
      Please cite codebook as: Arslan, R. C. (in press). How to automatically 
        document data with the codebook package to facilitate data re-use. Advances 
        in Methods and Practices in Psychological Science. doi:10.1177/2515245919838783."),
      p("Coming soon! Required codebook updates are pending for this functionality.")
    )
  )
)

