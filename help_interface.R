help_tab <- tabItem(tabName = "help_tab",
  fluidRow(
    box(
      title = tags$b("About this App"),
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      p("This application was started at the Society for the Improvement of Psychological Sciences 
        (SIPS) 2018 meeting by Lisa De Bruine, Erin Buchanan, and Alicia Moher. The app was then updated 
        by the DOOM Lab (primarily Erin Buchanan) and is currently maintained by Erin Buchanan. You can 
        send questions, suggestions, and other troubleshooting issues to her at buchananlab@gmail.com."),
      p(),
      p("Special thanks to Ruben Arslan for comments and suggestions on this application. You can check out 
        our paper for this project, along with helpful YouTube tutorials at https://osf.io/3y2ex/.")) # close box
  ) #close row
) #close tab