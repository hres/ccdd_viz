

shinyUI(fluidPage(theme=shinytheme('spacelab'),

    # Application title
    titlePanel("CCDD Visualization"),

    
    fluidRow(
        column(2,
            br(),
            selectInput('select_tm',label=div(style="font-size: 20px",'Select a TM concept'),tm_list),
            tableOutput('current_tm'),
            br(),
            br(),
            br(),
            br(),
            actionLink("remove", "Remove detail tabs")
        ),

        
        column(10,
            tabsetPanel(id='tabs',
               tabPanel(title='Network Graph', 
            br(),
            HTML('<b> Scroll your mouse to zoom in or out <br>
                      Drag nodes to rearrange the plot <br>
                      Click on orange nodes to look at concept details </b>'), 
            
            forceNetworkOutput("networkplot",height="700px")
               )
            )
        )
    )
))
