# Tab layout and sidebar panels for a shiny app that helps user
#See the biogeochemical impact of mountaintop mining to accompnay
#Ross et al., 2016


library(leaflet)
library(shiny)
library(dygraphs)


shinyUI(fluidPage(sidebarLayout(
  sidebarPanel(
    p(
      "Mountaintop Mining has dramatically changed they physical and ecological landscape
      of Central Appalachia. Mining activities flatten the landscape, fill valleys with
      shattered bedrock and coal residues, and turn 2 meter deep soils into 100 meter deep spoils. Here
      we show how these physical impacts to the landscape alter the hydrology and biogeochemistry
      of two watersheds that have been heavily mined, with an emphasis on changes to chemical weathering.
      The study follows a paired watershed approach, where we have two reference watershed that are unmined paired with two
      watersheds that have been mined. The small catchments (~1km2) are Rich's Branch (RB, reference) and
      Laurel Branch (LB, 99% mined), while the large ones (35 km2) are Left Fork (LF, reference) and Mud River (MR, 46% mined).
      To interact with the app click on a catchment and then select tabs. "
    ),
    leafletOutput("MapMine", height = 350),
    p(
      em(
        "This application was built by Matt Ross with support from NSF EAR, NSF GRFP"
      )
    ),
    width = 4
    ),
  mainPanel(width = 8,
            tabsetPanel(
              #Bit of code to containerize the gif plot
              
              tabPanel(
                "Geomorphology",
                textOutput('Sum.Text'),
                tags$head(
                  tags$style(type = "text/css",
                             "#geogif img {max-width: 100%; width: 100%; height: auto}")
                ),
                imageOutput('geogif')
              ),
              tabPanel(
                "Hydrologic Flux",
                br(),
                fluidRow(column(
                  6,
                  selectInput(
                    'comp',
                    label = 'Compare Selected Catchment With:',
                    choices = list(
                      'Mined vs Unmined' = 1,
                      '1st vs 4th Order streams' = 2,
                      'Single Watershed' = 3
                    ),
                    selected = 3
                  )
                )),
                #   column(6,
                #          selectInput('comp2',label='Compare Data for:',
                #                      choices=list('1st Order Catchments'=1,
                #                                   '4th Order Catchments'=2)))
                #)
                br(),
                dygraphOutput('pplots', width = '95%', height =
                                '100px'),
                br(),
                dygraphOutput('qplots', width = '95%', height =
                                '200px'),
                br(),
                plotOutput('cume.plot', width = '95%', height =
                             '300px')
              ),
              tabPanel(
                "Baseflow",
                br(),
                fluidRow(column(
                  12,
                  selectInput(
                    'base',
                    label = 'Choose baseflow data display',
                    choices = list('Compare baseflow between sites' = 1,
                                   'Look at baseflow for each site'=2),
                    selected=2
                  )
                )),
                br(),
                conditionalPanel(
                  condition = "input.base == 1",
                dygraphOutput('p.base', width = '95%', height = '100px'),
                dygraphOutput('q1.base', width = '95%', height = '200px'),
                dygraphOutput('q4.base', width = '95%', height = '200px')
                ),
                conditionalPanel(
                  condition="input.base==2",
                  dygraphOutput('b1', width = '95%', height = '125px'),
                  dygraphOutput('b2', width = '95%', height = '125px'),
                  dygraphOutput('b3', width = '95%', height = '125px'),
                  dygraphOutput('b4', width = '95%', height = '125px')
                )
              )
            ))
  )))
