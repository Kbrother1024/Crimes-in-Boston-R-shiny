shinyUI(dashboardPage(
  skin = "red",
  dashboardHeader(title = strong('Boston Crime')),
  dashboardSidebar(
    #sidebar
    sidebarMenu(
      menuItem('About', tabName = 'about', icon = icon('clipboard')),
      menuItem('Map', tabName = 'map' , icon = icon('map')),
      menuItem(
        'Time Series',
        tabName = 'time',
        icon = icon('chart-line')
      ),
      menuItem(
        'Word Cloud',
        tabName = 'word',
        icon = icon('cloudversify')
      ),
      #create link tab for code
      menuItem('Link to the code',
               href = 'https://github.com/Kbrother1024/Crimes-in-Boston',
               icon = icon('code'))
    )
  ),
  
  dashboardBody(tabItems(
    #create tab about
    tabItem(
      tabName = 'about',
      fluidRow(column(
        width = 12,
        align = 'center',
        box(
          title = strong('Boston Crime 2015-06-15 To 2018-10-03'),
          status = 'danger',
          solidHeader = T,
          width = NULL,
          img(src = "https://emerj.com/wp-content/uploads/2018/04/ai-for-crime-prevention-and-detection-5-current-applications.png",
              width = 800, align = "center")
        )
      )),
      fluidRow(column(
        width = 12,
        box(
          title = strong('Context'),
          status = 'danger',
          solidHeader = T,
          width = NULL,
          h4(
            'This is a dataset containing records from the new crime incident report system, which includes a reduced set of fields focused on capturing the type of incident as well as when and where it occurred.'
          )
        )
      )),
      fluidRow(column(
        width = 12,
        box(
          title = strong('Content'),
          status = 'danger',
          solidHeader = T,
          width = NULL,
          h4('This dataset has 2,60,760 rows and 17 columns.')
        )
      )),
      fluidRow(column(
        width = 12,
        box(
          title = strong('Inspiration --- Solutions'),
          status = 'danger',
          solidHeader = T,
          width = NULL,
          h4('How has crime changed over the years ? --- See map/time series Tab'),
          h4(
            'Is it possible to predict where or when a crime will be committed ? --- Hard, need more Data'
          ),
          h4(
            'Which areas of the city have evolved over this time span ? --- See map/time series Tab, Little changes in some districts'
          ),
          h4(
            'In which area most crimes are committed ? --- See map/time series Tab'
          )
        )
      )),
      fluidRow(column(
        width = 12,
        box(
          title = strong('More Info about This Dataset'),
          status = 'danger',
          solidHeader = T,
          width = NULL,
          a(h4("Crimes in Boston - Kaggle"),
            href = 'https://www.kaggle.com/ankkur13/boston-crime-data')
        )
      ))
    ),
    #create tab map
    tabItem(tabName = 'map',
            fluidRow(column(
              width = 12,
              box(
                title = 'Crime Analysis',
                status = 'danger',
                solidHeader = T,
                infoBoxOutput("maxBox"),
                infoBoxOutput("minBox"),
                infoBoxOutput("avgBox"),
                width = NULL
              )
            )),
            fluidRow(
              column(
                width = 6,
                box(
                  title = 'Select Input',
                  status = 'danger',
                  solidHeader = T,
                  selectInput(
                    "year",
                    label = strong("Select Year to Display"),
                    choices = unique(dist$year),
                    selected = 1
                  ),
                  checkboxInput('shoot', strong('Shooting?'), value = F),
                  width = NULL
                ),
                box(
                  title = 'Crime Rate by Time',
                  status = 'danger',
                  solidHeader = T,
                  leafletOutput('mymap1'),
                  width = NULL
                )
              ),
              column(
                width = 6,
                box(
                  title = 'Select District to Inspect',
                  status = 'danger',
                  solidHeader = T,
                  width = NULL,
                  selectInput(
                    "district_map",
                    label = strong("Choose a District"),
                    choices = as.character(unique(dist$district)),
                    selected = as.character(unique(dist$district))[1]
                  ),
                  checkboxInput('inspect', strong('Inspect Which Area?'), value = F)
                ),
                box(
                  title = 'Crime Rate by Time Data Table',
                  status = 'danger',
                  solidHeader = T,
                  dataTableOutput('data_table'),
                  width = NULL
                )
              )
            )),
    #create tab time
    tabItem(tabName = 'time',
            fluidRow(
              column(
                width = 4,
                box(
                  title = 'Select Input',
                  solidHeader = T,
                  status = 'danger',
                  pickerInput(
                    "district",
                    "Choose a District",
                    choices = as.character(unique(dist$district)),
                    selected = as.character(unique(dist$district)),
                    options = list(`actions-box` = TRUE),
                    multiple = T
                  ),
                  dateRangeInput(
                    "date",
                    strong("Date range"),
                    start = min(date_trend$date),
                    end = max(date_trend$date),
                    min = min(date_trend$date),
                    max = max(date_trend$date)
                  ),
                  selectInput(
                    "SelectBy",
                    label = strong("Select Date"),
                    choices = list(
                      "By Year" = 'year',
                      "By Month" = 'month',
                      "By Week" = 'day_of_week',
                      "By Hour" = 'hour',
                      "Ratio Change by Year" = 'ratio'
                    ),
                    selected = 1
                  ),
                  checkboxInput(
                    "Byother",
                    label = strong("Select by Date"),
                    value = FALSE
                  ),
                  width = NULL
                )
              ),
              column(
                width = 8,
                box(
                  title = 'Crime Rate by Time',
                  status = 'danger',
                  solidHeader = T,
                  plotlyOutput('date_line'),
                  width = NULL
                )
              )
            ),
            fluidRow(column(
              width = 12,
              box(
                title = 'Data table',
                status = 'danger',
                solidHeader = T,
                width = NULL,
                dataTableOutput('date_table')
              )
            ))),
    #create tab word
    tabItem(tabName = 'word',
            fluidRow(column(
              width = 12,
              box(
                title = 'Select Input',
                solidHeader = T,
                status = 'danger',
                selectInput(
                  "district_word",
                  "Choose a District",
                  choices = as.character(unique(dist$district)),
                  selected = as.character(unique(dist$district))[1]
                ),
                sliderInput(
                  "max",
                  "Maximum Number of Words:",
                  min = 1,
                  max = length(unique(dist$offense_code_group)),
                  value = 10,
                  step = 1
                ),
                width = NULL
              )
            )),
            fluidRow(
              column(
                width = 6,
                box(
                  title = 'Welcome to Word Cloud',
                  solidHeader = T,
                  status = 'danger',
                  width = NULL,
                  plotOutput('word')
                )
              ),
              column(
                width = 6,
                box(
                  title = 'Numbers of Crime Type',
                  solidHeader = T,
                  status = 'danger',
                  width = NULL,
                  plotlyOutput('hist')
                )
              )
            ))
  ))
))
