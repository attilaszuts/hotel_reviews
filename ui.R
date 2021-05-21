library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)


shinyUI(dashboardPage(
  dashboardHeader(title = "Hotel reviews"
  ), # close Header
  dashboardSidebar(
    sidebarMenu(
      id = 'tab',
      menuItem('Welcome!', tabName = 'welcome', icon = icon('concierge-bell')),
      menuItem('Set up filters', tabName = 'filter', icon = icon('filter')),
      menuItem('Wordcloud', tabName = 'cloud', icon = icon('cloud'))
    ) # close sidebarMenu
  ), # close Sidebar
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'welcome', 
        withSpinner(uiOutput('welcome'), type = 5, color = "#222d32")
      ), # close tabItem 'welcome'
      tabItem(
        tabName = 'filter',
        withSpinner(uiOutput('hotel_chooser'), type = 5, color = "#222d32")
        ), # close tabItem 'filter'
      tabItem(
        tabName = 'cloud',
        fluidRow(
          column(3, 
                 fluidRow(column(12, h1('here comes some description'))),
                 fluidRow(column(6, uiOutput('pickerInput')))
          ),
          column(9, withSpinner(uiOutput('wordcloud'), type = 5, color = "#222d32"))
        ) # close fluidRow
      ) # close tabItem 'cloud'
    ) # close tabItems
  ) # close Body
))