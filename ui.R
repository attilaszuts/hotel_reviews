library(shiny)
library(shinydashboard)
library(shinycssloaders)

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
        withSpinner(uiOutput('wordcloud'), type = 5, color = "#222d32")
      ) # close tabItem 'cloud'
    ) # close tabItems
  ) # close Body
))