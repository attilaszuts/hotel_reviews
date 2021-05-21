library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "Hotel reviews"
  ), # close Header
  dashboardSidebar(
    sidebarMenu(
      menuItem('Positive - Negative', tabName = 'pn', icon = icon('concierge-bell'))
    )
  ), # close Sidebar
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'pn',
        h1('hello ceu')
        ) # close tabItem 'pn'
    ) # close tabItems
  ) # close Body
))