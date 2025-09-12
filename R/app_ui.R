#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bs4Dash dplyr reactable
#' @importFrom shinyWidgets virtualSelectInput radioGroupButtons prettyCheckboxGroup noUiSliderInput wNumbFormat
#'  prepare_choices
#' @importFrom shinycssloaders withSpinner
#' @importFrom reactablefmtr group_border_sort group_merge_sort embed_img fivethirtyeight color_scales
#'  color_tiles data_bars
#' @importFrom nflreadr load_teams get_current_season get_current_week
#' @noRd

app_ui <- function(request) {
  htmltools::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dark = TRUE,
      footer = dashboardFooter(left = br()),
      freshTheme = fresh::create_theme(
        theme = "paper",
        fresh::bs4dash_sidebar_dark(
          bg = "#2d3b4d"
        ),
        fresh::bs4dash_status(
          primary = "purple",
          info = "#eec900"
        )
      ),
      #freshTheme = my_theme,
      #preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),

      # Dahsboard Header ===============
      header = dashboardHeader(
        title = dashboardBrand(
          title = div(
            style = "font-size:14pt",
            align = "center",
            "NFL EndZone Analytics",
            dashboardBadge(
              color = "warning",
              rounded = TRUE,
              position = "right",
              "v1.0"
            )
          ),
          color = "primary"
        ),
        compact = FALSE,
        fixed = TRUE,
        rightUi = tags$li(
          class = "dropdown",
          dropdownMenu(
            badgeStatus = NULL,
            type = "notifications",
            headerText = "NFL EndZone Analytics",
            icon = icon("info-circle"),
            notificationItem(
              inputId = "info1",
              text = "Developer: Tyler Pollard",
              icon = icon("users-cog"),
              status = "info"
            ),
            notificationItem(
              inputId = "info2",
              text = "Release Date: 20 Oct 2024",
              icon = icon("calendar"),
              status = "info"
            ),
            notificationItem(
              inputId = "info3",
              text = "Version: 1.0",
              icon = icon("code"),
              status = "info"
            )
          )
        ),
        ## Navbar Menu ------------------
        navbarMenu(
          id = "nav_menu",
          ### Home Tab ----
          navbarTab(tabName = "home_tab", text = "Home")
        ) # end navbarMenu
      ), # close header
      scrollToTop = TRUE,
      # Dashboard Sidebar =============
      sidebar = dashboardSidebar(
        #id = "sidebar",
        skin = "dark",
        elevation = 5,
        fixed = TRUE,
        minified = FALSE,
        status = "primary",
        compact = FALSE,
        collapsed = FALSE,
        #width = "150px",
        ## Sidebar Menu ---------------
        sidebarMenu(
          id = "menu_items",
          ### Data Tab ----
          h4("Data", style = "color: white"),
          menuItem(
            text = "Standings",
            tabName = "standings_tab",
            icon = icon("list-ol")
          ),
          #menuItem(text = "Scores", tabName = "scoresTab", icon = icon("football-ball")),
          menuItem(
            text = "Team Statistics",
            icon = icon("users"),
            expandedName = "team_statistics",
            menuSubItem(text = "Rankings", tabName = "team_rankings_tab"),
            menuSubItem(text = "Scoring", tabName = "team_scoring_tab"),
            menuSubItem(text = "Efficiency", tabName = "team_efficiency_tab")
          ),
          menuItem(
            text = "Player Statistics",
            icon = icon("user"),
            expandedName = "player_statistics",
            menuSubItem(text = "Offense", tabName = "player_offense_tab"),
            menuSubItem(text = "Defense", tabName = "player_defense_tab"),
            menuSubItem(text = "Special Teams", tabName = "player_special_tab"),
            menuSubItem(text = "Scoring", tabName = "player_scoring_tab"),
            menuSubItem(text = "Fantasy", tabName = "player_fantasy_tab")
          ),
          ### Betting Tab ----
          h4("Betting", style = "color: white"),
          menuItem(
            text = "Games",
            tabName = "betting_games_tab",
            icon = icon("dollar-sign")
          ),
          menuItem(
            text = "Player Props",
            tabName = "betting_player_props_tab",
            icon = icon("user-clock")
          ),
          menuItem(
            text = "Plot",
            tabName = "betting_plot_tab",
            icon = icon("chart-line")
          )
        ) # close sidebar menu
      ), # close dashboard sidebar
      # Dashboard Controlbar ==================
      controlbar = dashboardControlbar(
        id = "controlbar",
        collapsed = TRUE,
        width = 600,
        pinned = FALSE,
        fluidRow(
          tableOutput('show_inputs1')
        )
      ),
      # Dashboard Body ================
      body = dashboardBody(
        # useShinyjs(),
        ## Custon CSS ----
        # tags$head(
        #   tags$style(HTML("
        #   #team_rankings_tabBox_box .card-body { padding: 0px; }
        # "))
        # ),
        tabItems(
          # Home Tab  ###############################################
          tabItem(
            tabName = "home_tab",

            # Welcome Jumbotron ----
            bs4Jumbotron(
              title = h1("Welcome to the NFL EndZone Analytics Dashboard"),
              lead = shiny::includeMarkdown(app_sys("app/docs/Purpose.Rmd")),
              #lead = "This dashboard explores historical and current NFL data using powerful visualizations and modeling tools.",
              status = "primary",
              btnName = NULL,
              btnHref = NULL,
              div(
                style = "display: flex; justify-content: center; align-items: center; margin-top: 20px;",
                tags$img(
                  src = "www/nfl_logo.jpeg",
                  style = "max-width: 90%; height: auto; max-height: 400px; border-radius: 8px; box-shadow: 0px 4px 12px rgba(0,0,0,0.3);"
                )
              )
              #img = "www/nfl_logo.jpeg",
              #height = "400px",
              #includeMarkdown("./docs/Purpose.Rmd")
            ),
            #h1("Welcome to the NFL Game Dashboard", align  = "center"),
            #br(),
            box(
              width = 12,
              closable = FALSE,
              collapsible = FALSE, #headerBorder = FALSE,
              title = h2("App Overview", align = "center"),
              # fluidRow(column(width = 12, align = "center",
              #                 imageOutput("image"))
              # ),
              br(),
              shiny::withMathJax(),
              shiny::includeMarkdown(app_sys("app/docs/Description.Rmd"))
            ) # end box
          ), # close Home tab Item
          # Data Tab ################################################
          ## Standings Tab ##########################################
          tabItem(
            tabName = "standings_tab",
            mod_standings_ui("standings")
            #fluidPage(
            # fluidRow(
            #   ##### Inputs ----
            #   ###### Season ----
            #   #column(width = 1,
            #   div(style = "margin-right: 1rem",
            #       virtualSelectInput(
            #         inputId = "standings_Season",
            #         label = "Select season",
            #         choices = seq(2007, get_current_season()),
            #         selected = get_current_season()
            #       )
            #   ),
            #   ###### Table Stat ----
            #   #column(width = 2,
            #   div(style = "margin-right: 1rem",
            #       radioGroupButtons(
            #         inputId = "standings_stat",
            #         label = "Table Statistic",
            #         choices = c("Total", "Game"),
            #         status = "info"
            #       )
            #   ) # end column
            # ), # end fluidRow
            # br(),
            # ##### Season Table ----
            # fluidRow(
            #   style = "margin-left: -7.5px; margin-right: -7.5px",
            #   column(
            #     width = 6,
            #     style = "padding: 0px"
            #     #standings_tableOutput("standings_tableAFC")
            #   ), # end AFC column
            #   column(
            #     width = 6,
            #     style = "padding: 0px"
            #     #standings_tableOutput("standings_tableNFC")
            #   ) # end NFC column
            # ), # end divsion standings row
            # br(),
            # ##### Playoffs Table ----
            # fluidRow(
            #   column(
            #     width = 6
            #     #standingsPlayoffsTableOutput("standingsPlayoffsTableAFC")
            #   ), # end AFC column
            #   column(
            #     width = 6
            #     #standingsPlayoffsTableOutput("standingsPlayoffsTableNFC")
            #   ) # end NFC column
            # ) # end playoff standings row
            #) # end fluidPage
          ), # end Standings tabItem
          ## Team Tab ###############################################
          ### Team Rankings ==========================================
          tabItem(
            tabName = "team_rankings_tab",
            mod_team_rankings_ui("team_rankings")
            # h2("Team Rankings"),
            # #### Inputs ----
            # fluidRow(
            #   #column(width = 1,
            #   virtualSelectInput(
            #     inputId = "team_rankings_season",
            #     #width = "100%",
            #     label = "Select season",
            #     choices = seq(2007, get_current_season()),
            #     selected = get_current_season()
            #   )
            #   #) #end column
            # ), # end fluidRow
            # br(),
            # # tags$style(
            # #   ".card-body {padding: 0px;}"
            # # ),
            # fluidRow(
            #   # tags$style(
            #   #   ".col-sm-12 {padding: 0px;}"
            #   # ),
            #   tabBox(
            #     id = "team_rankings_tabBox",
            #     type = "pills",
            #     width = 12,
            #     #### Overview ----
            #     tabPanel(
            #       title = "Overview",
            #       value = "team_rankings_overview"
            #       #teamRankingsOverviewUI("team_rank_overview")
            #     ),
            #     #### EPA ----
            #     tabPanel(
            #       title = "EPA",
            #       value = "team_rankings_epa"
            #     ),
            #     #### ELO ----
            #     tabPanel(
            #       title = "Elo",
            #       value = "team_rankings_elo"
            #     ),
            #     #### SRS ----
            #     tabPanel(
            #       title = "SRS",
            #       value = "team_rankings_srs"
            #     )
            #   ) # end Team Rankings Tab box
            # )
          ), #end Team Rankings Tab
          ### Team Scoring ==========================================
          ### Team Efficiency ====================================
          ## Player Tab  ############################################
          ### Player Offense ========================================
          tabItem(
            tabName = "player_offense_tab",
            h2("Offensive Player Data"),
            tags$style(HTML(
              ".vscomp-dropbox-container  {z-index:99999 !important;}"
            )),

            #### Inputs ----
            fluidRow(
              ##### Season ----
              column(
                width = 3,
                noUiSliderInput(
                  inputId = "player_offense_season",
                  label = "Select seasons",
                  min = 2006,
                  max = get_current_season(),
                  step = 1,
                  value = c(get_current_season(), get_current_season()),
                  limit = 5,
                  behaviour = "drag",
                  format = wNumbFormat(decimals = 0)
                )
                # sliderInput(
                #   inputId = "playerOffenseSeason",
                #   label = "Select seasons",
                #   min = 2003,
                #   max = get_current_season(),
                #   step = 1,
                #   value = c(get_current_season(),get_current_season())
                # ),
              ),

              ##### Game Type ----
              column(
                width = 2,
                prettyCheckboxGroup(
                  inputId = "player_offense_gameType",
                  label = "Game Type",
                  choices = c("Regular Season" = "REG", "Playoffs" = "POST"),
                  selected = "REG",
                  inline = FALSE,
                  status = "info",
                  fill = TRUE
                )
              ),
              ##### Team ----
              column(
                width = 3,
                virtualSelectInput(
                  inputId = "player_offense_team",
                  label = "Select team to analyze",
                  choices = prepare_choices(
                    .data = teams_picker_choices,
                    label = team_name,
                    value = team_abbr,
                    group_by = team_division
                  ),
                  multiple = TRUE,
                  selected = teams_picker_choices$team_abbr,
                  showSelectedOptionsFirst = TRUE
                )
              ),
              ##### Table Stat ----
              column(
                width = 2,
                radioGroupButtons(
                  inputId = "player_offense_stat",
                  label = "Table Statistic",
                  choices = c("Total", "Game"),
                  status = "info"
                )
              ) # end column
            ), # end fluidRow
            tabBox(
              type = "pills",
              width = 12,
              #### Overview ----
              tabPanel(
                title = "Overview"
              ),
              #### Passing ----
              tabPanel(
                title = "Passing"
                #playerOffensePassingTableOutput("playerOffensePassingTable")
              ),
              #### Rushing ----
              tabPanel(
                title = "Rushing"
              ),
              #### Receiving ----
              tabPanel(
                title = "Receiving"
              )
            )
          ), # end Player Offense tabItem
          ### Player Defense ========================================
          #### Overview ----
          ### Player Special Teams ==================================
          #### Kick/Punt Returns ----
          #### Kicking ----
          #### Punting ----
          ### Player Scoring ========================================
          #### Overview ----
          ### Player Fantasy ========================================
          #### Ranks ----
          # Betting Tab  ############################################
          ## Games Tab ==============================================
          tabItem(
            tabName = "betting_games_tab",

            tabsetPanel(
              id = "betting_games_tabset",
              ### Lines ----
              tabPanel(
                title = "Lines",
                br(),
                h3("Betting Game Lines"),
                fluidRow(
                  column(
                    1,
                    selectInput(
                      inputId = "betting_season",
                      label = "Season",
                      choices = 2007:get_current_season(),
                      selected = get_current_season()
                    )
                  ),
                  column(
                    1,
                    selectInput(
                      inputId = "betting_week",
                      label = "Week",
                      choices = 1:get_current_week(),
                      selected = get_current_week()
                    )
                  )
                ),
                br(),
                fluidRow(
                  column(
                    width = 12,
                    # withSpinner(
                    #   bettingGamesLinesUI("gameLines"),
                    #   type = 8
                    # ),
                    align = "center"
                  )
                )
              ), # end Lines tabPanel

              ### Predictions ----
              tabPanel(
                title = "Predictions",
                br(),
                h3("Betting Predictions"),
                br()
                # Game detail header (populated when a game is clicked on the "Lines" tab)
                #bettingGameDetailUI("gameDetail") # end fluidRow
              ) # end Prediction tabPanel
            ) # end tabsetPanel
          ), #end betting_games_tab

          ## Player Props =========================================
          tabItem(
            tabName = "betting_player_props_tab",
            fluidRow(
              #tableOutput('show_inputs1')
            )
          ),

          ## Plot =================================================
          tabItem(
            tabName = "betting_plot_tab",
            h4("Plot Data"),
            fluidRow(
              # column(width = 3,
              #        modDataPlotInputUI("modDataPlotInput",
              #                           teams_picker_choices)
              # ),
              # column(width = 9,
              #        modDataPlotOutput("modPlot")
              # )
            )
          )
        ) # end tab Items
      ) # end dashboard body
    ) # end dashboard Page
  ) # end tagList
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import bs4Dash
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  # add_resource_path(
  #   "docs",
  #   app_sys("app/docs")
  # )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nflendzone"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}
