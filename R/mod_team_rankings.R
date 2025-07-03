teamRankingsOverviewUI <- function(id) {
  ns <- NS(id)
  withSpinner(
    reactable::reactableOutput(ns("teamRankingsOverviewTable")),
    type = 8
  )
}

teamRankingsOverviewServer <- function(id,
                                      rankings_data){
  moduleServer(id, function(input, output, session){
    
    overviewData <- reactive({
      rankings_data() |>
        # slice_tail(n = 1, by = team) |>
        # arrange(game_id) |>
        dplyr::select(
          team_logo_espn,
          team,
          games,
          wins,
          losses,
          ties,
          win_pct,
          elo = elo_post,
          off_total_epa_mean,
          def_total_epa_mean,
          off_total_epa_sum,
          def_total_epa_sum,
          pf,
          pa,
          MOV,
          SOS,
          SRS,
          OSRS,
          DSRS
        ) |>
        dplyr::mutate(
          dplyr::across(dplyr::contains("epa"),
                        ~dplyr::cummean(.x)),
          #.names = "{.col}_cum"),
          .keep = "unused",
          .after = elo,
          .by = team
        ) |>
        dplyr::mutate(
          net_total_epa_mean = off_total_epa_mean - def_total_epa_mean,
          .after = def_total_epa_mean
        ) |>
        dplyr::mutate(
          net_total_epa_sum = off_total_epa_sum - def_total_epa_sum,
          .after = def_total_epa_sum
        ) |>
        dplyr::mutate(
          pfg = pf / games,
          pag = pa / games,
          .before = MOV
        ) |>
        dplyr::mutate(
          OSRS_scale = scale(OSRS),
          DSRS_scale = scale(DSRS)
        ) |>
        dplyr::select(-c(pf, pa)) |>
        dplyr::slice_tail(n = 1, by = team) |>
        dplyr::arrange(desc(elo))
      # left_join(
      #   season_week_data |>
      #     slice_tail(n = 1, by = team) |>
      #     select(team, games_played, win, loss, tie, win_loss_percent),
      #   by = join_by(team)
      # ) |>
      # left_join(
      #   team_data |> select(team_abbr, team_logo_espn),
      #   by = join_by(team == team_abbr)
      # ) |>
      # select(team_logo_espn, team,
      #        games_played, win, loss, tie, win_loss_percent,
      #        everything()) |>
      # rename_with(~str_remove(.x, pattern = "team_"), .cols = -c(team_logo_espn, team)) |>
      # rename_with(~str_remove(.x, pattern = "_cum"), .cols = everything())
    })
    
    output$teamRankingsOverviewTable <- renderReactable({
      overviewDataReact <- reactable(
        data = overviewData(),
        theme = fivethirtyeight(
          centered = TRUE,
          header_font_size = "0.9em",
          font_size = "1.0em"
        ),
        highlight = TRUE,
        compact = TRUE,
        pagination = FALSE,
        wrap = FALSE,
        outlined = FALSE,
        bordered = FALSE,
        striped = TRUE,
        sortable = TRUE,
        #showSortable = TRUE,
        defaultSorted = list("elo" = "desc"),
        defaultSortOrder = "desc",
        fullWidth = TRUE,
        # defaultColGroup = colGroup(
        #   headerStyle = list(
        #     border = "none"
        #   )
        # ),
        columnGroups = list(
          colGroup(name = "Record",
                   columns = c("games", "wins", "losses", "ties", "win_pct")),
          colGroup(name = "Elo",
                   columns = c("elo")),
          colGroup(name = "EPA/Play",
                   columns = stringr::str_subset(colnames(overviewData()), "epa_mean")),
          colGroup(name = "EPA/Game",
                   columns = stringr::str_subset(colnames(overviewData()), "epa_sum")),
          colGroup(name = "Points/Game",
                   columns = c("pfg", "pag", "MOV")),
          colGroup(name = "Simple Rating System",
                   columns = c("SOS", "SRS", "OSRS", "DSRS"))
        ),
        defaultColDef = colDef(
          vAlign = "center",
          minWidth = 60,
          align = "center",
          format = colFormat(digits = 2),
          headerStyle = list(
            borderTop = "none",
            paddingTop = "3px"
          )
        ),
        columns = list(
          ## Team ----
          ### Team Logo ----
          team_logo_espn = colDef(
            name = "",
            maxWidth = 40,
            sticky = "left",
            cell = embed_img(height = "35px", width = "35px")
          ),
          ### Team Name ----
          team = colDef(
            name = "Team",
            maxWidth = 60,
            style = list(borderRight = "1.5px solid black")
          ),
          ## Record ----
          ### games ----
          games = colDef(
            name = "GP",
            format = colFormat(digits = 0),
            minWidth = 40,
            align = "center",
            #style = list(borderRight = "1px solid #d3d3d3")
          ),
          ### wins ----
          wins = colDef(
            name = "W",
            align = "center",
            minWidth = 30,
            format = colFormat(digits = 0)
          ),
          ### losses ----
          losses = colDef(
            name = "L",
            align = "center",
            minWidth = 30,
            format = colFormat(digits = 0)
          ),
          ### ties ----
          ties = colDef(
            name = "T",
            align = "center",
            minWidth = 30,
            format = colFormat(digits = 0)
          ),
          ### win_pct ----
          win_pct = colDef(
            name = "W-L%",
            format = colFormat(percent = TRUE, digits = 1),
            align = "center",
            #minWidth = 50,
            style = list(borderRight = "1px solid #d3d3d3")
          ),
          ## elo ----
          elo = colDef(
            #minWidth = 70,
            format = colFormat(digits = 0),
            #style = list(borderRight = "1px solid #d3d3d3"),
            style = color_scales(
              data = overviewData(),
              colors = c("red","pink", "whitesmoke", "palegreen", "green"),
              bias = 1
            )
          ),
          ## EPA/Play ----
          off_total_epa_mean = colDef(
            show = FALSE,
            name = "Off"
          ),
          def_total_epa_mean = colDef(
            show = FALSE,
            name = "Def"
          ),
          net_total_epa_mean = colDef(
            show = FALSE,
            name = "Net",
            style = list(borderRight = "1px solid #d3d3d3")
          ),
          ## EPA/Game ----
          off_total_epa_sum = colDef(
            name = "Off"
          ),
          def_total_epa_sum = colDef(
            name = "Def"
          ),
          net_total_epa_sum = colDef(
            name = "Net",
            style = list(borderRight = "1px solid #d3d3d3")
          ),
          ## Points ----
          ### pfg ----
          pfg = colDef(
            name = "PF",
            format = colFormat(digits = 2)
          ),
          ### pag ----
          pag = colDef(
            name = "PA",
            format = colFormat(digits = 2)
          ),
          ### MOV ----
          MOV = colDef(
            format = colFormat(digits = 2),
            style = list(borderRight = "1px solid #d3d3d3")
          ),
          ## Simple Rating System ----
          ### SOS ----
          SOS = colDef(
            format = colFormat(digits = 2)
          ),
          ### SRS ----
          SRS = colDef(
            format = colFormat(digits = 2),
            # cell = data_bars(overviewData(),
            #                  fill_color = c('tomato','white','dodgerblue'),
            #                  #fill_gradient = TRUE,
            #                  text_position = "outside-end",
            #                  number_fmt = scales::label_number(accuracy = .01))
            style = color_scales(
              data = overviewData(),
              colors = c("red","pink", "whitesmoke", "palegreen", "green"),
              #colors = c('tomato','whitesmoke','dodgerblue'),
              bias = 1
            )
          ),
          ### OSRS ----
          OSRS = colDef(
            format = colFormat(digits = 2),
            cell = color_tiles(overviewData(), 
                               number_fmt = scales::label_number(accuracy = .01),
                               colors = c('tomato','white','dodgerblue'),
                               color_by = "OSRS_scale",
                               bias = 1,
                               box_shadow = TRUE)
          ),
          OSRS_scale = colDef(
            show = FALSE
          ), 
          ### DSRS ----
          DSRS = colDef(
            format = colFormat(digits = 2),
            cell = color_tiles(overviewData(), 
                               number_fmt = scales::label_number(accuracy = .01),
                               colors = c('tomato','white','dodgerblue'),
                               color_by = "DSRS_scale",
                               bias = 1, 
                               box_shadow = TRUE)
          ),
          DSRS_scale = colDef(
            show = FALSE
          )
        )
      )
      overviewDataReact
    })
  })
}


#' @title Team Rankings Overview Table (UI)
#' @description Output UI for a team_rankings table module.
#' @param id Shiny module id.
#' @return A UI output for reactable standings table.
#' @export
#' @noRd
mod_team_rankings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(style = "margin-right: 1rem",
          # virtualSelectInput(
          selectInput(
            inputId = ns("season"),
            label = "Select season",
            choices = seq(2007, get_current_season()),
            selected = get_current_season()
          )
      )
    ),
    br(),
    fluidRow(
      tabBox(
        id = ns("tabBox"),
        type = "pills",
        width = 12,
        #### Overview ----
        tabPanel(
          title = "Overview",
          value = "overview_tab",
          teamRankingsOverviewUI(ns("overview"))
        ),
        #### EPA ----
        tabPanel(
          title = "EPA",
          value = "epa_tab"
        ),
        #### ELO ----
        tabPanel(
          title = "Elo",
          value = "elo_tab"
        ),
        #### SRS ----
        tabPanel(
          title = "SRS",
          value = "srs_tab"
        )
      ) # end Team Rankings Tab box
    )
  )
}


mod_team_rankings_server <- function(id,
                                     team_features_data,
                                     teams_data = teams_data){
  moduleServer(id, function(input, output, session){
    seasonRankings <- reactive(input$season)
    
    rankings_data <- reactive({
      req(seasonRankings())
      team_features_data |> dplyr::filter(season == seasonRankings()) |>
        dplyr::left_join(
          teams_data |> dplyr::select(team_abbr, team_logo_espn),
          by = dplyr::join_by(team == team_abbr)
        )
    })
    
    teamRankingsOverviewServer(
      id = "overview",
      rankings_data = rankings_data
    )
  })
}






