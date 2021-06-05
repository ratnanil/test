## code to prepare `game_info` dataset goes here

library(jsonlite)
library(tidyverse)
library(yaml)
library(glue)
full_json <- read_json("https://raw.githubusercontent.com/lsv/uefa-euro-2020/master/data.json")

as_tibble(full_json$teams[[1]][["flag"]])

teams <- map_dfr(full_json$teams, function(x){
  cbind(
    as_tibble(x[c("id","name","rank","disciplinary")]),
    as_tibble(x[["flag"]])
  )
})


vals <- c("id", "name", "matchtype", "home_team", "away_team","home_result", "away_result", "date","stadium", "finished","matchday") #
# "id", "name", "matchtype", "home_team", "away_team", "home_result", "away_result", "date", "stadium", "tvchannels", "finished", "matchday"

matches_groupphase <- imap_dfr(full_json$groups, function(y, grp){
  map_dfr(y$matches, function(x){
    values <- x[vals]

    if(!values[["finished"]]){
      values[["home_result"]] <- NA_integer_
      values[["away_result"]] <- NA_integer_
    }
    as_tibble(values) %>%
      mutate(group = grp)
  })
})


matches_groupphase <- matches_groupphase %>%
  mutate(date = parse_datetime(date))




# matches_groupphase_list <-
matches_groupphase %>%
  mutate(
    group = paste("Gruppe",group)
  ) %>%
  split(.$group) %>%
  imap(~printlines(.x, .y, ".github/ISSUE_TEMPLATE"))


printlines <- function(grp, grpid, pth){

  grpph <- paste0("Gruppenphase (Gruppe",grpid,")")

  lines_header <- c(
    "---",
    glue("name: {grpph}"),
    glue("about: Tips für {grpph}"),
    glue("title: 'Tips {grpph}'"),
    glue("labels: '{grpph}'"),
    "assignees: 'ratnanil'",
    "---",
    ""
  )

  gr_teams <- grp %>%
    select(home_team) %>%
    pull() %>%
    unique()

  lines_team <- teams %>%
    filter(id %in% gr_teams) %>%
    transmute(team = paste(id, ": ",name)) %>%
    pull()

  lines_games <- grp %>%
    transmute(id,
              teams = paste(home_team, away_team, sep = " vs "),
              datum = date) %>%
    pmap(function(id, teams, datum){
        glue("Spiel {id} {teams}:")
    }) %>%
    unlist()

  cocat <- c(
    lines_header,
    lines_team,
    "",
    lines_games
  )


  write_lines(cocat, file.path(pth, glue("gruppenphase_{grpid}.md")))

}




map(function(x){

  x %>%
    mutate(spiel = paste("Spiel",row_number())) %>%
    split(.$spiel) %>%
    map(function(y){
      y %>%
        transmute(datum = strftime(date,"%d.%m.%Y %H:%M:%S"),
                  teams = paste(home_team, away_team, sep = " vs "),
                  tipp = "") %>%
        as.list()
    })
}) %>%
  write_yaml(".github/ISSUE_TEMPLATE/groupphase.md")


usethis::use_data(matches, overwrite = TRUE)
