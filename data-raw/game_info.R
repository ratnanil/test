## code to prepare `game_info` dataset goes here

library(jsonlite)
library(tidyverse)
library(yaml)
full <- read_json("https://raw.githubusercontent.com/lsv/uefa-euro-2020/master/data.json")

vals <- c("id", "name", "matchtype", "home_team", "away_team","home_result", "away_result", "date","stadium", "finished","matchday") #
# "id", "name", "matchtype", "home_team", "away_team", "home_result", "away_result", "date", "stadium", "tvchannels", "finished", "matchday"

matches_groupphase <- imap_dfr(full$groups, function(y, grp){
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


matches_groupphase_list <- matches_groupphase %>%
  mutate(
    group = paste("Gruppe",group)
  ) %>%
  split(.$group) %>%
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
