cheat_sheet_text = readLines(config::get('espn_cheet_sheet'))
get_columns <- function(cheat_sheet_text) {
  column_extract_regex = '^[0-9]+\\. \\([A-Z]+[0-9]+\\) [0-9A-Za-z\\. \'\\-/]+, [A-Z]+ \\$[0-9]+ [0-9]+ ?'
  columns = tibble::tibble(column=character())
  i=0
  for(line in cheat_sheet_text) {
    i=i+1
    while(nchar(line) > 0) {
      if(!str_detect(line, column_extract_regex)) line = ""
      columns = columns %>% add_row(column = str_extract(line, column_extract_regex))
      line = str_replace(line, column_extract_regex, '')
    }
  }
}

columns = get_columns(cheat_sheet_text)
x = columns |>
  mutate(
    g = str_match_all(column, '^(?<rank>[0-9]+)\\. \\((?<pos>[A-Z]+)[0-9]+\\) (?<PLAYER>[0-9A-Za-z\\. \'\\-/]+), [A-Z]+ \\$(?<rating>[0-9]+) [0-9]+ ?')
    #rank = str_extract(column, '^[0-9]+'),
    #pos = '^[0-9]+\\. \\(\\([A-Z]+\\)[0-9]+\\)'
  )

espn_cheat_sheet = purrr::map(pull(x,g), as.data.frame) |> purrr::list_rbind() |> as_tibble()
