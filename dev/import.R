pos = c('QB','RB','WR','TE','K','DST')
# Next year, up the WRs to make them more valuable
n_pos = c('QB' = 2, 'RB' = 5, 'WR'=4, 'TE'=2, 'DST'=1, 'K'=1)*10
filter_to_pos <- function(df, n_pos) {
  purrr::map_dfr(1:length(n_pos), ~df |> filter(pos == names(n_pos)[.x]) |> slice(1:(n_pos[.x])))
}

############################################################################################################
#### Sportsline
############################################################################################################
read_sportsline_sheet <- function(sheet) {
  readxl::read_excel(config::get('sportsline_file'), sheet=sheet) |>
    dplyr::mutate(pos=sheet)
}

# Sportsline is ranked per position from 99 (best) to 1 (worst) per position, with blank rows in between
# We set a per_pos_ranking as 1-99
sportsline = purrr::map_dfr(pos, read_sportsline_sheet) |>
  # Remove blank rows
  filter(!is.na(PLAYER)) |>
  mutate(
    rating = as.numeric(`OPTIMAL POSITION RATING`),
    PLAYER = str_replace_all(PLAYER, "[\\.]",""),
    ROUND = as.numeric(ROUND)
  ) |>
  # Rank from 100 to 0
  arrange(desc(rating)) |>
  # Take the top N at each position
  filter_to_pos(n_pos) |>
  # For each position
  group_by(pos) |>
  mutate(
    # Set the dropoff (how much better the player is than the next best)
    dropoff = rating - dplyr::lead(rating)
  ) |>
  # Arrange from worst to best
  arrange(rating) |>
  mutate(
    # value is the cumulative dropoff for players below the player (e.g.)
    # E.g. take RB6 -> WR7 -> WR8 -> RB9 -> RB10. RB6's dropoff is 3, and value is 3+1+...
    # Thus RB6's value is 3 higher than RB9
    value = cumsum(replace_with_prev(dropoff))
  ) |>
  ungroup() |>
  # Scale the players' values (across all positions) to be from 0-100
  mutate(value = scale_value(value)) |>
  select(PLAYER, pos, sportsline_value=value, sportsline_round=ROUND)

############################################################################################################
#### CBS
############################################################################################################

# CBS is simply a rank of the top 200 so we should view the row number as the ADP
# We set the value:
#    Arrange players by their ADP
#    Per position, set the dropoff of their ADP to the next player in that position's ADP
#    Set the value as the player's (+ all the players below that player's) cumulative dropoff
#    Scale the value per position
#
#    If we wanted to functionalize this, we would just need ADP and pos
cbs = readr::read_tsv(config::get('cbs_consensus'),show_col_types = FALSE) |>
  mutate(
    # CBS messed up these players I think
    PLAYER = case_when(
      PLAYER == 'B. Robinson Jr.'~'Brian Robinson Jr',
      PLAYER == 'B. Robinson'~'Bijan Robinson',
      PLAYER == 'T. Hill' & pos == 'WR'~'Tyreek Hill',
      PLAYER == 'T. Hill' & pos == 'TE'~'Taysom Hill',
      TRUE~PLAYER
    ),
    # The players are listed by draft pick, so we simply need to add draft pick
    adp = dplyr::row_number(),
    player_regex = case_when(
      grepl('^[A-Z]\\. [A-Za-z ]+$', PLAYER)~str_replace(PLAYER,'\\.','.*'),
      TRUE~PLAYER
    ),
    rank = dplyr::row_number()
  ) |>
  # Arrange the players from best to worst
  arrange(rank) |>
  # Take the top N at each position
  filter_to_pos(n_pos) |>
  # For each position
  group_by(pos) |>
  mutate(
    # Set the dropoff (how much better the player is than the next best)
    dropoff = rating - dplyr::lead(rating)
  ) |>
  # Arrange from worst to best
  arrange(desc(rank)) |>
  mutate(
    # value is the cumulative dropoff for players below the player (e.g.)
    # E.g. take RB6 -> WR7 -> WR8 -> RB9 -> RB10. RB6's dropoff is 3, and value is 3+1+...
    # Thus RB6's value is 3 higher than RB9
    value = cumsum(replace_with_prev(dropoff))
  ) |>
  ungroup() |>
  mutate(
    # Scale the players' values (across all positions) to be from 0-100
    value = scale_value(value)
  ) |>
  select(player_regex, pos, cbs_rank = rank, cbs_value = value)

############################################################################################################
#### ESPN
############################################################################################################


cheat_sheet_text = readLines(config::get('espn_cheet_sheet'))
get_columns <- function(cheat_sheet_text) {
  column_extract_regex = '^[0-9]+\\. \\([A-Z]+[0-9]+\\) [0-9A-Za-z\\. \'\\-/]+, [A-Z]+ \\$[0-9]+ [0-9]+ ?'
  columns = tibble::tibble(column=character())
  i=0
  for(line in cheat_sheet_text) {
    i=i+1
    while(nchar(line) > 0) {
      if(!str_detect(line, column_extract_regex)) line = ""
      columns = columns |> add_row(column = str_extract(line, column_extract_regex))
      line = str_replace(line, column_extract_regex, '')
    }
  }
  return(columns)
}

columns = get_columns(cheat_sheet_text)
x = columns |>
  mutate(
    g = str_match_all(column, '^(?<rank>[0-9]+)\\. \\((?<pos>[A-Z]+)[0-9]+\\) (?<PLAYER>[0-9A-Za-z\\. \'\\-/]+), [A-Z]+ \\$(?<rating>[0-9]+) [0-9]+ ?')
    #rank = str_extract(column, '^[0-9]+'),
    #pos = '^[0-9]+\\. \\(\\([A-Z]+\\)[0-9]+\\)'
  )

espn_cheat_sheet_raw = purrr::map(pull(x,g), as.data.frame) |>
  purrr::list_rbind() |>
  as_tibble()

espn_cheat_sheet = espn_cheat_sheet_raw |>
  mutate(
    rating = as.numeric(rating),
    rank = as.numeric(rank),
    PLAYER = str_replace_all(PLAYER, '[\\.]', '') |>
      str_replace(' D/ST$','')
  ) |>
  # Arrange from best to worst (using rank since dollar amounts might have ties
  arrange(rank) |>
  # Take the top N at each position
  filter_to_pos(n_pos) |>
  # For each position
  group_by(pos) |>
  mutate(
    # Set the dropoff (how much better the player is than the next best)
    # We have to do things differently here because we're sorting by rank and not rating.
    dropoff = rating - coalesce(dplyr::lead(rating),0)
  ) |>
  # Sort from worst to best
  arrange(rating) |>
  mutate(
    # value is the cumulative dropoff for players below the player (e.g.)
    # E.g. take RB6 -> WR7 -> WR8 -> RB9 -> RB10. RB6's dropoff is 3, and value is 3+1+...
    # Thus RB6's value is 3 higher than RB9
    value = cumsum(replace_with_prev(dropoff))
  ) |>
  ungroup() |>
  # Scale the players' values (across all positions) to be from 0-100
  mutate(value = scale_value(value)) |>
  select(PLAYER, pos, espn_rank = rank, espn_value = value)

############################################################################################################
#### D/ST map
############################################################################################################
dst_map = jsonlite::fromJSON(config::get('dst_map_json'))

############################################################################################################
#### Merge
############################################################################################################
overall = sportsline |>
  full_join(espn_cheat_sheet, by = c("PLAYER", "pos")) |>
  fuzzyjoin::regex_full_join(cbs |> select(-pos), by = c("PLAYER" = "player_regex")) |>
  mutate_at(vars(contains("_value")), ~coalesce(.,0)) |>
  dplyr::rowwise() |>
  mutate(
    avg_value = round((espn_value + sportsline_value + cbs_value)/3, 1),
    sd_value = sd(c(espn_value, sportsline_value, cbs_value)),
    sportsline_round = coalesce(sportsline_round, max(coalesce(sportsline_round, 0))),
    espn_rank = coalesce(espn_rank, nrow(espn_cheat_sheet)),
    cbs_rank = coalesce(cbs_rank, nrow(cbs)),
    sportsline_rank = sportsline_round*10-5, # average per round
    avg_rank = round((coalesce(espn_rank,nrow(espn_cheat_sheet)) + coalesce(cbs_rank,nrow(cbs)) + coalesce(sportsline_rank,max(sportsline_rank)))/3, 1),
    sd_rank = sd(c(espn_rank, sportsline_rank, cbs_rank))
  ) |>
  ungroup() |>
  arrange(desc(avg_value)) |>
  select(
    PLAYER, pos,
    avg_value, sd_value, sportsline_round, sportsline_value, cbs_value, espn_value,
    avg_rank, sd_rank, sportsline_rank, cbs_rank, espn_rank,
  )

googlesheets4::write_sheet(
  overall,
  "https://docs.google.com/spreadsheets/d/15lT7yaLF27vUR48KikooXetWlwDy2fN6fb27MtVLztc/edit#gid=0",
  sheet = as.character(Sys.Date())
)
