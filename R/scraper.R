#' @export
scraper <- function(url) {
  url = "https://www.espn.com/fantasy/football/story/_/id/36312955/nfl-fantasy-football-rankings-2023-qb-quarterback"
  content = rvest::read_html(url)
  tables = content |> html_elements("table")
  asides = content |> html_elements("aside")
  rankings_table = tables[[2]]
  article = get_element_by_id(content, "section", "article-feed")

  rankings_wrapper = get_element_by_text(content, "h2", "2023 quarterback rankings") |>
    xml_parent()

  table = rankings_wrapper |> html_element("table")
  header = table |> html_element("thead") |> xml_children()
  body = table |> html_element("tbody") |> xml_children()
  xml2::as_xml_document(list(header, body))

  system(paste(config::get('phantomjs_exe'), 'js/phantom.js'))
}

get_element_by_text <- function(xml, element_type, text) {
  elements = xml |> html_nodes(element_type)
  elements[which(elements |> html_text() == text)]
}

get_element_by_id <- function(xml, element_type, id) {
  elements = xml |> html_nodes(element_type)

  elements[[which(elements |> html_attr("id") == id)]]
}

starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
