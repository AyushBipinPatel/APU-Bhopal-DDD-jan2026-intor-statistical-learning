### Script to get MPC minuites from the rbi website

### the main url : https://website.rbi.org.in/web/rbi/press-releases?q=%22minutes+of+the+monetary+policy+committee%22&delta=100


# library and setup ----------------------------------------------------------------

library(tidyverse)
library(rvest)

url <- "https://website.rbi.org.in/web/rbi/press-releases?q=%22minutes+of+the+monetary+policy+committee%22&delta=100"

# button: div.btn-wrap a.matomo_download.download_link
# indentifier: span.mtm_list_item_heading
# 



# get download links and doc indentifier-----------------------------------------------------

read_html(url) |>
  html_elements("div.btn-wrap") |>
  html_element("a.matomo_download") |>
  html_attr("href") -> mpc_minutes_links

read_html(url) |>
  html_elements("span.mtm_list_item_heading") |>
  html_text2()-> mpc_minutes_identifier

tibble(
  identifier = mpc_minutes_identifier,
  d_link = mpc_minutes_links
) |>
  mutate(
    d_link = ifelse(str_starts(d_link,"https"),d_link, paste0("https://website.rbi.org.in",d_link))
  ) -> MPC_minutes_table


# get text from all pdfs -------------------------------------------------

get_mpc_minutes <- function(idn,lnk) {
  
  pdftools::pdf_text(lnk) |>
  str_split(pattern = "\n") |>
  list_c() -> text
  
  tibble(
    identifier = rep(idn,length(text)),
    lines = text
  )

}

pmap(
  list(
    idn = MPC_minutes_table$identifier,
    lnk = MPC_minutes_table$d_link
  ),
  get_mpc_minutes,
  .progress = T
) -> MPC_text_files

MPC_text_files |>
  list_rbind() -> MPC_text_files


# save the data ----------------------------------------------------------

write.csv(MPC_text_files,"rbi-mpc-for-text-analyses/mpc-text-files.csv")
