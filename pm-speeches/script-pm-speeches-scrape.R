### get speeches by Dr. MMS and ABV 


# libr -------------------------------------------------------------------

library(tidyverse)

#  get speech  -----------------------------------------------------------

## raw speech text -mms

mms_vol3 <- pdftools::pdf_text(pdf = "pm-speeches/selectedspeeches03unse.pdf") 

## speech and title extraction - mms


get_all_speechs <- function(title, pg, last_pg, data) {
    
  tibble(
    title = title,
    text = str_c(data[c(pg:last_pg)],collapse = "\n")
  )
  
  
}

tibble(
    content = mms_vol3[7:20] |>
                read_lines() |>
                str_squish()
) |> 
  separate(col = content,sep = "(?<=[[:upper:]]) (?=[[:digit:]]{1,3}$)",into = c("title","pg")) |>
  filter(title != "") |> 
  fill(pg,.direction =  "down") |> 
  filter(!is.na(pg)) |>
  group_by(pg) |>
  summarise(
    title = str_c(title, collapse = " ")
  ) |>
  ungroup() |>
  arrange(as.numeric(pg)) |> 
  mutate(
    last_pg = lead(as.integer(pg)) -1
  ) |> 
  filter(!is.na(last_pg)) -> drmms_speech_contents_vol3

pmap(
    drmms_speech_contents_vol3 |>
      relocate(title,pg,last_pg) |>
      mutate(
        pg =as.numeric(pg) +20,
        last_pg = last_pg+20
      ),
    get_all_speechs,data = mms_vol3
) |>
  list_rbind() -> drmms_speech_text_df_3



write.csv(drmms_speech_text_df_3,"pm-speeches/DrManmohan-singh-speeches.csv")
