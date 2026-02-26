### Script to clean the MPC minutes data


# library ----------------------------------------------------------------

library(tidyverse)


# read data --------------------------------------------------------------

read_csv("rbi-mpc-for-text-analyses/mpc-text-files.csv") -> MPC_text_table


# clean data -------------------------------------------------------------

MPC_text_table |> 
  select(-`...1`) |>
  filter(!is.na(lines)) |> 
  mutate(
    appendage_end_mark = str_detect(lines, "^2\\. ")
  )|>
  group_by(identifier) |>
  mutate(
    begin_data = which(appendage_end_mark == T),
    rn = row_number()
  ) |> 
  ungroup() |>
  filter(rn >= begin_data, .by = identifier) |> 
  filter(!str_detect(lines,"^[:digit:]$")) |>
  select(identifier,lines) |>
  write_csv("rbi-mpc-for-text-analyses/cleaned-mpc-minutes-text.csv")


