library(readr)
library(dplyr)
library(magrittr)

dat <- readr::read_csv("data/TEST_OPED_MC_WASHINGTONPOST.csv")

fancy_plot <- function(x, ctr = "United States of America") {
  
  # Generating ids
  ids <- sort(unique(with(x, c(ctry1_rep, ctry2_par)))) %>%
    setdiff(ctr)
  
  ids <- tibble::tibble(
    id   = 1L:length(ids),
    name = ids
  )
  
  years <- x$yr_impl %>% unique %>% sort
  
  plot.new()
  plot.window(xlim = range(years), ylim = range(ids$id))
  
  # Setting us in the middle
  ctr_id <- as.integer(floor(nrow(ids)/2))
  ids[ids$id >= ctr_id, 1L] <- ids[ids$id >= ctr_id, 1L] + 1L
  ids <- rbind(
    ids,
    tibble::tibble(
      id = ctr_id, 
      name = ctr
    )) %>%
    arrange(id)
  
  # all.equal(sort(ids$id), 1L:nrow(ids))
  
  # Receiving countries
  z <- dat %>%
    filter(yr_impl == 2009 & ctry1_rep == ctr) %>%
    group_by(ctry2_par) %>%
    summarize(
      received = sum(protec1)
    ) %>%
    filter(received > 0) %>%
    rename(name = ctry2_par) %>%
    dplyr::left_join(ids)
  
  
  
}