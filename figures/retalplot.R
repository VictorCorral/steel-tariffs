library(readr)
library(dplyr)
library(magrittr)

dat <- readr::read_csv("data/TEST_OPED_MC_WASHINGTONPOST.csv")

fancy_plot <- function(x, ctr = "United States of America", cols = c("gray", "tomato")) {
  
  # Generating ids -------------------------------------------------------------
  ids <- sort(unique(with(x, c(ctry1_rep, ctry2_par)))) %>%
    setdiff(ctr)
  
  ids <- tibble::tibble(
    id   = 1L:length(ids),
    name = ids
  )
  
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
  
  # Creating color palette -----------------------------------------------------
  colfun <- grDevices::colorRamp(cols)
  xran <- x %>%
    group_by(ctry2_par, yr_impl) %>%
    filter(ctry1_rep == ctr) %>%
    summarise(
      count = sum(protec1)
    ) %$% range(count)
  
  # Sent numbers ranges
  max_received <- x %>% group_by(ctry2_par, yr_impl) %>%
    summarise(
      x = sum(protec1)
    ) %$% max(x)
  
  years <- x$yr_impl %>% unique %>% sort
  
  
  # Creating the plot ----------------------------------------------------------
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  par(las = 2, mai = oldpar$mai*c(1,1,0,1))
  plot.new()
  plot.window(xlim = range(years), ylim = range(ids$id))
  
  # Wrapper of xspline
  myxspline <- function(y, from, to, qty, curve = c(.2, .8)) {
    
    coli <- colfun(qty)
    coli <- grDevices::rgb(
      coli[1], coli[2], coli[3],
      maxColorValue = 255,
      alpha = 200
      )
      
    xspline(
      x = c(y, y+.25, y+.5),
      y = c(from, from*curve[1] + to*curve[2] ,to),
      border = coli, shape = -.7, lwd=(qty^(1.5)*max_received)^(1/1.5)
      )
  }
  
  # From US to the world
  for (y in years[-length(years)]) {
  
    # Receiving countries
    z <- dat %>%
      filter(yr_impl == y & ctry1_rep == ctr) %>%
      group_by(ctry2_par) %>%
      summarize(
        received = sum(protec1)
      ) %>%
      filter(received > 0) %>%
      rename(name = ctry2_par) %>%
      dplyr::left_join(ids, by = "name") %>%
      mutate(
        received = coalesce(received, 0)
      )
      
    # Is there any data?
    if (nrow(z) == 0)
      next
    
    # Rescaling
    z$received <- with(z, received/max_received)
    
    for (i in 1:nrow(z)) {
      myxspline(y = y - .5, from = ctr_id, to = z$id[i], qty = z$received[i])
    }
  
  }
  
  # From the world to US 
  for (y in years[-length(years)]) {
    
    # Receiving countries
    z <- dat %>%
      filter(yr_impl == y & ctry2_par == ctr) %>%
      group_by(ctry1_rep) %>%
      summarize(
        received = sum(protec1)
      ) %>%
      filter(received > 0) %>%
      rename(name = ctry1_rep) %>%
      dplyr::left_join(ids, by = "name") %>%
      mutate(
        received = coalesce(received, 0)
      )
    
    # Is there any data?
    if (nrow(z) == 0)
      next
    
    # Rescaling
    z$received <- with(z, received/max_received)
    
    for (i in 1:nrow(z)) {
      myxspline(y, from = z$id[i], to = ctr_id, qty = z$received[i], curve = c(.8, .2))
    }
    
  }
  
  axis(1, labels = years, at = years)
  axis(2, labels = ids$name, at = ids$id, cex.axis=.2, tick = FALSE)
  
  
}

dat %>% 
  filter(yr_impl >= 2013 & yr_impl <= 2018) %>%
  fancy_plot