library(readr)
library(dplyr)
library(magrittr)

dat <- readr::read_csv("data/TEST_OPED_MC_WASHINGTONPOST.csv")

fancy_plot <- function(x, ctr = "United States of America", cols = c("gray", "tomato")) {
  
  # Generating ids -------------------------------------------------------------
  ids <- sort(unique(with(x, c(ctry1_rep, ctry2_par))), decreasing = TRUE) %>%
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
  par(las = 2, mai = oldpar$mai*c(1,1.5,0,1))
  plot.new()
  plot.window(xlim = range(years), ylim = range(ids$id))
  
  # Maxi height
  MAX_H <- nrow(ids)/10
  
  # Wrapper of xspline
  myxspline <- function(year, from, to, qty_from, qty_to, col, curve = c(.2, .8)) {
    
    # coli <- colfun(qty_to)
    # coli <- grDevices::rgb(
    #   coli[1], coli[2], coli[3],
    #   maxColorValue = 255,
    #   alpha = 70
    #   )
      
    qty_from <- qty_from*MAX_H
    qty_to   <- qty_to*MAX_H
    
    if (!qty_from & qty_to)
      return(NULL)
    
    xspline(
      x     = c(
        year,
        year + .5,
        year + 1,
        year + 1,
        year + .5,
        year),
      y     = c(
        from + qty_from/2,
        (from + qty_from/2)*curve[1] + (to + qty_to/2)*curve[2],
        to + qty_to/2,
        to - qty_to/2, (from - qty_from/2)*curve[1] + (to - qty_to/2)*curve[2],
        from - qty_from/2
        ),
      col    = grDevices::adjustcolor(col, 1-abs(qty_to - qty_from)/1.25),
      shape  = c(0, -.7, 0, 0, .7, 0),
      border = NA,
      open   = FALSE
      # lwd    = (qty^(1.5)*max_received)^(1/1.5)
      )
  }
  
  for (i in ids$id)
    abline(h = i, col = adjustcolor(i, .3), lwd=1, lty=2)
  
  # From US to the world
  for (y in years[-length(years)]) {
  
    # Receiving countries
    z_from <- x %>%
      filter(yr_impl == y & ctry1_rep == ctr) %>%
      group_by(ctry2_par) %>%
      summarize(
        sent = sum(protec1)
      ) %>%
      filter(sent > 0) %>%
      rename(name = ctry2_par) %>%
      dplyr::left_join(ids, by = "name") %>%
      transmute(
        country = id,
        from    = coalesce(sent, 0L)/max_received
      )
      
    
    # Receiving countries
    z_to <- x %>%
      filter(yr_impl == (y + 1L) & ctry2_par == ctr) %>%
      group_by(ctry1_rep) %>%
      summarize(
        received = sum(protec1)
      ) %>%
      filter(received > 0) %>%
      rename(name = ctry1_rep) %>%
      dplyr::left_join(ids, by = "name") %>%
      transmute(
        country  = id,
        to       = coalesce(received, 0L)/max_received
      )
    
    # Is there any data?
    if (nrow(z_to) == 0)
      next
    
    # merging
    d <- full_join(z_from, z_to, by = "country") %>%
      mutate(
        from = coalesce(from, 0),
        to   = coalesce(to, 0)
      )
    
    for (i in 1:nrow(d)) {
      # myxspline(y = y, from = z$id[i], to = ctr_id, qty = z$received[i], curve = c(.8, .2))
      myxspline(
        year = y,
        from = d$country[i],
        to   = d$country[i],
        qty_from = d$from[i],
        qty_to   = d$to[i],
        curve = c(.8, .2),
        col = d$country[i]
        )
    }
    
  }
  
  axis(1, labels = years, at = years)
  axis(2, labels = ids$name, at = ids$id, cex.axis=.5, tick = FALSE)
  
  
}

dat %>% 
  filter(yr_impl >= 2014, yr_impl <= 2017, protec1 >= 20) %>%
  fancy_plot
