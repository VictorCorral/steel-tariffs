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
  
  # Creating color palette -----------------------------------------------------
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
  par(las = 2, mai = oldpar$mai*c(1.5,2.5,0,1))
  plot.new()
  plot.window(xlim = range(years), ylim = range(ids$id))
  
  # Maxi height
  MAX_H <- 1
  
  # Wrapper of xspline
  myxspline <- function(year, from, to, qty_from, qty_to, col, curve = c(.5, .5)) {

    
    if (!qty_from & qty_to)
      return(NULL)
    
    # Readjusting quantities
    qty <- qty_from + qty_from
    qty_from <- qty_from/qty
    qty_to   <- qty_to/qty
    curve    <- curve/sum(curve)

    # Adjusting the color
    colt <- grDevices::adjustcolor(col, 1-abs(qty_to - qty_from)/(qty_from + qty_to))
    
    xspline(
      x     = c(
        year,
        year + 0.5,
        year + 1.0,
        year + 1.0,
        year + 0.5,
        year),
      y     = c(
        from + qty_from/2,
        (from + qty_from/20)*curve[1] + (to + qty_to/20)*curve[2],
        to + qty_to/2,
        to - qty_to/2,
        (from - qty_from/20)*curve[1] + (to - qty_to/20)*curve[2],
        from - qty_from/2
        ),
      col    = colt,
      shape  = c(0, -.9, 0, 0, .9, 0),
      border = adjustcolor(col, .5),
      open   = FALSE,
      lwd    = 0.5
      )
  }
  
  # Colors
  cols <- viridis::viridis(nrow(ids))
  
  # Adding a grid
  for (i in ids$id)
    symbols(x = mean(years), y = i, rectangles = cbind(max(years) - min(years), 1),
            bg = adjustcolor(cols[i], .3), add=TRUE, inches = FALSE,
            fg = "white"
            )
    
  
  
  # From US to the world
  for (y in years[-length(years)]) {
  
    # Receiving countries
    z_from <- x %>%
      filter(yr_impl == y & ctry1_rep == ctr) %>%
      rename(
        sent = protec1,
        name = ctry2_par
        ) %>%
      filter(sent > 0) %>%
      dplyr::left_join(ids, by = "name") %>%
      transmute(
        country = id,
        from    = coalesce(sent, 0L)/max_received
      )
      
    
    # Receiving countries
    z_to <- x %>%
      filter(yr_impl == (y ) & ctry2_par == ctr) %>%
      rename(
        received = protec1,
        name = ctry1_rep
        ) %>%
      filter(received > 0) %>%
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
      myxspline(
        year = y,
        from = d$country[i],
        to   = d$country[i],
        qty_from = d$from[i],
        qty_to   = d$to[i],
        curve = c(.5, .5),
        col = cols[d$country[i]]
        )
    }
    
  }
  
  # Adding axis
  axis(1, labels = years, at = years, tick = FALSE, line = -1.5, cex.axis = .7)
  axis(2, labels = ids$name, at = ids$id, cex.axis=.5, tick = FALSE, line = -1.5)
  
  par(mai = rep(0, 4))
  plot.window(xlim=c(0,3), ylim=c(0,15))
  myxspline(
    year = 1,
    from = 1,
    to   = 1,
    qty_from = 2,
    qty_to = 2,
    col = "lightgray"
  )
  text(x = 1.5, y = 1.5, labels = "Policy flows", cex = .75, font=2)
  text(
    x = .9, y = 1,
    labels = sprintf("Relative number of\nprotectionist policies from\n%s", ctr),
    pos = 2, cex = .75)
  text(x = 1, y = 1, labels = "[", pos = 2, cex=2)
  text(
    x = 2.1, y = 1,
    labels = sprintf("Relative number of\nprotectionist policies sent to\n%s", ctr),
    pos = 4, cex = .75)
  text(x = 2, y = 1, labels = "]", pos = 4, cex=2)
  text(
    x = 1.5, y = .5, pos = 1,
    labels = "Transparency porportional to\nlack of reciprocity", cex=.75, col = gray(.1)
  )

}

ids <- dat %$% c(ctry1_rep, ctry2_par) %>%
  unique %>% sort 

ids <- tibble::tibble(
  id = seq_along(ids),
  name = ids
)


for (i in 0:3) {
  group1 <- ids[(1:56) + 56*i,]
  group1 <- as.vector(group1$name)
  
  graphics.off()
  pdf(sprintf("plot%02i.pdf", i))
  
  dat %>% 
    filter(
      (ctry1_rep == "United States of America" & ctry2_par %in% group1) |
      (ctry1_rep %in% group1 & ctry2_par == "United States of America")
      ) %>%
    fancy_plot
  
  dev.off()
}