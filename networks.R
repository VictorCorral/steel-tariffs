library(readr)
library(dplyr)
library(magrittr)

dat <- readr::read_csv("data/TEST_OPED_MC_WASHINGTONPOST.csv")

countries <- sort(
  c("United States of America", "Russia", "China", "Canada", "Germany", "France", "United Kingdom")
)

# Preparing the data
dat %<>%
  filter(ctry1_rep %in% countries, ctry2_par %in% countries) %>%
  transmute(
    ego   = ctry1_rep,
    alter = ctry2_par,
    year  = as.character(yr_impl),
    sent  = protec1
    )

# Creating the received
dat %<>% rename(
  ego   = alter,
  alter = ego,
  received = sent
) %>%
  left_join(
    dat
  ) 

# Making space
sent <- array(
  dim = c(length(countries), length(countries), length(unique(dat$year))),
  dimnames = list(countries, countries, sort(unique(dat$year))))

sent_lagged <- sent

# Sent data
sent[as.matrix(dat[,1:3])] <- dat$sent
sent[is.na(sent)] <- 0

# Sent lagged
dat_lagged <- dat %>%
  arrange(ego, alter, year) %>%
  group_by(ego, alter) %>%
  mutate(
    sent = lag(sent)
  ) %>% ungroup

sent_lagged[as.matrix(dat_lagged[,1:3])] <- dat_lagged$sent
sent_lagged[is.na(sent_lagged)] <- 0

# Creating igraph objects
library(igraph)

sent <- apply(sent, 3, igraph::graph_from_adjacency_matrix, mode = "directed", weighted = TRUE)

fancy_plot <- function(x) {
  
  set.seed(1)
  Edge  <- E(x)$weight
  Vcol  <- viridis::viridis(vcount(x), alpha = .7)
  Ecol  <- Vcol[match(as_edgelist(x)[, 1], countries)]
  Vsize <- colSums(as.matrix(as_adj(x, attr = "weight")))
  
  pos   <- layout_in_circle(x)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mai = rep(.5, 4), xpd = NA)
  plot.new()
  plot.window(xlim = range(pos[,1]), ylim = range(pos[,2]))
  
  plot(
    x,
    edge.width          = sqrt(Edge),
    layout              = pos,
    edge.curved         = TRUE,
    edge.color          = Ecol,
    vertex.color        = Vcol,
    vertex.frame.color  = adjustcolor(Vcol, alpha.f = 2),
    vertex.label.family = "sans",
    vertex.label.color  = "black",
    vertex.size         = netdiffuseR::igraph_vertex_rescale(Vsize, minmax.relative.size = c(.05, .1)),
    edge.arrow.mode     = "-",
    rescale = FALSE,
    add = TRUE
    )
  
}

fancy_plot(sent$`2015`)
