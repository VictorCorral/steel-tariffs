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

# Exporting the data to excel --------------------------------------------------

WriteXLS::WriteXLS(
  apply(sent, 3, as.data.frame),
  ExcelFileName = "sent.xls",
  row.names = TRUE
  )


# Creating igraph objects ------------------------------------------------------
library(igraph)

ig_sent <- apply(sent, 3, igraph::graph_from_adjacency_matrix, mode = "directed", weighted = TRUE)

fancy_plot <- function(x) {
  
  set.seed(1)
  Edge  <- E(x)$weight
  Vcol  <- viridisLite::viridis(vcount(x))
  Ecol  <- unname(unlist(Map(
    function(col, a) adjustcolor(col, a),
    col = Vcol[match(as_edgelist(x)[, 1], countries)],
    a   = Edge/max(Edge)
  ), recursive = TRUE))
  
  Vsize <- colSums(as.matrix(as_adj(x, attr = "weight")))
  
  pos   <- layout_in_circle(x)
  
  plot.new()
  plot.window(xlim = range(pos[,1]), ylim = range(pos[,2]))
  Vsize <- netdiffuseR::igraph_vertex_rescale(Vsize, minmax.relative.size = c(.05, .1))
  
  plot(
    x,
    edge.width          = sqrt(Edge),
    layout              = pos,
    edge.curved         = TRUE,
    edge.color          = Ecol,
    vertex.color        = Vcol,
    vertex.frame.color  = Vcol,
    vertex.label.family = "sans",
    vertex.label.color  = "black",
    vertex.label.cex    = 1.25,
    vertex.size         = Vsize,
    edge.arrow.mode     = "-",
    vertex.label.dist   = 3,
    vertex.label.degree = atan2(-pos[,2], pos[,1]),
    rescale = FALSE,
    add = TRUE
    )
  
}

# Creating the fancy plots -----------------------------------------------------

graphics.off()
tiff(filename = "networks.tiff", width = 1024, height = 1024)
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(3, 3), mai = rep(.5, 4), xpd=NA, oma = c(3,0,3,0))
for (s in names(ig_sent[-c(1:2)])) {
  fancy_plot(ig_sent[[s]])
  title(main = s, cex = 1.25)
}
  
par(mfrow = c(1, 1), mai=rep(0, 4), oma = c(3, 0, 2.5, 0))
box()
mtext("Protectionist networks", 3, cex=1.5, outer=TRUE)
mtext(
  "Note: Edge width by number of protectionist policies, and colors by country of origin",
  1,
  outer = TRUE,
  cex=1
  )
par(oldpar)
dev.off()

