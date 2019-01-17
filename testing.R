library(tidyverse)
library(tidygraph)
library(ggraph)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/navdata")
library("navdata")

#http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/

data("phone.call2")
phone.net <- tbl_graph(
  nodes = phone.call2$nodes, 
  edges = phone.call2$edges,
  directed = TRUE
)

ggraph(phone.net, layout = "graphopt") + 
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(size = 4, colour = "#00AFBB") +
  geom_node_text(aes(label = label), repel = TRUE)+
  theme_graph()


library(corrr)
res.cor <- mtcars [, c(1, 3:6)] %>%  # (1)
  t() %>% correlate() %>%            # (2)
  shave(upper = TRUE) %>%            # (3)
  stretch(na.rm = TRUE) %>%          # (4)
  filter(r >= 0.998)                 # (5)
res.cor


set.seed(1)
cor.graph <- as_tbl_graph(res.cor, directed = FALSE)
ggraph(cor.graph) + 
  geom_edge_link() + 
  geom_node_point() +
  geom_node_text(
    aes(label = name), size = 3, repel = TRUE
  ) +
  theme_graph()


set.seed(123)
phone.net %>%
  activate(nodes) %>%
  mutate(centrality = centrality_authority()) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  geom_node_text(aes(label = label), repel = TRUE)+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_graph()
