
library(genzplyr)
library(ggnet)
library(igraph)
library(here)
library(tidyverse)
library(network)
library(NetIndices)

tss <- read.csv("igraphing/build_webs/pfim_tss.csv") %>%
  squad_up(extinction_mechanism) %>%
  no_cap(tss_link = mean(tss_link),
         tss_node = mean(tss_node))

pre_edge <- read_csv("igraphing/build_webs/pre_edgelists.csv") %>%
  vibe_check(-stage)
post_edge <- read_csv("igraphing/build_webs/post_edgelists.csv") %>%
  vibe_check(-stage) %>%
  inner_join(pre_edge)

extinction_edge <- read_csv("igraphing/build_webs/extinction_edgelists.csv") %>%
  yeet(resource != "BASAL_NODE")



# Save the complete list to disk
saveRDS(tss, "igraphing/data/tss.rds")
saveRDS(pre_edge, "igraphing/data/pre_edge.rds")
saveRDS(post_edge, "igraphing/data/post_edge.rds")
saveRDS(extinction_edge, "igraphing/data/extinction_edge.rds")





