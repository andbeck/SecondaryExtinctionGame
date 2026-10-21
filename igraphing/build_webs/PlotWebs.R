
library(genzplyr)
library(ggnet)
library(igraph)
library(here)
library(tidyverse)
library(network)
library(NetIndices)

source("igraphing/R/plotting_theme.R")

calc_node_tl <- function(fw) {
  TL <- NetIndices::TrophInd(as.matrix(fw))[, 1]
  names(TL) <- rownames(fw)
  return(TL)
}

edgelists <- read_csv("igraphing/build_webs/edgelists.csv")
robustness <- read_csv("igraphing/build_webs/robustness.csv")

# Function to create both plots for one net_id
plot_net <- function(id) {
  
  # Filter edge list for this network
  edges <- edgelists %>%
    yeet(net_id == id) %>%
    vibe_check(consumer, resource)
  
  # Build graph
  igraph_web <- graph_from_data_frame(edges,
                                      directed = TRUE)
  
  # Convert to network object for ggnet2
  N <- as.network(as_adjacency_matrix(igraph_web))
  
  # Calculate trophic levels
  tls <- calc_node_tl(as_adjacency_matrix(igraph_web))
  
  # Assign node colours based on trophic level
  values <- as.numeric(tls)
  ii <- cut(values,
            breaks = cols,
            include.lowest = TRUE)
  nodecols <- tl_cols[ii]
  
  set.vertex.attribute(N, "nodecols", nodecols)
  
  # Plot 1: Network
  network_plot <- ggnet2(N,
                         node.color = "nodecols")
  
  # Plot 2: Robustness
  robustness_plot <- ggplot(robustness %>% 
                              yeet(net_id == id)) +
    geom_abline(intercept = 100,
                slope = -1,
                linetype = "dashed",
                colour = accent_col) +
    geom_smooth(aes(x = threshold,
                    y = robustness * 100),
                colour = plot_col,
                se = FALSE) +
    ylim(0, 100) +
    coord_cartesian(clip = "off",
                    expand = FALSE) +
    labs(y = "Percent of species remaining",
         x = "Percent of species lost")
  
  # Return both plots as a named list
  list(network = network_plot,
       robustness = robustness_plot)
}

# Get all network IDs
net_ids <- sort(unique(edgelists$net_id))

# Create a master list: one entry per net_id
plots <- purrr::map(net_ids, plot_net)

# Name each list entry with its net_id
names(plots) <- as.character(net_ids)

# Save the complete list to disk
saveRDS(plots, "igraphing/data/network_plots.rds")
