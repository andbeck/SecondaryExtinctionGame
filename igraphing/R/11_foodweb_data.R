# Initial food-web definition and cheddar Community object.
# Keep species names exactly aligned with files in www/pics/ if using images.

foodweb_graph <- igraph::make_graph(c(
  # Apex level
  "Tyrannosaurus rex", "Allosaurus",
  "Tyrannosaurus rex", "Velociraptor",

  # Secondary -> Primary
  "Velociraptor", "Triceratops",

  "Allosaurus", "Stegosaurus",
  "Allosaurus", "Triceratops",

  # Primary -> Producers
  "Stegosaurus", "Cycads",
  "Stegosaurus", "Ferns",
  "Triceratops", "Ferns"
), directed = TRUE)

foodweb_nodes <- data.frame(node = igraph::V(foodweb_graph)$name)
foodweb_edges <- igraph::as_data_frame(foodweb_graph, what = "edges")
names(foodweb_edges) <- c("consumer", "resource")

initial_community <- cheddar::Community(
  properties = list(title = "foodweb"),
  nodes = foodweb_nodes,
  trophic.links = foodweb_edges
)
