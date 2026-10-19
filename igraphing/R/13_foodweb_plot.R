# ============================================================
# FOOD WEB IMAGE LOADING AND PLOTTING HELPERS
# Preserves image proportions and trophic-level orientation.
# ============================================================

load_species_images <- function(species, image_dir = "www/pics") {
  
  imgs <- lapply(species, function(sp) {
    
    path <- file.path(image_dir, paste0(sp, ".png"))
    
    if (!file.exists(path)) {
      return(NULL)
    }
    
    img <- png::readPNG(path)
    
    # Convert grayscale images to RGBA.
    if (is.matrix(img)) {
      rgba <- array(1, dim = c(nrow(img), ncol(img), 4))
      for (ch in 1:3) {
        rgba[, , ch] <- img
      }
      return(rgba)
    }
    
    channels <- dim(img)[3]
    
    # Convert grayscale + alpha to RGBA.
    if (channels == 2) {
      rgba <- array(1, dim = c(dim(img)[1], dim(img)[2], 4))
      
      for (ch in 1:3) {
        rgba[, , ch] <- img[, , 1]
      }
      
      rgba[, , 4] <- img[, , 2]
      return(rgba)
    }
    
    # Convert RGB to RGBA.
    if (channels == 3) {
      rgba <- array(1, dim = c(dim(img)[1], dim(img)[2], 4))
      rgba[, , 1:3] <- img
      return(rgba)
    }
    
    img
  })
  
  names(imgs) <- species
  imgs
}


# ============================================================
# PLOT FOOD WEB
# ============================================================

plot_foodweb <- function(
    community,
    img_list = list(),
    main = "Food Web Cascade",
    image_width_in = 0.65,
    image_max_height_in = 0.85
) {
  
  # ----------------------------------------------------------
  # Handle extinction.
  # ----------------------------------------------------------
  
  if (is.null(community)) {
    plot.new()
    title(main)
    text(0.5, 0.5, "Entire ecosystem extinct", cex = 1.4)
    return(invisible(NULL))
  }
  
  species <- get_species(community)
  
  if (length(species) == 0L) {
    plot.new()
    title(main)
    text(0.5, 0.5, "All species extinct", cex = 1.4)
    return(invisible(NULL))
  }
  
  # ----------------------------------------------------------
  # Build the food-web graph.
  # ----------------------------------------------------------
  
  predation <- cheddar::PredationMatrix(community)
  
  graph <- igraph::graph_from_adjacency_matrix(
    predation,
    mode = "directed",
    diag = FALSE
  )
  
  if (igraph::vcount(graph) != length(species)) {
    plot.new()
    title(main)
    text(0.5, 0.5, "Could not align network nodes", cex = 1.1)
    return(invisible(NULL))
  }
  
  igraph::V(graph)$name <- species
  
  # ----------------------------------------------------------
  # Generate a network layout.
  # ----------------------------------------------------------
  
  layout <- tryCatch(
    igraph::layout_with_sugiyama(graph)$layout,
    error = function(e) NULL
  )
  
  if (is.null(layout) ||
      nrow(layout) != length(species) ||
      any(!is.finite(layout))) {
    
    layout <- igraph::layout_nicely(graph)
  }
  
  # ----------------------------------------------------------
  # Use trophic levels for vertical placement.
  # Higher trophic levels should appear at the TOP.
  # ----------------------------------------------------------
  
  tl <- tryCatch(
    cheddar::TrophicLevels(community),
    error = function(e) NULL
  )
  
  if (!is.null(tl) && !is.null(names(tl))) {
    
    matched <- match(species, names(tl))
    
    if (all(!is.na(matched))) {
      
      trophic_y <- as.numeric(tl[matched])
      
      if (all(is.finite(trophic_y))) {
        layout[, 2] <- trophic_y
      }
    }
  }
  
  # ----------------------------------------------------------
  # Normalize node coordinates.
  # ----------------------------------------------------------
  
  x_range <- range(layout[, 1], finite = TRUE)
  y_range <- range(layout[, 2], finite = TRUE)
  
  if (diff(x_range) > 0) {
    
    layout[, 1] <- 0.08 +
      (layout[, 1] - x_range[1]) /
      diff(x_range) * 0.84
    
  } else {
    
    layout[, 1] <- 0.50
  }
  
  # Flip the vertical positions so higher trophic levels are at the top.
  if (diff(y_range) > 0) {
    layout[, 2] <- 0.85 -
      (layout[, 2] - y_range[1]) /
      diff(y_range) * 0.70
  } else {
    layout[, 2] <- 0.50
  }
  
  # ----------------------------------------------------------
  # Preserve plotting parameters.
  # ----------------------------------------------------------
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  par(
    mar = c(4, 3, 5, 3),
    xpd = FALSE
  )
  
  # ----------------------------------------------------------
  # Create one plotting region.
  # ----------------------------------------------------------
  
  plot(
    NA,
    xlim = c(0, 1),
    ylim = c(0, 1),
    xlab = "",
    ylab = "",
    axes = FALSE,
    asp = 1,
    main = main,
    xaxs = "i",
    yaxs = "i"
  )
  
  # Actual physical plot dimensions and user-coordinate ranges.
  # These are used to convert inches into plotting coordinates.
  usr <- par("usr")
  pin <- par("pin")
  
  x_units_per_inch <- diff(usr[1:2]) / pin[1]
  y_units_per_inch <- diff(usr[3:4]) / pin[2]
  
  # ----------------------------------------------------------
  # Draw directed food-web edges.
  # ----------------------------------------------------------
  
  edges <- igraph::as_edgelist(graph, names = TRUE)
  
  if (nrow(edges) > 0L) {
    
    for (i in seq_len(nrow(edges))) {
      
      from <- match(edges[i, 1], species)
      to   <- match(edges[i, 2], species)
      
      if (!is.na(from) && !is.na(to)) {
        
        arrows(
          layout[from, 1],
          layout[from, 2],
          layout[to, 1],
          layout[to, 2],
          col = "gray70",
          length = 0.08,
          lwd = 1.5
        )
      }
    }
  }
  
  # ----------------------------------------------------------
  # Draw images without stretching or squashing.
  # Image sizes are calculated in physical inches first.
  # ----------------------------------------------------------
  
  for (i in seq_along(species)) {
    
    sp <- species[i]
    img <- img_list[[sp]]
    
    x <- layout[i, 1]
    y <- layout[i, 2]
    
    if (!is.null(img)) {
      
      img_h <- dim(img)[1]
      img_w <- dim(img)[2]
      
      # Original pixel aspect ratio.
      aspect <- img_h / img_w
      
      # Choose physical dimensions while preserving proportions.
      draw_width_in <- min(
        image_width_in,
        image_max_height_in / aspect
      )
      
      draw_height_in <- draw_width_in * aspect
      
      # Convert physical dimensions to plot coordinates.
      width <- draw_width_in * x_units_per_inch
      height <- draw_height_in * y_units_per_inch
      
      # Keep the complete image within the plot region.
      x <- min(max(x, usr[1] + width / 2),
               usr[2] - width / 2)
      
      y <- min(max(y, usr[3] + height / 2),
               usr[4] - height / 2)
      
      rasterImage(
        img,
        x - width / 2,
        y - height / 2,
        x + width / 2,
        y + height / 2,
        interpolate = TRUE
      )
      
      # Place the label below the image.
      label_y <- y - height / 2 -
        0.025 * y_units_per_inch
      
      text(
        x,
        max(usr[3] + 0.02 * diff(usr[3:4]), label_y),
        labels = sp,
        font = 2,
        cex = 0.75
      )
      
    } else {
      
      # Fallback when an image is missing.
      points(
        x, y,
        pch = 21,
        bg = "white",
        cex = 2.8
      )
      
      text(
        x,
        max(usr[3] + 0.02 * diff(usr[3:4]),
            y - 0.045 * diff(usr[3:4])),
        labels = sp,
        font = 2,
        cex = 0.8
      )
    }
  }
  
  invisible(NULL)
}