# Reusable ecological logic. Keep functions here independent of Shiny UI.

get_species <- function(community) {
  if (is.null(community)) return(character())
  cheddar::NPS(community)$node
}

remove_species_cascade <- function(community, species) {
  before <- get_species(community)

  if (length(before) == 0L) {
    return(list(
      community = NULL,
      primary = character(),
      secondary = character()
    ))
  }

  if (length(species) != 1L || !species %in% before) {
    stop("Please select a species that exists in the current food web.")
  }

  # Handle the final species separately to avoid cheddar edge cases.
  if (length(before) == 1L) {
    return(list(
      community = NULL,
      primary = species,
      secondary = character()
    ))
  }

  updated <- tryCatch(
    cheddar::RemoveNodes(community, remove = species, method = "cascade"),
    error = function(e) {
      # Do not silently treat every model error as total extinction.
      stop("The extinction cascade could not be calculated: ",
           conditionMessage(e), call. = FALSE)
    }
  )

  after <- get_species(updated)
  disappeared <- setdiff(before, after)

  list(
    community = if (length(after) == 0L) NULL else updated,
    primary = species,
    secondary = setdiff(disappeared, species)
  )
}
