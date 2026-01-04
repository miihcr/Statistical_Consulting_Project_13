# moderators.R

# contains:
# compute_susceptibility
# compute_cohesion
# run_cfa
# plot_sem


fit_table <- function(fit) {
  data.frame(
    ChiSq = lavaan::fitMeasures(fit, "chisq"),
    df    = lavaan::fitMeasures(fit, "df"),
    CFI   = lavaan::fitMeasures(fit, "cfi"),
    TLI   = lavaan::fitMeasures(fit, "tli"),
    RMSEA = lavaan::fitMeasures(fit, "rmsea"),
    SRMR  = lavaan::fitMeasures(fit, "srmr")
  )
}

compute_sus_reliability <- function(df, items_all, items_social_anx, items_peer_esteem) {
  sus_scores <- df |>
    dplyr::select(dplyr::all_of(items_all)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  
  a_social <- psych::alpha(sus_scores[, items_social_anx], check.keys = FALSE)$total$raw_alpha
  o_social <- psych::omega(sus_scores[, items_social_anx], plot = FALSE)$omega.tot
  
  a_peer <- psych::alpha(sus_scores[, items_peer_esteem], check.keys = FALSE)$total$raw_alpha
  o_peer <- psych::omega(sus_scores[, items_peer_esteem], plot = FALSE)$omega.tot
  
  a_all <- psych::alpha(sus_scores[, items_all], check.keys = FALSE)$total$raw_alpha
  o_all <- psych::omega(sus_scores[, items_all], plot = FALSE)$omega.tot
  
  rel <- tibble::tibble(
    scale = c("SocialAnxiety", "PeerEsteem", "Total"),
    alpha = c(a_social, a_peer, a_all),
    omega = c(o_social, o_peer, o_all)
  )
  
  list(sus_scores = sus_scores, reliability = rel)
}

run_cfa_models <- function(sus_scores) {
  cfa_1f <- "
    SocialSusceptibility =~ SUS_1 + SUS_2 + SUS_3 + SUS_4 + SUS_5 + SUS_6 + SUS_7 + SUS_8
  "
  cfa_2f <- "
    SocialAnxiety =~ SUS_1 + SUS_3 + SUS_5 + SUS_7
    PeerEsteem    =~ SUS_2 + SUS_4 + SUS_6 + SUS_8
  "
  cfa_minus2 <- "
    SocialSusceptibility =~ SUS_1 + SUS_3 + SUS_4 + SUS_5 + SUS_6 + SUS_7 + SUS_8
  "
  cfa_minus4 <- "
    SocialSusceptibility =~ SUS_1 + SUS_2 + SUS_3 + SUS_5 + SUS_6 + SUS_7 + SUS_8
  "
  
  specs <- list(
    one_factor = cfa_1f,
    two_factor = cfa_2f,
    minus_item2 = cfa_minus2,
    minus_item4 = cfa_minus4
  )
  
  fits <- purrr::imap(specs, ~ lavaan::cfa(.x, data = sus_scores, std.lv = TRUE, missing = "fiml"))
  fit_indices <- purrr::imap_dfr(fits, ~ cbind(model = .y, fit_table(.x)))
  
  list(fits = fits, fit_indices = fit_indices)
}

plot_sem_png <- function(fit, filename, width = 7, height = 5, dpi = 300) {
  # semPlot uses base plotting; save via png device
  grDevices::png(filename, width = width, height = height, res = dpi, units = "in")
  semPlot::semPaths(
    fit,
    what = "std",
    weighted = FALSE,
    nCharNodes = 0
  )
  grDevices::dev.off()
  invisible(filename)
}

build_nomination_edges <- function(df) {
  df2 <- df |>
    dplyr::mutate(
      ppn = as.character(ppn),
      class = as.character(class),
      school = as.character(school),
      nom_like = as.character(nom_like)
    ) |>
    dplyr::select(ppn, school, class, nom_like) |>
    dplyr::mutate(nom_like = strsplit(nom_like, ","))
  
  edges <- tidyr::unnest(df2, cols = c(nom_like)) |>
    dplyr::mutate(nom_like = stringr::str_trim(nom_like)) |>
    dplyr::filter(!is.na(nom_like), nom_like != "") |>
    dplyr::rename(source = ppn, target = nom_like)
  
  nodes <- df2 |>
    dplyr::select(ppn, school, class) |>
    dplyr::rename(name = ppn)
  
  list(data_sn = df2, edges = edges, nodes = nodes)
}

find_invalid_ids <- function(edges, nodes) {
  classes <- sort(unique(nodes$class))
  invalid_all <- data.frame(name = character(), class = character(), school = character())
  
  for (cls in classes) {
    class_edges <- subset(edges, class == cls)
    class_nodes <- subset(nodes, class == cls)
    
    invalid_ids <- setdiff(unique(class_edges$target), unique(class_nodes$name))
    if (length(invalid_ids) > 0) {
      temp <- data.frame(
        name   = invalid_ids,
        class  = rep(cls, length(invalid_ids)),
        school = rep(unique(class_nodes$school)[1], length(invalid_ids))
      )
      invalid_all <- rbind(invalid_all, temp)
    }
  }
  invalid_all
}

compute_network_summary <- function(data_sn, edges, nodes_extended, max_outdegree = 5) {
  classes <- sort(unique(nodes_extended$class))
  
  out <- tibble::tibble(
    class = character(),
    n_nodes = numeric(),
    n_edges = numeric(),
    n_nominators = numeric(),
    cohesion_capped = numeric(),
    avg_outdegree = numeric(),
    density_directed_raw = numeric(),
    reciprocity = numeric(),
    transitivity = numeric(),
    n_components = numeric(),
    largest_component_size = numeric()
  )
  
  for (cls in classes) {
    edges_sub <- subset(edges, class == cls)
    nodes_sub <- subset(nodes_extended, class == cls)
    
    g <- igraph::graph_from_data_frame(
      d = edges_sub[, c("source", "target")],
      vertices = nodes_sub,
      directed = TRUE
    )
    
    n_nodes <- igraph::vcount(g)
    n_edges <- igraph::ecount(g)
    n_nominators <- nrow(subset(data_sn, class == cls))
    
    dens_raw <- igraph::edge_density(g, loops = FALSE)
    avg_outdeg <- ifelse(n_nominators > 0, n_edges / n_nominators, NA)
    capped_den <- ifelse(n_nominators > 0, avg_outdeg / max_outdegree, NA)
    
    rec <- igraph::reciprocity(g)
    trans <- igraph::transitivity(g, type = "global")
    
    comps <- igraph::components(g, mode = "weak")
    n_comp <- comps$no
    largest_comp <- max(comps$csize)
    
    out <- dplyr::bind_rows(out, tibble::tibble(
      class = cls,
      n_nodes = n_nodes,
      n_edges = n_edges,
      n_nominators = n_nominators,
      avg_outdegree = round(avg_outdeg, 3),
      cohesion_capped = round(capped_den, 3),
      density_directed_raw = round(dens_raw, 3),
      reciprocity = round(rec, 3),
      transitivity = round(trans, 3),
      n_components = n_comp,
      largest_component_size = largest_comp
    ))
  }
  
  out
}

add_moderators_to_data <- function(df_raw, network_summary, sus_items_all) {
  df <- df_raw
  
  # susceptibility score: mean of items
  df$susceptibility <- round(rowMeans(df[, sus_items_all], na.rm = TRUE), 2)
  
  # Impute missing susceptibility by class mean
  class_means <- tapply(df$susceptibility, df$class, mean, na.rm = TRUE)
  for (i in seq_len(nrow(df))) {
    if (is.na(df$susceptibility[i])) {
      cls <- as.character(df$class[i])
      df$susceptibility[i] <- class_means[cls]
    }
  }
  
  # Merge cohesion into main dataset by class
  df <- merge(
    df,
    network_summary[, c("class", "cohesion_capped", "avg_outdegree")],
    by = "class",
    all.x = TRUE
  )
  
  # Center variables
  df$avg_outdegree_c   <- as.numeric(scale(df$avg_outdegree, center = TRUE, scale = FALSE))
  df$cohesion_capped_c <- as.numeric(scale(df$cohesion_capped, center = TRUE, scale = FALSE))
  df$susceptibility_c  <- as.numeric(scale(df$susceptibility, center = TRUE, scale = FALSE))
  
  df
}

compute_moderators_pipeline <- function(df_raw) {
  items_social_anx <- c("SUS_1","SUS_3","SUS_5","SUS_7")
  items_peer_esteem <- c("SUS_2","SUS_4","SUS_6","SUS_8")
  items_all <- c("SUS_1","SUS_2","SUS_3","SUS_4","SUS_5","SUS_6","SUS_7","SUS_8")
  
  rel_out <- compute_sus_reliability(df_raw, items_all, items_social_anx, items_peer_esteem)
  cfa_out <- run_cfa_models(rel_out$sus_scores)
  
  net <- build_nomination_edges(df_raw)
  invalid <- find_invalid_ids(net$edges, net$nodes)
  nodes_extended <- dplyr::bind_rows(net$nodes, invalid)
  network_summary <- compute_network_summary(net$data_sn, net$edges, nodes_extended, max_outdegree = 5)
  
  df_with_mods <- add_moderators_to_data(df_raw, network_summary, items_all)
  
  list(
    data = df_with_mods,
    sus_reliability = rel_out$reliability,
    cfa_fits = cfa_out$fits,
    cfa_fit_indices = cfa_out$fit_indices,
    network_summary = network_summary
  )
}