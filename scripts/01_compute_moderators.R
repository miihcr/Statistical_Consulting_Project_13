# 01_compute_moderators.R


# Function to extract key fit indices
fit_table <- function(fit) {
  data.frame(
    ChiSq = fitMeasures(fit, "chisq"),
    df = fitMeasures(fit, "df"),
    CFI = fitMeasures(fit, "cfi"),
    TLI = fitMeasures(fit, "tli"),
    RMSEA = fitMeasures(fit, "rmsea"),
    SRMR = fitMeasures(fit, "srmr")
  )
}

## --- Social susceptibility ---  ## 

data <- read.csv(here::here("data","raw","data_2.csv"))

# Calculate SUS scores and check reliability
social_anxiety_items = c("SUS_1","SUS_3","SUS_5","SUS_7")
peers_self_esteem_items = c("SUS_2","SUS_4","SUS_6","SUS_8")
all_items = c("SUS_1", "SUS_2", "SUS_3", "SUS_4",
              "SUS_5", "SUS_6", "SUS_7", "SUS_8")

sus_scores = data[, grep("SUS_", names(data))]

### Reliability analysis (internal consistency)

#### Two factors

social_anxiety_rel = alpha(sus_scores[social_anxiety_items], check.keys = FALSE)
alpha_value1 = social_anxiety_rel$total$raw_alpha 
alpha_value1

omega(sus_scores[social_anxiety_items])$omega.tot

self_esteem_rel = alpha(sus_scores[peers_self_esteem_items], check.keys = FALSE)
alpha_value2 = self_esteem_rel$total$raw_alpha
alpha_value2

omega(sus_scores[peers_self_esteem_items])$omega.tot

#### One factor

all_items_rel = alpha(sus_scores[all_items], check.keys = FALSE)
alpha_value3 = all_items_rel$total$raw_alpha # cronbachs alpha for total score
alpha_value3

omega(sus_scores)$omega.tot # omega for total score 


### CFA

# 1-factor model

cfa_1f <- "
  SocialSusceptibility =~ SUS_1 + SUS_2 + SUS_3 + SUS_4 + SUS_5 + SUS_6 + SUS_7 + SUS_8
"

# 2-factor model
cfa_2f <- "
  SocialAnxiety =~ SUS_1 + SUS_3 + SUS_5 + SUS_7
  PeerEsteem    =~ SUS_2 + SUS_4 + SUS_6 + SUS_8
"

fit1 <- cfa(
  cfa_1f,
  data = sus_scores,
  std.lv = TRUE, # factor variances are set to 1 for identification
  missing = "fiml" # missing value handling
)


fit2 <- cfa(
  cfa_2f,
  data = sus_scores,
  std.lv = TRUE,
  missing = "fiml"
)


summary(fit1, fit.measures = TRUE, standardized = TRUE)
summary(fit2, fit.measures = TRUE, standardized = TRUE)


# Fit measures
fit_table(fit1)
fit_table(fit2)


interpret(fit1)
interpret(fit2)



# Removing item 2
cfa_minus2 <- '
  SocialSusceptibility =~ SUS_1 + SUS_3 + SUS_4 + SUS_5 + SUS_6 + SUS_7 + SUS_8
'

fit_minus2 <- cfa(
  cfa_minus2,
  data    = sus_scores,
  std.lv  = TRUE,
  missing = "fiml"
)

interpret(fit_minus2)

summary(fit_minus2, fit.measures = TRUE, standardized = TRUE)
fit_table(fit_minus2)


# Removing item 4
cfa_minus4 <- '
  SocialSusceptibility =~ SUS_1 + SUS_3 + SUS_2 + SUS_5 + SUS_6 + SUS_7 + SUS_8
'

fit_minus4 <- cfa(
  cfa_minus4,
  data    = sus_scores,
  std.lv  = TRUE,
  missing = "fiml"
)

interpret(fit_minus4)

summary(fit_minus4, fit.measures = TRUE, standardized = TRUE)
fit_table(fit_minus4)


## Visualizing CFA models

dir.create("figures/sem-plots", recursive = TRUE, showWarnings = FALSE)


# ONE FACTOR MODEL
png("figures/sem-plots/semplot_onefactor.png", 
    width = 7, height = 5, res = 300,
    units = "in")


semPaths(
  fit1,
  what="std",      
  weighted = FALSE,
  intercepts = FALSE,    
  sizeMan = 6,
  sizeLat = 11,           
  label.cex = 1.1,       
  nCharNodes = 0)   


dev.off()

# TWO FACTOR MODEL 

png("figures/sem-plots/semplot_twofactor.png", width = 7, height = 5, res = 300,
    units = "in")

semPaths(fit2,         
         what="std",      
         weighted = FALSE,
         intercepts = FALSE,    
         sizeMan = 6,
         sizeLat = 11,           
         label.cex = 1.1,       
         nCharNodes = 0)     


dev.off()

# ONE FACTOR MODEL MODEL (WITHOUT ITEM 2)
png("figures/sem-plots/semplot_minus2.png", width = 7, height = 5, res = 300,
    units = "in")

semPaths(fit_minus2,       
         what="std",      
         weighted = FALSE,
         intercepts = FALSE,    
         sizeMan = 6,
         sizeLat = 11,           
         label.cex = 1.1,       
         nCharNodes = 0)    

dev.off()



## --- Social Cohesion (Network) --- ##

data$ppn = as.character(data$ppn)
data$class = as.character(data$class)
data$nom_like = as.character(data$nom_like)

data_sn = subset(data, select = c(ppn, school, class, nom_like))

# Convert comma-separated nominations to list
data_sn$nom_like = strsplit((data_sn$nom_like), ",")

# Expand to long format
edges = unnest(data_sn, cols = c(nom_like))

# Clean up whitespace and remove NA
edges$nom_like = str_trim(edges$nom_like)
edges = subset(edges, nom_like != "" & !is.na(nom_like))

# Create node list (all participants)
nodes = data_sn[, c("ppn", "school", "class")]
colnames(nodes)[1] = "name"   # igraph expects "name" for node IDs

# Rename for clarity
colnames(edges)[colnames(edges) == "nom_like"] = "target"
colnames(edges)[colnames(edges) == "ppn"] = "source"


### Missing id's


# Check per class for nominations not present in ppn
classes = sort(unique(edges$class))

invalid_all = data.frame(name = numeric(), class = numeric(), school = numeric())

for (c in classes) {
  cat("\nClass:", c, "\n")
  
  class_data  = subset(edges, class == c)
  class_nodes = subset(nodes, class == c)
  
  # IDs mentioned but not present
  invalid_ids = setdiff(unique(class_data$target), unique(class_nodes$name))
  
  if (length(invalid_ids) == 0) {
    cat("All nominations in this class refer to existing participants.\n")
  } else {
    cat("Invalid nominations found (does not have ppn in original data):\n")
    print(invalid_ids)
    
    # Store these invalid IDs in a data frame
    temp = data.frame(
      name = invalid_ids,
      class = rep(c, length(invalid_ids)),
      school = unique(class_nodes$school)  # assumes all rows in class_nodes have same school
    )
    invalid_all = rbind(invalid_all, temp)
  }
}

# Combine invalid nodes with the original node list
nodes_extended = rbind(nodes, invalid_all)


### Plots and metrics


# Ensure plotting margins
par(mar = c(1, 1, 2, 1))

# Create an empty results data frame
network_summary = data.frame(
  class = character(),
  density_directed = numeric(),
  density_reciprocal = numeric(),
  reciprocity = numeric(),
  transitivity = numeric(),
  path_length_min = numeric(),
  path_length_max = numeric(),
  path_length_mean = numeric(),
  n_components = numeric(),
  largest_component_size = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each class
for (cls in classes) {
  # Subset edges and nodes for this class
  edges_sub = subset(edges, class == cls)
  nodes_sub = subset(nodes_extended, class == cls)
  
  # Build graph including all nodes
  g = graph_from_data_frame(
    d = edges_sub[, c("source", "target", "class", "school")],
    vertices = nodes_sub,
    directed = TRUE
  )
  
  # Compute network metrics
  dens = round(edge_density(g, loops = FALSE),3) # directed density
  
  # Reciprocated ties density (undirected)
  g_recip = as.undirected(g, mode = "mutual")
  dens_recip = round(edge_density(g_recip, loops = FALSE), 3)
  
  rec = round(reciprocity(g),3) # Reciprocity (only makes sense for directed graphs)
  trans = round(transitivity(g, type = "global"),3) # Transitivity (global clustering coefficient)
  
  # Shortest path lengths
  dists = distances(g)
  dists[is.infinite(dists)] = NA  # ignore disconnected pairs
  path_min = min(dists, na.rm = TRUE)
  path_max = max(dists, na.rm = TRUE)
  path_mean = round(mean(dists, na.rm = TRUE),3)
  
  # Connected components
  comps = components(g, mode = "weak")
  n_comp = comps$no
  largest_comp = max(comps$csize)
  
  # Store results
  network_summary = rbind(network_summary, data.frame(
    class = cls,
    density_directed = dens,
    density_reciprocal = dens_recip,
    reciprocity = rec,
    transitivity = trans,
    path_length_min = path_min,
    path_length_max = path_max,
    path_length_mean = path_mean,
    n_components = n_comp,
    largest_component_size = largest_comp
  ))
  
  # Plot each network
  plot(
    g,
    main = paste("Directed Network for Class", cls),
    edge.width = 0.5,
    vertex.size = 15,
    vertex.label.cex = 0.8,
    layout = layout_with_fr
  )
  
  plot(
    g_recip,
    main = paste("Reciprocal Network for Class", cls),
    edge.width = 0.5,
    vertex.size = 15,
    vertex.label.cex = 0.8,
    layout = layout_with_fr
  )
}

network_summary

### Network descriptives

# Clean nominations column into numeric lists
data_sn$nominations_clean = lapply(data_sn$nom_like, function(x) {
  if (is.null(x) || all(is.na(x))) return(NA)
  as.numeric(unlist(str_extract_all(paste(x, collapse = " "), "\\d+")))
})

# Count number of nominations per subject
data_sn$n_nominations = sapply(data_sn$nominations_clean, function(x) {
  if (all(is.na(x))) 0 else length(x)
})

# Create summary data frame
class_summary = data.frame(
  class = classes,
  n_subjects = NA,
  total_nominations = NA,
  mean_nominations = NA,
  total_reciprocal_nomination = NA
)

# Loop through classes and compute descriptives
for (i in seq_along(classes)) {
  cls = classes[i]
  sub_df = data_sn[data_sn$class == cls, ]
  
  n_subjects = nrow(sub_df)
  total_nominations = sum(sub_df$n_nominations, na.rm = TRUE)
  mean_nominations = mean(sub_df$n_nominations, na.rm = TRUE)
  
  # Get reciprocal edge count from the already-created g_recip graph
  edges_sub = subset(edges, class == cls)
  nodes_sub = subset(nodes_extended, class == cls)
  g = graph_from_data_frame(
    d = edges_sub[, c("source", "target")],
    vertices = nodes_sub,
    directed = TRUE
  )
  g_recip = as.undirected(g, mode = "mutual")
  total_reciprocal = ecount(g_recip)
  
  class_summary[i, "n_subjects"] = n_subjects
  class_summary[i, "total_nominations"] = total_nominations
  class_summary[i, "mean_nominations"] = mean_nominations
  class_summary[i, "total_reciprocal_nomination"] = total_reciprocal
}

# View results
print(class_summary)

network_overview = merge(class_summary, network_summary,
                         by = "class",
                         all = TRUE)

print(network_overview)


### Adjusting original dataset


# Add new column: mean per row (ignore missing values)
data$susceptibility = round(rowMeans(data[, all_items], na.rm = TRUE),2)

# For missing values: Compute class means for susceptibility
class_means = tapply(data$susceptibility, data$class, mean, na.rm = TRUE)

for (i in 1:nrow(data)) {
  if (is.na(data$susceptibility[i])) {
    cls <- data$class[i]
    data$susceptibility[i] <- class_means[as.character(cls)]
  }
}


# Add cohesion (density per class from network_summary)
data = merge(data,
             network_summary[, c("class", "density_directed", "density_reciprocal")],
             by = "class",
             all.x = TRUE)

names(data)[names(data) == "density_reciprocal"] = "cohesion_recip"

names(data)[names(data) == "density_directed"] = "cohesion_directed"

# Center it for the analysis
data$cohesion_recip_c = as.numeric(scale(data$cohesion_recip, center = TRUE, scale = FALSE))

data$cohesion_directed_c = as.numeric(scale(data$cohesion_directed, center = TRUE, scale = FALSE))

data$susceptibility_c = as.numeric(scale(data$susceptibility, center = TRUE, scale = FALSE))

# Save 
saveRDS(
  data,
  here::here("data","processed","data2_incl_moderation.rds")
)

message("Moderators computed and saved.")