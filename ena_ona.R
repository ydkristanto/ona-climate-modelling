library(tidyverse)
library(readxl)
library(rENA)
library(tma)
library(ona)

# Data preparation ----
# Import data
raw_data <- read_excel(
  "coding/20240926_transformed_coded.xlsx"
)

# Transform data
data_wide <- raw_data |> 
  filter(
    scene == "cps",
    !is.na(subject)
  ) |> 
  select(-start_time, -end_time, -end_time_ms) |> 
  mutate(
    C.B.1 = if_else(str_detect(code, "\\bC\\.B\\.1\\b"), 1, 0, missing = 0),
    C.B.2 = if_else(str_detect(code, "\\bC\\.B\\.2\\b"), 1, 0, missing = 0),
    C.B.3 = if_else(str_detect(code, "\\bC\\.B\\.3\\b"), 1, 0, missing = 0),
    C.B.4 = if_else(str_detect(code, "\\bC\\.B\\.4\\b"), 1, 0, missing = 0),
    C.M.1 = if_else(str_detect(code, "\\bC\\.M\\.1\\b"), 1, 0, missing = 0),
    C.M.2 = if_else(str_detect(code, "\\bC\\.M\\.2\\b"), 1, 0, missing = 0),
    C.M.3 = if_else(str_detect(code, "\\bC\\.M\\.3\\b"), 1, 0, missing = 0),
    C.R.1 = if_else(str_detect(code, "\\bC\\.R\\.1\\b"), 1, 0, missing = 0),
    C.R.2 = if_else(str_detect(code, "\\bC\\.R\\.2\\b"), 1, 0, missing = 0),
    C.R.3 = if_else(str_detect(code, "\\bC\\.R\\.3\\b"), 1, 0, missing = 0),
    C.R.4 = if_else(str_detect(code, "\\bC\\.R\\.4\\b"), 1, 0, missing = 0),
    M.S.1 = if_else(str_detect(code, "\\bM\\.S\\.1\\b"), 1, 0, missing = 0),
    M.S.2 = if_else(str_detect(code, "\\bM\\.S\\.2\\b"), 1, 0, missing = 0),
    M.S.3 = if_else(str_detect(code, "\\bM\\.S\\.3\\b"), 1, 0, missing = 0),
    M.R.1 = if_else(str_detect(code, "\\bM\\.R\\.1\\b"), 1, 0, missing = 0),
    M.R.2 = if_else(str_detect(code, "\\bM\\.R\\.2\\b"), 1, 0, missing = 0),
    M.R.3 = if_else(str_detect(code, "\\bM\\.R\\.3\\b"), 1, 0, missing = 0),
    M.E.0 = if_else(str_detect(code, "\\bM\\.E\\.0\\b"), 1, 0, missing = 0),
    M.E.1 = if_else(str_detect(code, "\\bM\\.E\\.1\\b"), 1, 0, missing = 0),
    M.E.2 = if_else(str_detect(code, "\\bM\\.E\\.2\\b"), 1, 0, missing = 0)
  ) |> 
  select(-c(code, note))

# Data for collaborative process
data_wide_col <- data_wide |> 
  select(
    -c(M.S.1, M.S.2, M.S.3,
      M.R.1, M.R.2, M.R.3,
      M.E.0, M.E.1, M.E.2)
  )

# Data for EGST
data_wide_cog <- data_wide |> 
  select(
    -c(C.B.1, C.B.2, C.B.3, C.B.4,
       C.M.1, C.M.2, C.M.3,
       C.R.1, C.R.2, C.R.3, C.R.4)
  )

# Data for collaborative process and EGST, simple version
data_simple <- data_wide |> 
  mutate(
    COL_Building = if_else(
      C.B.1 + C.B.2 + C.B.3 + C.B.4 >= 1, 1, 0
    ),
    COL_Monitoring = if_else(
      C.M.1 + C.M.2 + C.M.3 >= 1, 1, 0
    ),
    COL_Repairing = if_else(
      C.R.1 + C.R.2 + C.R.3 + C.R.4 >= 1, 1, 0
    ),
    COG_Situational_Reasoning = if_else(
      M.S.1 + M.S.2 + M.S.3 >= 1, 1, 0
    ),
    COG_Graphical_Reasoning = if_else(
      M.R.1 + M.R.2 + M.R.3 >= 1, 1, 0
    ),
    COG_EGST = if_else(
      M.E.0 + M.E.1 + M.E.2 >= 1, 1, 0
    )
  ) |> 
  select(
    -c(M.S.1, M.S.2, M.S.3,
       M.R.1, M.R.2, M.R.3,
       M.E.0, M.E.1, M.E.2),
    -c(C.B.1, C.B.2, C.B.3, C.B.4,
       C.M.1, C.M.2, C.M.3,
       C.R.1, C.R.2, C.R.3, C.R.4)
  )

# Basic statistical analysis ----
# Computing percentage
data_pctg <- data_simple |> 
  group_by(model_phase, scene) |> 
  summarise(
    time = min(start_time_ms) / 60000,
    pctg_CB = (sum(COL_Building) / n()) * 100,
    pctg_CM = (sum(COL_Monitoring) / n()) * 100,
    pctg_CR = (sum(COL_Repairing) / n()) * 100,
    pctg_MS = (sum(COG_Situational_Reasoning) / n() * 100),
    pctg_MR = (sum(COG_Graphical_Reasoning) / n() * 100),
    pctg_ME = (sum(COG_EGST) / n() * 100),
    .groups = "drop"
  )

# Visualisation the percentages
data_pctg |> 
  pivot_longer(
    cols = 4:9,
    names_to = "codes",
    values_to = "pctg"
  ) |> 
  mutate(
    model_phase = fct_reorder(model_phase, time),
    codes = if_else(
      codes == "pctg_CB", "Building",
      if_else(
        codes == "pctg_CM", "Monitoring",
        if_else(
          codes == "pctg_CR", "Repairing",
          if_else(
            codes == "pctg_MS", "Situational Reasoning",
            if_else(
              codes == "pctg_MR", "Graphical Reasoning",
              "EGST"
            )
          )
        )
      )
    )
  ) |> 
  mutate(
    codes = fct_relevel(codes, "Building", "Monitoring", "Repairing", "Situational Reasoning", "Graphical Reasoning", "EGST")
  ) |> 
  ggplot(
    aes(x = model_phase, y = pctg)
  ) + 
  geom_line(
    aes(group = codes, colour = codes),
    linewidth = 1,
    show.legend = F
  ) + 
  geom_point(
    aes(colour = codes),
    size = 1,
    show.legend = F
  ) + 
  facet_wrap(vars(codes), ncol = 3) + 
  theme_minimal(base_size = 14) + 
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, size = 7),
    legend.position = "top"
  ) + 
  labs(
    x = "Activity",
    y = "Percentage (%)"
  )

# Epistemic Network Analysis ----
# Specify units
unitCols <- c("subject")

# Specify codes
codeCols <- c(
  "COL_Building", "COL_Monitoring", "COL_Repairing",
  "COG_Situational_Reasoning", "COG_Graphical_Reasoning", "COG_EGST"
)

# Specify conversations
conversationCols <- c("model_phase")

# Construct a model
set_ena <- ena(
  data = data_simple,
  units = unitCols,
  codes = codeCols,
  conversation = conversationCols,
  window.size.back = 5
)

# Calculate line weights
line_weights_matrix <- as.matrix(set_ena$line.weights)
line_weights_data <- line_weights_matrix |> 
  colMeans() |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  as_tibble() |> 
  rename(weight = `colMeans(line_weights_matrix)`) |> 
  separate(rowname, into = c("source", "target"), sep = " & ")
line_weight_mean <- as.vector(colMeans(line_weights_matrix))

# Plot a mean network
ena.plot(
  set_ena,
  title = "Mean Plot",
  dimension.labels = c("X", "Y"),
  font.size = 12
) |> 
  ena.plot.network(
    network = line_weight_mean,
    colors = "blue",
    layers = c("edges", "nodes")
  )


# Ordered Network Analysis ----
# Specify units
my_units <- c("subject")

# Specify codes
my_codes <- c(
  "COL_Building", "COL_Monitoring", "COL_Repairing",
  "COG_Situational_Reasoning", "COG_Graphical_Reasoning", "COG_EGST"
)
my_cog_codes <- c(
  "COG_Situational_Reasoning", "COG_Graphical_Reasoning", "COG_EGST"
)

# Specify conversations
my_hoo_rules <- conversation_rules(
  model_phase %in% UNIT$model_phase
)

# Accumulate connections
accum_ona <- contexts(
  x = data_simple,
  hoo_rules = my_hoo_rules,
  units_by = my_units
) |> 
  accumulate_contexts(
    codes = my_codes,
    decay.function = decay(simple_window, window_size = 5),
    return.ena.set = FALSE
  )

# Construct an ONA model
set_ona <- model(accum_ona)

# Data from ONA model
ona_rotation_matrix <- set_ona$rotation.matrix |> 
  as_tibble() |> 
  separate(
    col = codes,
    into = c("source", "target"),
    sep = " & "
  ) |> 
  arrange(source, target)
ona_line_weights <- set_ona$line.weights |> 
  as_tibble() |> 
  pivot_longer(
    cols = 4:39,
    names_to = "codes",
    values_to = "weight"
  ) |> 
  separate(
    col = codes,
    into = c("source", "target"),
    sep = " & "
  ) |> 
  arrange(subject, source, target)
ona_line_weights_mean <- ona_line_weights |> 
  group_by(source, target) |> 
  summarise(
    weight = mean(weight, na.rm = TRUE),
    .groups = "drop"
  )
ona_node_position <- set_ona$rotation$nodes |> 
  as_tibble()
ona_model_var <- set_ona$model$variance

# Visualize the ONA model
ona:::plot.ena.ordered.set(
  set_ona, 
  title = "ONA Mean Network"
) |> 
  edges(
    desaturate_edges_by = sqrt,
    weights = colMeans(set_ona$line.weights),
    # edge_size_multiplier = 1,
    edge_arrow_saturation_multiplier = 1.5,
    node_position_multiplier = 1,
    edge_color = c("red")
  ) |> 
  nodes(
    node_size_multiplier = .4,
    node_position_multiplier = 1,
    self_connection_color = c("red")
  )

# Anna
ona:::plot.ena.ordered.set(
  set_ona, 
  title = "ONA Network for Anna"
) |> 
  units(
    points = set_ona$points$ENA_UNIT$`Anna`,
    points_color = "red",
    show_mean = FALSE,
    show_points = TRUE,
    with_ci = FALSE
  ) |> 
  edges(
    desaturate_edges_by = sqrt,
    weights = set_ona$line.weights$ENA_UNIT$`Anna`,
    # edge_size_multiplier = 1,
    edge_arrow_saturation_multiplier = 1.5,
    node_position_multiplier = 1,
    edge_color = c("red")
  ) |> 
  nodes(
    node_size_multiplier = .4,
    node_position_multiplier = 1,
    self_connection_color = c("red")
  )

# Tamara
ona:::plot.ena.ordered.set(
  set_ona, 
  title = "ONA Network for Tamara"
) |> 
  units(
    points = set_ona$points$ENA_UNIT$`Tamara`,
    points_color = "blue",
    show_mean = FALSE,
    show_points = TRUE,
    with_ci = FALSE
  ) |> 
  edges(
    desaturate_edges_by = sqrt,
    weights = set_ona$line.weights$ENA_UNIT$`Tamara`,
    # edge_size_multiplier = 1,
    edge_arrow_saturation_multiplier = 1.5,
    node_position_multiplier = 1,
    edge_color = c("blue")
  ) |> 
  nodes(
    node_size_multiplier = .4,
    node_position_multiplier = 1,
    self_connection_color = c("blue")
  )

# Anna - Tamara
ona:::plot.ena.ordered.set(
  set_ona, 
  title = "Subtracted Network: Anna vs. Tamara"
) |> 
  edges(
    desaturate_edges_by = sqrt,
    weights = (colMeans(set_ona$line.weights$ENA_UNIT$Anna) - colMeans(set_ona$line.weights$ENA_UNIT$Tamara)) * 4,
    # edge_size_multiplier = 1,
    edge_arrow_saturation_multiplier = 1.5,
    node_position_multiplier = 1,
    edge_color = c("red", "blue")
  ) |> 
  nodes(
    node_size_multiplier = .4,
    node_position_multiplier = 1,
    self_connection_color = c("red", "blue")
  )

# Confusion matrix for mean network
ona_line_weights_mean |> 
  mutate(weight = round(weight, 2)) |> 
  ggplot(aes(x = source, y = target, fill = weight)) + 
  geom_tile(show.legend = FALSE) + 
  geom_text(
    aes(label = weight),
    size = 3.5
  ) + 
  scale_fill_gradient(
    low = "#f7f7f7",
    high = "#2171b5"
  ) + 
  theme_minimal(base_size = 15) + 
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(
      angle = 30,
      vjust = 1,
      hjust = 1
    )
  )







