library(ggraph)
library(tidygraph)
library(tibble)
library(ggplot2)
library(sysfonts)     
library(showtext) 


# Add new font 
font_add_google("Fira Sans", "fira")
showtext_auto()

# Set colours 
colors <- c(
  core = "#7C9DBD",        # Soft steel blue
  outcome = "#E3A857",     # Warm amber
  actor = "#7EA8BE",       # Muted blue
  question = "#A4C6A2",    # Soft sage green
  mechanism = "#CDD6DD",   # Light gray-blue
  answer = "#B7C9D8",      # Soft gray-blue
  example = "#F2E8CF"      # Warm beige
)

# --- Nodes ---
nodes <- tibble::tibble(
  name = c("Z", "A", "B", "a", "b", "g", "h", 
           "aa", "ba", "aaa", "aab", "baa", "bab",
           "aaba", "aabb", "baba", "babb", "ha", "hb",
           "aaaa", "aabaa", "aabba", "babaa", "babba", "hba", "ga"),
  label = c("Political effects\nof green subsidies",
            "Reducing opposition\nBoosting short-term, but\npotentially unstable, support",
            "Building stable\nsupport base",
            "Citizens", "Business", "New interest\ngroups", "New electoral\nconstituencies",
            "Increased\nsupport?", "Less counter-\nlobbying?", "No", "Yes,\nHow?",
            "No", "Yes,\nChannels?",
            "Attitudinal", "Behavioural", "Direct only", "Direct &\nIndirect",
            "Electoral", "Green consumers\n(Besley & Persson)",
            "Example:\nBelgium\n(De Groote et al.)",
            "Example:\nLower opposition to\nclimate reform",
            "Example:\nPro-climate voting\n(Felsenfeld et al.)",
            "Example:\nSubsidies to\ndownstream industries",
            "Example:\nDownstream benefits\nfrom upstream subsidies",
            "Example:\nPV diffusion &\nGreen voting",
            "Example:\nEEG, Feed-in Tariff\n(Germany)"),
  type = c("core", "outcome", "outcome", "actor", "actor", "actor", "actor",
           "question", "question", "answer", "answer", "answer", "answer",
           "mechanism", "mechanism", "mechanism", "mechanism",
           "mechanism", "mechanism",
           "example", "example", "example", "example", "example", "example", "example")
)

# --- Edges ---
edges <- tribble(
  ~from, ~to, ~label,
  "Z", "A", "Short to\nmedium term",
  "Z", "B", "Medium to\nlong term",
  "A", "a", NA,
  "A", "b", NA,
  "B", "g", NA,
  "B", "h", NA,
  "a", "aa", NA,
  "aa", "aaa", NA,
  "aa", "aab", NA,
  "aaa", "aaaa", NA,
  "aab", "aaba", NA,
  "aab", "aabb", NA,
  "aaba", "aabaa", NA,
  "aabb", "aabba", NA,
  "b", "ba", NA,
  "ba", "baa", NA,
  "ba", "bab", NA,
  "bab", "baba", NA,
  "bab", "babb", NA,
  "baba", "babaa", NA,
  "babb", "babba", NA,
  "g", "ga", NA,
  "h", "ha", NA,
  "h", "hb", NA,
  "ha", "hba", NA,
  "hb", "hba", NA
)

# --- Create graph ---
graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# --- Plot with Sugiyama layout ---
showtext_opts(dpi = 300) 
decision_plot <- ggraph(graph, layout = "sugiyama") +
  geom_edge_link(
    aes(filter = !is.na(label), label = label),
    angle_calc = "along",
    label_dodge = unit(0.4, "mm"),
    label_size = 4.7,  # Edge labels: readable
    color = "#444444",
    arrow = arrow(length = unit(3, 'mm')),
    end_cap = circle(2, 'mm')
  ) +
  geom_edge_link(
    aes(filter = is.na(label)),
    color = "#444444",
    arrow = arrow(length = unit(3, 'mm')),
    end_cap = circle(2, 'mm')
  ) +
  geom_node_label(
    aes(label = label, fill = type),
    size = 3.6,                        # Large font for visibility
    label.size = 0.2,
    label.padding = unit(0.25, "lines"),
    lineheight = 0.95,
    family = "fira"
  ) +
  scale_fill_manual(values = colors) +
  coord_flip() +
  scale_y_reverse() +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "fira"),
    plot.margin = margin(20, 60, 20, 60)  # top, right, bottom, left
  )


# --- Save plot --
ggsave(paste0(here::here("03 Simulations", "Week3", "Figures"), "/decision_tree.png"), 
       plot = decision_plot, width = 21, height = 8.5, dpi = 300, 
       limitsize = FALSE)