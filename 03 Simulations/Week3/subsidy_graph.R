
# Import librares 

library(DiagrammeR)
library(DiagrammeRsvg)
library(magick)

# set path to Figures directory 
fig_dir <- here::here("03 Simulations", "Week3", "Figures")


# Create graph 
decision_tree <- grViz("
  digraph causal {
    graph [ranksep = 0.6, nodesep = 0.4, fontsize = 10, rankdir = LR]

    # Base style
    node [fontname = 'Helvetica', fontsize = 10, style = filled, shape = box, width=1.8, height=0.8]

    # Core concept
    node [fillcolor = '#2C3E50', fontcolor = white, penwidth = 1.5]
    Z [label = 'Political effects\\nof green subsidies']

    # Short/long-term outcomes
    node [fillcolor = '#E2B365', fontcolor = black]
    A [label = 'Reducing opposition\\nBoosting short-term\\nPotentially unstable support']
    B [label = 'Building stable\\nsupport base']

    # Actors
    node [fillcolor = '#B0C4DE']
    a [label = 'Citizens']
    b [label = 'Business']
    g [label = 'New interest\\ngroups']
    h [label = 'New constituencies']

    # Conditional questions
    node [shape = circle, fillcolor = '#90C7C1', width=1.6, height=1.2]
    aa [label = 'Increased\\nsupport?']
    ba [label = 'Less counter-\\nlobbying?']
    aaa [label = 'No']
    aab [label = 'Yes,\\nHow?']
    baa [label = 'No']
    bab [label = 'Yes,\\nChannels?']

    # Mechanisms - soft gray-blue
    node [shape = circle, fillcolor = '#C5D3D5']
    aaba [label = 'Attitudinal']
    aabb [label = 'Behavioural']
    baba [label = 'Direct only']
    babb [label = 'Direct &\\nIndirect']
    ha [label = 'Electoral']
    hb [label = 'Green consumers\\n(Besley & Persson)']

    # Examples - refined beige
    node [shape = box, fillcolor = '#F6E7C1']
    aaaa [label = 'Example:\\nBelgium\\n(De Groote et al.)']
    aabaa [label = 'Example:\\nLower opposition to\\nclimate reform']
    aabba [label = 'Example:\\nPro-climate voting\\n(Felsenfeld et al.)']
    babaa [label = 'Example:\\nSubsidies to\\ndownstream industries']
    babba [label = 'Example:\\nDownstream benefit\\nfrom upstream subsidies']
    hba [label = 'Example:\\nPV diffusion &\\nGreen voting']
    ga [label = 'Example:\\nEEG, Feed-in Tariff\\n(Germany)']

    # Edges
    edge [color = '#555555', arrowsize = 0.8, fontname = 'Helvetica', fontsize = 9]

    Z -> A [label = 'Short to\\nmedium term', labelangle = -40, labeldistance = 1.5]
    Z -> B [label = 'Medium to\\nlong term', labelangle = 45, labeldistance = 1.5]

    A -> a
    A -> b
    B -> g
    B -> h

    b -> ba
    ba -> baa
    ba -> bab
    bab -> baba
    bab -> babb
    baba -> babaa
    babb -> babba

    a -> aa
    aa -> aaa
    aa -> aab
    aaa -> aaaa
    aab -> aaba
    aab -> aabb
    aaba -> aabaa
    aabb -> aabba

    g -> ga
    h -> ha
    h -> hb
    ha -> hba
    hb -> hba

    {rank = same; A; B}
  }
")

# Save the graph to a png file
svg <- decision_tree %>% export_svg()
image <- image_read_svg(svg, width = 3000, height = 2000)
image_trimmed <- image_trim(image)

image_write(image_trimmed, path = file.path(fig_dir, "decision_tree_trimmed.png"), format = "png")



# Save the graph to a file
# decision_tree %>%
#   export_svg() %>%
#   charToRaw() %>%
#   rsvg_png(file = file.path(fig_dir, "decision_tree.png"), 
#            width = 3000,     
#            height = 2000)


