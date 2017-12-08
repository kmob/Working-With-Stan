# graphical model
# binary process

gm_two_rates <- function() {
  library(DiagrammeR)
  
  diagram_spec <- c("
digraph boxes_and_circles {
                    
                    # graph statement
                    graph [overlap = false, fontsize = 10]
                    
  node [shape=circle, 
        color=black, 
                    style=filled, 
                    fillcolor=white, 
                    peripheries=2]
                    d [ label=<&delta;>]
                    
                    node [shape=circle, 
                    color=black, 
                    style=filled, 
                    fillcolor=white, 
                    peripheries=1]
                    t1 [ label=<&theta;<SUB>1</SUB>>]
                    t2 [ label=<&theta;<SUB>2</SUB>>]
                    
                    node [shape=square, 
                    color=black, 
                    style=filled, 
                    fillcolor=grey, 
                    peripheries=1]
                    k1 [label=<<I>k<SUB>1</SUB></I>>]
                    k2 [label=<<I>k<SUB>2</SUB></I>>]
                    n1 [label=<<I>n<SUB>1</SUB></I>>]
                    n2 [label=<<I>n<SUB>2</SUB></I>>]
                    
                    node [shape = plain,
                    color = black,
                    style = filled,
                    fillcolor = white,
                    peripheries = 0]
    note_1 [label=<&delta; is &theta;<SUB>1 </SUB> is &theta;<SUB>2 </SUB>>]
    note_2 [label=<&theta;<SUB>1 </SUB> ~ Beta(1,1)>]
    note_3 [label=<&theta;<SUB>2 </SUB> ~ Beta(1,1)>]
    note_4 [label=<<I>k</I><SUB>1 </SUB> ~ Binomial(&theta;,<I>n</I>)>]
    note_5 [label=<<I>k</I><SUB>2 </SUB> ~ Binomial(&theta;,<I>n</I>)>]
                    
                    subgraph {
                    rank = same
                    d
                    note_1}
                    
                    subgraph {
                    rank = same
                    t1
                    note_2
                    note_3}

                    subgraph {
                    rank = same
                    k1
                    note_4
                    note_5}

                    # edge statements
                    edge [arrowhead = vee,
                    arrowtail = vee]
                    d -> t1
                    d -> t2
                    t1 -> k1
                    t2 -> k2
                    k1 -> n1 [dir = back]
                    k2 -> n2 [dir = back]
}
")  
  grViz(diagram = diagram_spec,
        width = 400,
        height = 400)
  
}