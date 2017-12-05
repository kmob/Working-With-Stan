# graphical model
# binary process

gm_binary_process <- function() {
  library(DiagrammeR)
  
  diagram_spec <- c(
    "
    digraph boxes_and_circles {
    
    # graph statement
    graph [overlap=false, fontsize=10]
    
    # node statements
    node [shape=circle,
    color=black,
    style=filled,
    fillcolor=white,
    peripheries=1]
    t [ label=<&theta;>]
    
    node [shape=square,
    color=black,
    style=filled,
    fillcolor=grey,
    peripheries=1]
    k [label=<<I>k</I>>]
    n [label=<<I>n</I>>]
    
    node [shape=plain,
    color=black,
    style=filled,
    fillcolor=white,
    peripheries=0]
    note_1 [label=<&theta; ~ Beta(1,1)>]
    note_2 [label=<<I>k </I> ~ Binomial(&theta;,<I>n</I>)>]
    
    subgraph {
    rank = same; t; note_1;
    }
    
    subgraph {
    rank = same; k; note_2;
    }
    
    # edge statements
    edge [arrowhead=vee,arrowtail=vee]
    t->k
    k->n [dir=back]
    }
    ")
  
  grViz(diagram = diagram_spec,
        width = 200,
        height = 200)
  
}