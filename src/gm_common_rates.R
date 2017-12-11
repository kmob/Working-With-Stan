# graphical model
# binary process

gm_common_rates <- function() {
  library(DiagrammeR)
  
  diagram_spec <- c("
digraph boxes_and_circles {
                    
                    # graph statement
                    graph [overlap=false, 
                    fontsize=12]
                    
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
                    k1 [label=<<I>k<SUB>1</SUB></I>>]
                    k2 [label=<<I>k<SUB>2</SUB></I>>]
                    n1 [label=<<I>n<SUB>1</SUB></I>>]
                    n2 [label=<<I>n<SUB>2</SUB></I>>]

                    node [shape = plain,
                    color = black,
                    style = filled,
                    fillcolor = white,
                    peripheries = 0]
                    note_2 [label=<&theta; ~ Beta(1,1)>]
                    note_4 [label=<<I>k</I><SUB>1 </SUB> ~ Binomial(&theta;,<I>n</I>)>]
                    note_5 [label=<<I>k</I><SUB>2 </SUB> ~ Binomial(&theta;,<I>n</I>)>]
                    
                    subgraph {
                    rank = same
                    t
                    note_2}
                    
                    subgraph {
                    rank = same
                    k1
                    note_4
                    note_5}
                    
                    # edge statements
                    edge [arrowhead=vee,
                    arrowtail=vee]
                    t->k1
                    t->k2
                    k1->n1 [dir=back] 
                    k2->n2 [dir=back] 
}
")
  
  grViz(diagram = diagram_spec,
        width = 400,
        height = 400)
  
}