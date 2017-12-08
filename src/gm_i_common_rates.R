# graphical model
# binary process

gm_i_common_rates <- function() {
  library(DiagrammeR)
  
  diagram_spec <- c("
digraph boxes_and_circles {
                    
                    # graph statement
                    graph [overlap=false, fontsize=12]
                    
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
                    ki [label=<<I>k<SUB>i</SUB></I>>]
                    ni [label=<<I>n<SUB>i</SUB></I>>]

                    node [shape = plain,
                    color = black,
                    style = filled,
                    fillcolor = white,
                    peripheries = 0]
                    note_1 [label=<&theta; ~ Beta(1,1)<br/><br/><br/><I>k</I><SUB>i </SUB> ~ Binomial(&theta;,<I>n</I><SUB>i </SUB>)>]

                    # edge statements
                    edge [arrowhead=vee,
                    arrowtail=vee]

                    subgraph {
                    rank = same
                    t
                    note_1}

                    # subgraph
                    subgraph cluster1 {
                    style=rounded
                    ki->ni [dir=back]
                    label = <<I>i</I>>
                    labeljust=r
                    labelloc=b
                    color=blue
                    }


                    t->ki
}
")
  
  grViz(diagram = diagram_spec,
        width = 200,
        height = 200)
  
}