# plot theta

post_density_plot <- function(post_data, Nbreaks) {
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
  y <- hist(post_data, Nbreaks, plot=F)
  plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=1,
       xlim=c(0,1), xlab="Rate", ylab="Posterior Density")  
}
