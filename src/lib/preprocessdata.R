myPlot <- function(beta){
  y <- isbsg2$team.size.gp - mean(isbsg2$team.size.gp)
  x <- isbsg2$norm.pdr - mean(isbsg2$norm.pdr)
  freqData <- as.data.frame(table(x, y))
  names(freqData) <- c("team.size.gp", "norm.pdr", "freq")
  plot(
    as.numeric(as.vector(freqData$norm.pdr)), 
    as.numeric(as.vector(freqData$team.size.gp)),
    pch = 21, col = "black", bg = "lightblue",
    cex = .15 * freqData$freq, 
    xlab = "norm.pdr", 
    ylab = "team.size.gp"
  )
  abline(0, beta, lwd = 3)
  points(0, 0, cex = 2, pch = 19)
  mse <- mean( (y - beta * x)^2 )
  title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
##

manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))
