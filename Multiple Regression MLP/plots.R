library(ggplot2)


# Fonction de plot de la distribution des labels y 
plot_distribution <- function(y){
  df <- as.data.frame(y)
  V <- df[,1]
  
  plot_y <- ggplot(df, aes(x= V)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.2)+
    geom_density(alpha=.2, color="darkblue", fill="lightblue") + geom_vline(aes(xintercept=mean(V)),
    color="blue", linetype="dashed", size=1) +
    ggtitle(colnames(y)) + xlab("Normalized values") + ylab("Density")

  return(plot_y)
}


# Fonction de plot de la régression entre labels prédits et true labels
# L'argument fit de la fonction correspond à un objet reg renvoyé par la fonction regression_multiple()
plot_regression <- function (fit) {
  
  sum <- summary(fit)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(sum$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


