# Function for residuals plots
residuals_plots <- function(mod) {
  res <- mod %>% residuals()
  est <- res %>%
    autoplot() +
    labs(x='Year', y='Residual') +
    scale_x_continuous(breaks=scales::extended_breaks(5)) +
    theme_bw()
  qq <- res %>%
    data.frame() %>%
    ggplot(aes(sample=res)) +
    stat_qq() +
    stat_qq_line() +
    labs(x='Theoretical Quantile', y='Sample Quantile') +
    theme_bw()
  acf <- res %>%
    ggAcf(lag.max=12*3) +
    labs(title='') +
    theme_bw()
  pacf <- res %>%
    ggPacf(lag.max=12*3) +
    labs(title='') +
    theme_bw()
  plot_grid(est, qq, acf, pacf, nrow=2)
}

# Function for residuals tests
residuals_tests <- function(mod) {
  mod %>% checkresiduals(plot=F)
  mod %>% residuals %>% shapiro.test
}

# Sliding window functions
func_arima <- function(y, h){
  fit <- Arima(y, order=c(1, 0, 0), seasonal=c(0, 0, 1), include.mean=T, lambda=NULL)
  forecast(fit, h)
}
func_ets <- function(y, h){
  fit <- ets(y, model='ANN', damped=F)
  forecast(fit, h)
}