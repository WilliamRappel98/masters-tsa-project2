### Packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, forecast, tseries, cowplot, knitr, kableExtra)


### Functions
source('utils.R')


### Reproducibility
set.seed(10)


### Data
df <- read_csv('data/IPCA.csv')
df$Date[1]
df$Date[nrow(df)-12]
df$Date[nrow(df)-11]
df$Date[nrow(df)]
x  <- ts(df$IPCA, frequency=12, start=c(2008, 1), end=c(2021, 12))
xx <- ts(df$IPCA, frequency=12, start=c(2022, 1), end=c(2022, 12))


### Time series plot
x %>%
  autoplot() +
  labs(x='Year', y='IPCA (Monthly Var.)') +
  scale_x_continuous(breaks=scales::extended_breaks(15)) +
  scale_y_continuous(labels=function(x) paste0(x, '%')) +
  theme_bw()


### STL decomposition
x %>%
  mstl() %>%
  autoplot() +
  labs(x='Year') +
  scale_x_continuous(breaks=scales::extended_breaks(15)) +
  scale_y_continuous(labels=function(x) paste0(x, '%')) +
  theme_bw()


### ARIMA model
# Diffs
ndiffs(x)
nsdiffs(x)
adf.test(x)
# ACF and PACF
acf <- x %>%
  ggAcf(lag.max=12*3) +
  labs(title='') +
  theme_bw()
pacf <- x %>%
  ggPacf(lag.max=12*3) +
  labs(title='') +
  theme_bw()
plot_grid(acf, pacf, nrow=1)
# p = 0, 1
# q = 0, 1, 2, 3
# P = 0
# Q = 0, 1
p <- 0:1
q <- 0:3
P <- 0
Q <- 0:1
best_aicc <- Inf
for (pi in p) {
  for (qi in q) {
    for (Pi in P) {
      for (Qi in Q) {
        mod <- x %>% Arima(order=c(pi, 0, qi), seasonal=c(Pi, 0, Qi), include.mean=T, lambda=NULL)
        if (mod$aicc < best_aicc) {
          best_mod <- mod
          best_aicc <- mod$aicc
        }
      }
    }
  }
}
(mod_arima <- best_mod)
# Residuals plots
residuals_plots(mod_arima)
# Residuals tests
residuals_tests(mod_arima)


### ETS model
(mod_ets <- ets(x, model='ZZN'))
# Residuals plots
residuals_plots(mod_ets)
# Residuals tests
residuals_tests(mod_ets)


### Sliding window validation
CV_arima <- x %>% tsCV(forecastfunction=func_arima, h=12, initial=13)
CV_ets <- x %>% tsCV(forecastfunction=func_ets, h=12, initial=13)
MAE_arima <- CV_arima %>% abs() %>% colMeans(na.rm=T)
MAE_ets <- CV_ets %>% abs() %>% colMeans(na.rm=T)
tab <- cbind(MAE_arima, MAE_ets)
tab %>%
  kable(
    col.names=c('ARIMA', 'ETS'),
    caption='MAE by horizon.',
    digits=3,
    format.args=list(decimal.mark='.', scientific=F),
    align='c',
    booktabs=T
  )
tab_plot <- tab %>%
  as.data.frame() %>%
  mutate(Horizon=1:12) %>%
  gather(key='Model', value='MAE', -Horizon)
tab_plot %>%
  ggplot(aes(x=Horizon, y=MAE)) +
  geom_line(aes(color=Model)) + 
  scale_x_continuous(breaks=scales::extended_breaks(12)) +
  scale_color_manual(
    values=c('black', 'red'),
    breaks=c('MAE_arima', 'MAE_ets'),
    labels=c('ARIMA', 'ETS')
  ) +
  theme_bw()


### Forecast
# Benchmark comparison
h <- 12
preds <- list(
  'ARIMA' = forecast(mod_arima, h=h),
  'ETS' = forecast(mod_ets, h=h),
  'naive' = naive(x, h=h),
  'meanf' = meanf(x, h=h),
  'holt' = holt(x, h=h),
  'hw' = hw(x, h=h),
  'auto.arima' = forecast(auto.arima(x), h=h),
  'sltf' = stlf(x, h=h),
  'bats' = forecast(bats(x), h=h),
  'tbats' = forecast(tbats(x), h=h),
  'thetaf' = forecast(thetaf(x), h=h)
)
mae <- unlist(lapply(preds, function(m) return(mean(abs(xx - m$mean)))))
final <- data.frame(MAE=mae)
final %>%
  kable(
    caption='MAE on test.',
    digits=3,
    format.args=list(decimal.mark='.', scientific=F),
    align='c',
    booktabs=T
  )
# plot
# plot
vec <- c('meanf', 'Observed')
cores <- c('#0000AA', 'red')
names(cores) <- vec
preds <- meanf(x, h=h, level=95)
x %>%
  autoplot() + xlab('Year') + ylab('IPCA (Monthly Var.)') + theme_bw() +
  autolayer(preds, series='meanf') +
  autolayer(xx, series='Observed') +
  scale_x_continuous(breaks=scales::extended_breaks(10)) +
  scale_y_continuous(labels=function(x) paste0(x, '%')) +
  scale_colour_manual(
    values=cores,
    breaks=vec,
    name='')