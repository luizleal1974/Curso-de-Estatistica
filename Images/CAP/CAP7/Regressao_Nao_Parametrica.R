library(plotly)
library(np)
options(np.messages = FALSE)

# Função definida pelo programador
loess_gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  tune.loess <- function(s){
    lo <- loess(y ~ x, span = s)
    mean((lo$fitted - y)^2) / (1 - lo$trace.hat/nobs)^2
  }
  os <- optimize(tune.loess, interval = c(.01, 99))$minimum
  return(os)
}

# Dados
times = c(2.4, 2.6, 3.2, 3.6, 4.0, 6.2, 6.6, 6.8, 7.8, 8.2, 8.8, 8.8, 9.6, 10.0, 10.2, 10.6, 11.0, 11.4, 13.2, 13.6, 13.8, 14.6, 14.6, 14.6, 14.6, 14.6, 14.6, 14.8, 15.4, 15.4, 15.4, 15.4, 15.6, 15.6, 15.8, 15.8, 16.0, 16.0, 16.2, 16.2, 16.2, 16.4, 16.4, 16.6, 16.8, 16.8, 16.8, 17.6, 17.6, 17.6, 17.6, 17.8, 17.8, 18.6, 18.6, 19.2, 19.4, 19.4, 19.6, 20.2, 20.4, 21.2, 21.4, 21.8, 22.0, 23.2, 23.4, 24.0, 24.2, 24.2, 24.6, 25.0, 25.0, 25.4, 25.4, 25.6, 26.0, 26.2, 26.2, 26.4, 27.0, 27.2, 27.2, 27.2, 27.6, 28.2, 28.4, 28.4, 28.6, 29.4, 30.2, 31.0, 31.2, 32.0, 32.0, 32.8, 33.4, 33.8, 34.4, 34.8, 35.2, 35.2, 35.4, 35.6, 35.6, 36.2, 36.2, 38.0, 38.0, 39.2, 39.4, 40.0, 40.4, 41.6, 41.6, 42.4, 42.8, 42.8, 43.0, 44.0, 44.4, 45.0, 46.6, 47.8, 47.8, 48.8, 50.6, 52.0, 53.2, 55.0, 55.0, 55.4, 57.6)
accel = c(0.0, -1.3, -2.7, 0.0, -2.7, -2.7, -2.7, -1.3, -2.7, -2.7, -1.3, -2.7, -2.7, -2.7, -5.4, -2.7, -5.4, 0.0, -2.7, -2.7, 0.0, -13.3, -5.4, -5.4, -9.3, -16.0, -22.8, -2.7, -22.8, -32.1, -53.5, -54.9, -40.2, -21.5, -21.5, -50.8, -42.9, -26.8, -21.5, -50.8, -61.7, -5.4, -80.4, -59.0, -71.0, -91.1, -77.7, -37.5, -85.6, -123.1, -101.9, -99.1, -104.4, -112.5, -50.8, -123.1, -85.6, -72.3, -127.2, -123.1, -117.9, -134.0, -101.9, -108.4, -123.1, -123.1, -128.5, -112.5, -95.1, -81.8, -53.5, -64.4, -57.6, -72.3, -44.3, -26.8, -5.4, -107.1, -21.5, -65.6, -16.0, -45.6, -24.2, 9.5, 4.0, 12.0, -21.5, 37.5, 46.9, -17.4, 36.2, 75.0, 8.1, 54.9, 48.2, 46.9, 16.0, 45.6, 1.3, 75.0, -16.0, -54.9, 69.6, 34.8, 32.1, -37.5, 22.8, 46.9, 10.7, 5.4, -1.3, -21.5, -13.3, 30.8, -10.7, 29.4, 0.0, -10.7, 14.7, -1.3, 0.0, 10.7, 10.7, -26.8, -14.7, -13.3, 0.0, 10.7, -14.7, -2.7, 10.7, -2.7, 10.7)
dados = data.frame(times, accel)

# Parâmetro
s = loess_gcv(x = dados$times, y = dados$accel)

# Regressão não paramétrica
LOESS = loess(formula = accel ~ times, data = dados, span = s, degree = 2)
KERNEL = npreg(accel ~ times, regtype = "ll", bwmethod = "cv.aic", gradients = TRUE, data = dados)

# Teste de significância
sig_test = npsigtest(KERNEL) 

# Qualidade do ajuste
npr_R2 = function(y, fit_val){
  ybar = mean(y)
  r2 = (sum((y - ybar) * (fit_val - ybar))^2) / (sum((y - ybar)^2) * sum((fit_val - ybar)^2))
  return(r2)
}
r2 = data.frame(Modelo = c("LOESS", "KERNEL"),
           R2 = c(npr_R2(y = dados$accel, fit_val = fitted(LOESS)  ),
                  npr_R2(y = dados$accel, fit_val = fitted(KERNEL) )
                  )
           )

# Prever nova observação
new = data.frame(times = c(37))
accel_LOESS = predict(LOESS, newdata = new)
accel_KERNEL = predict(KERNEL, newdata = new)
nova = data.frame(new, accel_LOESS, accel_KERNEL)

# Atributos dos eixos
ax = list(title =  list(text = "<b>Tempo (ms)</b>"    , font = list(family = "Calibri", size = 44, color = 'black')), tickfont = list(family = "Calibri", size = 44, color = "black"), zerolinecolor = 'rgb(255, 255, 255)', zerolinewidth = 2, gridcolor = 'rgb(255, 255, 255)')
ay = list(title =  list(text = "<b>Aceleração (g)</b>", font = list(family = "Calibri", size = 44, color = 'black')), tickfont = list(family = "Calibri", size = 44, color = "black"), zerolinecolor = 'rgb(255, 255, 255)', zerolinewidth = 2, gridcolor = 'rgb(255, 255, 255)')

# Scatter plot
plot_ly() %>%
  add_trace(x = dados$times, y = dados$accel    , type = 'scatter' , mode = 'markers' , name = ''       , marker = list(color = '#000000'  , size = 8 ) , showlegend = FALSE) %>%
  add_trace(x = dados$times, y = fitted(LOESS)  , type = 'scatter' , mode = 'lines'   , name = 'Loess'  , line   = list(color = 'steelblue', width = 5) , showlegend = TRUE ) %>%
  add_trace(x = dados$times, y = fitted(KERNEL) , type = 'scatter' , mode = 'lines'   , name = 'Kernel' , line   = list(color = '#f6546a'  , width = 5) , showlegend = TRUE ) %>%
  layout(plot_bgcolor = 'rgb(230, 230, 230)',
         title = list(text = "<b>Scatter plot</b>", font = list(family = "Calibri", size = 44, color = 'black')),
         legend = list(title = list(text = '<b>Modelo</b>', font = list(family = "Calibri", size = 40, color = 'black')), font = list(family = "Calibri", size = 40, color = 'black')),
         xaxis = ax,
         yaxis = ay,
         margin = list(autoexpand = FALSE, t = 80, b = 120, r = 190, l = 160),
         showlegend = T)
