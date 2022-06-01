install.packages("tidyquant")
install.packages("PerformanceAnalytics")
install.packages("tidyverse")

library(tidyquant)
library(PerformanceAnalytics)
library(tidyverse) # dyplyr, ggplot

# historial method = historical
# parametric (variance-covariance) = gaussian
# monte carlo = modified

# define tickers and share ratio
tickers = c('AAPL','FB', 'GOOGL', 'NVDA', 'TSLA')
weights = c(0.25, 0.15, 0.2, 0.2, 0.2)
invested_value = 1000000000 # 1 bilions

# get data
prices <- tq_get(tickers,
                 from = "2015-01-01",
                 to = "2022-05-13",
                 get = "stock.prices")

# plot data over time
prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size=2) +
  theme_classic() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Giá điều chỉnh của Apple, Facebook, Google, Nvidia, Tesla từ 2015 đến hiện tại") +
  scale_y_continuous(breaks = seq(0,300,10)) + 
  theme(legend.position = "top")

# chỉ lấy symbol, date, adjusted
adjusted <- prices %>%
  select(c("symbol", "date", "adjusted")) %>%
  spread(key="symbol", value="adjusted")

# sum all tickers together
adjusted["returns"] = rowSums(adjusted[, 2:6] * weights)
  
adjusted

# calculate daily returns
returns <- adjusted %>%
  select(c("date", "returns")) %>%
  mutate(returns = (returns - lead(returns)) / returns) 
# áp dụng ct, (ngày ht - ngày tương lai) / ngày hiện tại

returns

plot_hist <- function(var_95, var_99, method) {
  title = paste("Phân phối tỉ suất lợi nhuận với VaR bằng phương pháp", method)
  
  var_95_label = paste("VaR", method, "95% CI:", round(var_95, 5))
  var_99_label = paste("VaR", method, "99% CI:", round(var_99, 5))
  
  text_position = 0.2
  
  return(
    returns %>%
      ggplot(aes(x = returns, y = ..density..)) +
      geom_histogram(color = "black", fill = "cyan") + 
      # kernel density
      geom_density(lwd = 0.75, linetype = 1, colour = "red", fill = "blue", alpha = 0.15) +
      geom_vline(xintercept=var_95, color="blue", size=1) +
      geom_text(aes(x=text_position, label=var_95_label, y=4.5), colour="blue", angle=0, vjust = 1.2, size=5)+
      geom_vline(xintercept=var_99, color="purple", size=1) +
      geom_text(aes(x=text_position, label=var_99_label, y=4), colour="purple", angle=0, vjust = 1.2, size=5)+
      ggtitle(title) +
      theme_minimal() + 
      theme(legend.position = "top")
  )
}


## historial method
make_var <- function(method) {
  var_method = switch(method, "Historical" = "historical",
                              "Parametric" = "gaussian",
                              "Monte Carlo" = "modified")
  
  var_95 = VaR(R = returns$returns, p = 0.95, method = var_method)
  var_99 = VaR(R = returns$returns, p = 0.99, method = var_method)
  
  print(paste("VaR của phương pháp", method, "với 95% CI", var_95 * invested_value))
  print(paste("VaR của phương pháp", method, "với 99% CI", var_99 * invested_value))
  
  p <- plot_hist(var_historial_95,var_historial_99, method)
  
  return (list("var_95" = var_95,
               "var_99" = var_99,
               "plot" = p))
}

# histogram
historical <- make_var("Historical")
historical$plot
historical$var_95
historical$var_99

parametric <- make_var("Parametric")
parametric$plot
parametric$var_95
parametric$var_99

monte_carlo <- make_var("Monte Carlo")
monte_carlo$plot
monte_carlo$var_95
monte_carlo$var_99
