install.packages("tidyquant")
install.packages("tidyverse")

library(tidyquant)
library(tidyverse)

tickers = c('TSLA')
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
       title = "Giá điều chỉnh của Tesla từ 2015 đến hiện tại") +
  scale_y_continuous(breaks = seq(0,300,10)) + 
  theme(legend.position = "top")

# chỉ lấy symbol, date, adjusted
adjusted <- prices %>%
  select(c("symbol", "date", "adjusted")) %>%
  spread(key="symbol", value="adjusted")

# calculate daily returns
returns <- adjusted %>%
  select(c("date", "TSLA")) %>%
  mutate(returns = log(TSLA / lead(TSLA)))
# loại bỏ giá trị bị mất
returns <- drop_na(returns)

# tính volatility
vol <- sd(returns$returns)

time <- sqrt(1)

CI_95 = 0.95
CI_99 = 0.99


# ppf -> percent point function: inverse of cdf 
ppf_95 <- qnorm(1 - CI_95)
ppf_99 <- qnorm(1 - CI_99)

var_95 <- vol * CI_95 * time
var_99 <- vol * CI_99 * time

print(paste("Value at Risk of TSLA 95% is", round(var_95 * 100, 2), "%"))
print(paste("Value at Risk of TSLA 99% is", round(var_99 * 100, 2), "%"))

list_VaR = c()
for (time in 1:100) {
  VaR_local <- vol * CI_95 * sqrt(time)
  list_VaR <- append(list_VaR, VaR_local)
}

list_VaR

list_VaR <- data.frame(list(time=1:100, VaR=list_VaR))

ggplot(data=list_VaR) +
  geom_line(aes(x=time, y=VaR), size=2) +
  theme_classic() + 
  labs(x = 'Time',
       y = "VaR",
       title = "Value at Risk CI 95% theo các khoảng thời gian")

plot_hist <- function(var_95, var_99) {
  title = paste("Phân phối tỉ suất lợi nhuận với VaR bằng phương pháp Variance-Covariance")
  
  var_95_label = paste("Var 95% CI:", round(var_95, 5))
  var_99_label = paste("VaR 99% CI:", round(var_99, 5))
  
  text_position = 0.12
  
  return(
    returns %>%
      ggplot(aes(x = returns, y = ..density..)) +
      geom_histogram(color = "black", fill = "cyan") + 
      # kernel density
      geom_density(lwd = 0.75, linetype = 1, colour = "red", fill = "blue", alpha = 0.15) +
      geom_vline(xintercept=var_95, color="blue", size=1) +
      geom_text(aes(x=text_position, label=var_95_label, y=10), colour="blue", angle=30, vjust = 1.2, size=5)+
      geom_vline(xintercept=var_99, color="purple", size=1) +
      geom_text(aes(x=text_position, label=var_99_label, y=15), colour="purple", angle=30, vjust = 1.2, size=5)+
      ggtitle(title) +
      theme_minimal() + 
      theme(legend.position = "top")
  )
}

plot_hist(var_95, var_99)

