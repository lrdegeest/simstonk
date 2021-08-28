# simstonk

R package to simulate investment portfolios. 

Simulations are done by drawing ordered sequences from asset returns. 

So you can ask questions like, "if I hold AAPL for 365 days, what is the probability I get a return greater than 5%". 

You can adjust the start and end period of pricing data to explore how this probability changes over time.

Built this package for teaching. Useful in the classroom. 

# Install

```r
library(remotes)
remotes::install_github("lrdegeest/simstonk")
```

# Use

Get asset data for some time range:

```
appl = get_stonk("AAPL", from = "2010-01-01", to "2020-01-01")
```

Get the distribution of returns to holding the asset for some time range:

```
appl_sim = simulate_returns(appl, n_days_in_market = 30)
```

Plot them:

```
plot_sim(appl_sim, cumulative = TRUE, normalized = TRUE)
```
