# Analysis of the New Markets Tax Credit Program
Rashan Jibowu  
03/20/2015  

Load necessary libraries


```r
library(data.table)
library(ggplot2)
```

Load the data


```r
data <- read.csv("./data/projects.csv", na.strings = NA)
```

Clean up column names


```r
colnames(data) <- c("projectID", "metroStatus", "year", "CDE", "investment", "projectCost", "city", "state", "zipcode", "purpose", "investeeType", "multiCDEStatus", "multiTractStatus")
```

Convert to data table for faster processing


```r
dt <- data.table(data)
```

Find the ten most active CDFIs overall


```r
totalInvestedByCDE <- dt[,list(totalInvestment = sum(investment)), by = c("CDE")]
```

```
## Warning in gsum(investment): Group 3 summed to more than type 'integer'
## can hold so the result has been coerced to 'numeric' automatically, for
## convenience.
```

```r
# Exclude the Multi-CDE Projects
topInvestors <- totalInvestedByCDE[order(totalInvestment, decreasing = TRUE),][2:11]

# prepare plot parameters
title <- c("10 Most Active CDEs")
yLabel <- c("Total Investment (millions)")
xLabel <- c("Community Development Entity (CDE)")

# Plot data
g <- ggplot(topInvestors, aes(y = totalInvestment / 1e+06, x = CDE))
g + geom_bar(stat = "identity", color = "white", fill = "#003366", width = 0.8) + 
  #scale_x_discrete(limits = rev(levels("CDE"))) + 
  coord_flip() + 
  labs(title = title, x = xLabel, y = yLabel)
```

![](nmtc_analysis_files/figure-html/most active CDFIs-1.png) 

