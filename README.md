# covid-data-prediction


This is a very simple shiny app for project, stat433.

Run this app by calling <code>shiny::runGitHub('covid-data-prediction','Clouddelta',ref='main')</code>.


It takes some time to run because of loading csv file remotely.


## Problem Fixed:
```{r}
read.csv(url("https://covid19.who.int/WHO-COVID-19-global-data.csv"))  #working
```

## <code>read.csv</code> versus <code>read_csv</code>
<code>read.csv</code> imports data as a dataframe, which is an old structure, and <code>read_csv</code> imports data as a tibble.

Not working
```{r}
read.csv('https://covid19.who.int/who-data/vaccination-data.csv')
```

Working
```{r}
library(readr)
read_csv('https://covid19.who.int/who-data/vaccination-data.csv')
```