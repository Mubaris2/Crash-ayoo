# Crash-ayoo

A crash detection system built using R

## Prerequisites

### Install packages

```R
install.packages(c("DBI", "RSQLite", "dplyr", "lubridate", "readr", "arules", "arulesViz", "ggplot2"))
```

## Running the code

```R
source("scripts/generate_data.R")   # regenerates synthetic_data.csv
source("scripts/etl_pipeline.R")    # rebuilds dimension + fact tables
source("scripts/warehouse.R")       # loads into SQLite
source("scripts/event_mining.R")    # mines rules + runs validation

```

## Team
* Ashwin S
* Adnan Omar
* Mohamed Sheik Mubaris
* Muntasir P V
