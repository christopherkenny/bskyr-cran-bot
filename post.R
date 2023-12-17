library(bskyr)
library(dplyr)

pkgs <- available.packages() |>
  as_tibble() |>
  select(Package, Version)

cat(nrow(pkgs))
