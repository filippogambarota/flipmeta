# Simulation explorer

This Shiny application reads `simulation/res.rds`, aggregates rejection rates
for the focal coefficient `x1`, and displays Type I error or power dynamically.

Run it from the repository root with:

```r
shiny::runApp("simulation/shiny")
```

The horizontal-axis condition and the checked conditions remain separate in
the plot and table. All unchecked conditions are averaged, weighting each cell
by its number of valid Monte Carlo replications.

The application accepts both the current `zeta` column and legacy simulation
files where the scale coefficient is named `gamma`.
