# Beyond Average Scores: Identification of Consistent and Inconsistent Academic Achievement in Grouping Units

This project compares two approaches for identifying clustering units (schools) with unusual within-cluster variance:

- Spike-and-Slab Mixed-Effects Location Scale Model (SS-MELSM) implemented in ivd (via JAGS).

- Two-stage Hierarchical Linear Model (HLM) following Raudenbush & Bryk (1987).

## Methods Summary

For each simulation condition, synthetic student test score data are generated for a specified number of schools and students per school. School-level residual variance is modeled with a mixture distribution: most schools are assigned to a spike component (baseline variance), and a proportion are assigned to a slab component (elevated variance). The magnitude of variance inflation in slab schools is controlled by a specified standard deviation on the log-scale. Data are analyzed using both the SS-MELSM and the two-stage HLM. Performance is evaluated in terms of true/false positives and negatives when flagging schools as unusually variable.

## Project Structure

- `R/`: Main simulation script and helper functions

- `models/`: Model definition files (e.g., SpikeSlab.bug)

- `output/`: Simulation results saved as `.rds`
    
## References

Carmo, M., Williams, D. R., & Rast, P. (2024, November 24). Beyond Average Scores: Identification of Consistent and Inconsistent Academic Achievement in Grouping Units. https://doi.org/10.31234/osf.io/sh6ne

Raudenbush, S. W., & Bryk, A. S. (1987). Examining Correlates of Diversity. Journal of Educational Statistics, 12(3), 241–269. https://doi.org/10.3102/10769986012003241
