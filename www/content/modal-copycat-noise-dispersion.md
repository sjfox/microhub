Controls how much observation noise is layered onto each simulated Copycat trajectory when Data Type is set to Proportion.

For proportion data, noise is drawn from a Beta distribution centered on the trajectory's simulated value, with this number acting as its concentration (precision): higher values (e.g. 500-1000) pull draws tightly around the simulated trajectory, while lower values (e.g. 5-20) spread them out and widen the forecast's uncertainty. A reasonable starting point is 50-200; retune it by comparing forecast interval coverage in the Retrospective tab.

This setting has no effect when Data Type is set to Counts -- count data still uses Poisson noise, whose spread is determined automatically by the trajectory's mean.
