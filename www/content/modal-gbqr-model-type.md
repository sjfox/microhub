**Individual** fits a separate model for each target group using only that group's historical data. Use this when your target groups have distinct epidemic patterns and several years of history each.

**Global** trains a single model on all target groups simultaneously, using one-hot encodings so the model learns patterns that are shared across groups. Use this when some groups have limited data and could benefit from patterns learned across other groups.
