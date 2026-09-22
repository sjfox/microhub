How should INFLAenza share short-term (week-to-week) information across your target groups?

Every option below fits all target groups in a single model with a shared seasonal curve. They differ only in how each group's *recent deviations* from that seasonal baseline are allowed to relate to the other groups'.

**Exchangeable (default)** assumes one common correlation between every pair of target groups. If one group ticks up this week, every other group is nudged up by the same expected amount. This is usually a good fit for age strata, where the groups are all drawn from the same population and respond to the same local conditions. It is the structure INFLAenza has always used, so leaving this selected reproduces your previous forecasts exactly.

**Independent (iid)** gives each group its own short-term dynamics, pooled only through a shared prior on how volatile groups tend to be. Use this when the groups genuinely move on their own schedules — separate regions with distinct epidemics, say — and you do not want a surge in one to pull the others up with it.

**None (shared trend)** fits a single short-term trend for all groups at once, with groups differing only by an overall level. This is the most heavily pooled option. It can help when individual groups have very little data, and it is the natural comparison point for asking whether group-specific dynamics are earning their keep at all.

**Spatial (neighbor graph)** uses an uploaded neighbor graph so that groups sharing a border are correlated and distant groups are not. This is the most realistic structure when your target groups are places, because an epidemic in one region genuinely does spread to its neighbors before it reaches the far side of the map. It requires a neighbor graph uploaded on the Data tab; without one, the model falls back to Exchangeable and records a warning.

### Which should I choose?

There is no universally correct answer, and the honest way to decide is to try them. Use the Retrospective tab's Advanced Setup to add one INFLAenza configuration per structure, then run them together and compare WIS and coverage on your own data. Published comparisons have found the exchangeable structure hard to beat for US state-level respiratory forecasts, so treat the alternatives as hypotheses to test rather than upgrades.
