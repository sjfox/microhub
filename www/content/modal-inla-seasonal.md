How should INFLAenza share the seasonal curve across your target groups?

The seasonal effect is a cyclic smooth over epiweek — the repeating annual shape your data follows. This setting controls how many such curves are estimated, and which target groups share one.

**Shared across all groups (default)** estimates a single seasonal curve from every target group at once. This is the right choice when your groups differ in level but follow the same annual rhythm — age strata within one country, or states within one climate zone. It is also the most data-efficient option, since every group contributes to one well-identified curve. This is what INFLAenza has always done, so leaving it selected reproduces your previous forecasts exactly.

**One curve per seasonal group** estimates a separate curve for each group named in an uploaded seasonal grouping file, with the curves drawn from a common prior. Use this when some regions genuinely peak at a different time of year: a tropical territory alongside temperate states, or a southern-hemisphere country in a mostly northern panel. Because the curves share a prior, each one is free to take whatever shape its data supports while still being regularised — which matters a great deal when the distinct region has only a few seasons of history. This option is unavailable until a seasonal grouping is uploaded on the Data tab.

**One curve per target group** gives every target group its own seasonal curve with no pooling of shape. This is the most flexible option and the least data-efficient: a cyclic curve over 52 epiweeks estimated from a single group's few seasons is poorly identified, and the result is often a curve that chases noise. It is mainly useful as a comparison point, to check whether the grouped or shared versions are losing anything real.

### The grouping file

Two columns, `target_group` and `season_group`. Season group labels are arbitrary text — only which groups share a label matters, never the label itself:

```
target_group,season_group
Alaska,Alaska
Hawaii,Tropical
Puerto Rico,Tropical
```

List only the exceptions. Any target group you leave out falls into one shared default group, so the example above produces three curves: one for Alaska, one shared by Hawaii and Puerto Rico, and one shared by every remaining state.

### Choosing a grouping

Seasonal grouping is deliberately separate from the neighbor graph, because spatial adjacency and seasonal regime are different things that merely tend to coincide. Alaska has no land border with another state but its seasonal shape is temperate; south Florida borders Georgia but behaves subtropically. Keeping them apart lets you say "Hawaii and Puerto Rico share a tropical curve" — pooling two data-poor series that are nowhere near each other — which deriving the grouping from geography could never express.

Prefer fewer, larger groups over many small ones. Two regions that peak at roughly the same time are better off sharing a curve than each estimating its own from thin data. As with the group structure setting, the honest way to decide is to add one configuration per grouping in the Retrospective tab's Advanced Setup and compare scores — and because scores break out by target group, you can check whether the distinct region actually improved without dragging the rest down.
