Each value of `retrospective_group` gets its own **Zone** dropdown (A–E) instead of the single country search box used elsewhere in the app. That's because a `retrospective_group` value doesn't have to be a real, lookup-able country name — it can be any label you chose (a country, a region, a hospital network, a code name) — so the app can't auto-assign a zone for it the way it can for a country typed into **Local Seasonality** on the Data tab.

#### How to pick a zone for each group

1.  If the group *is* a real country (or a region within one), the fastest way is to type that country's name into the **Local Seasonality** selector on the Data tab and read off the zone badge it's assigned — then come back here and pick that same letter for the matching group.

2.  Otherwise, match the group's typical respiratory season to the closest description below:

    -   **Zone A** — Northern Hemisphere winter peak (roughly October–April)
    -   **Zone B** — Northern Hemisphere / transitional (roughly September–May)
    -   **Zone C** — Tropical / year-round with moderate seasonality
    -   **Zone D** — Southern Hemisphere / tropical summer peak (roughly March–November, often broader/earlier)
    -   **Zone E** — Southern Hemisphere winter peak (roughly May–November)

Getting this exactly right matters less than getting it close: the assigned zone is used by the **Copycat**, **CalCopycat**, **Seasonal Baseline**, **newGBQR**, **parGBQR**, and **FourCAT** models mainly to define the start/end weeks of the season and to line up the current season with comparable historical seasons for that group. (newGBQR and parGBQR also learn peak timing empirically from each group's own uploaded data rather than relying on the zone alone.)

Every group defaults to Zone E until you change it — double-check each one before running, especially if your groups span different hemispheres or climates.
