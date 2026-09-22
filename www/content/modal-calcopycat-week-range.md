**Respiratory Week Range** controls how wide a ring of neighboring weeks
CalCopycat considers around each yearly anchor.

CalCopycat compares today's date against the same calendar position 1 year
back, 2 years back, 3 years back, and so on -- exact 52-week multiples,
assuming weekly data -- rather than scoring every historical date in the
record. This setting widens each of those anchors into a ring: a historical
week qualifies as a candidate if it falls within this many weeks of any of
those yearly anchors.

A value of 0 only considers historical weeks that land on exactly 52, 104,
156, ... weeks before today. Raising it broadens the pool of candidate weeks
CalCopycat can draw on, at the cost of allowing slightly less
seasonally-aligned matches. The default of 2 mirrors Copycat's own
Respiratory Week Range setting.
