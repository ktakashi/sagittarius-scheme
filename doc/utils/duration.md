[§2] (util duration) - Duration {#duration}
-------------------------------------------

###### [!Library] `(util duration)`

This library provides a set of utility procedures of duration.

###### [!Function] `duration:of-nanos` _n_
###### [!Function] `duration:of-millis` _n_
###### [!Function] `duration:of-seconds` _n_
###### [!Function] `duration:of-minutes` _n_
###### [!Function] `duration:of-hours` _n_
###### [!Function] `duration:of-days` _n_

Returns a time of object of type `duration` with the value of _n_ units.

For example, `(duration:of-millis 10)` returns 10 milli seconds equivalent
value of time duration object.
