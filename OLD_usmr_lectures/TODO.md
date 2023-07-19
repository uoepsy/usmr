# To change for 2021/22

### Cook's Distance

lots of students struggling with rule-of-thumb approaches (remove when >1, etc.) because this can lead to recursive data removal.

Umberto's suggestion:

> I think next year we should move to the widely accepted F distribution threshold.
> `summary(influence.measures(fitted_model))`
> will place an asterisk next to influential point when `cooks.distance() > qf(0.5, p, n-p)`

