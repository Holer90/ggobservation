# ggobservation

## Overview
`ggobservation` extends `ggplot2` with the `geom_observation()` function, which adds the number of observations to the plot.

## Installation
```{r}
library(devtools)
install_github("Holer90/ggobservation")
```


## Settings
These settings are available to `geom_observation()`.

* `compute`: (`group`, `panel`) controls if calculations are per panel or per group.
* `hjust`: (`top`, `middle`, `bottom`) where the annotation is added horizontally
* `vjust`: (`left`, `center`, `right`) where the annotation is added vertically
* `prefix`: (string, default: `"n = "`) the prefix before the count.
* `suffix`: (string, default: `""`) the suffix after the count.
* `separation_factor`: (number, default: `1`) the separation of annotations when there are multiple groups in a single panel.

## Examples
//todo


## Tasks
* Write documentation
* Add Examples with pictures to README
* Fix spacing for `compute = "group"`, and deprecate `separation_factor`.
* Do I need the helper functions, or do I get them by loading tidyverse?
* Add `label` setting, which extend geom_label instead of geom_text.
