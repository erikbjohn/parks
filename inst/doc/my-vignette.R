## ---- eval=TRUE, size='tiny', echo=FALSE, warning=FALSE------------------
library(knitr)
parks.shapes <- get_parks()
plot(parks.shapes)
table(parks.shapes@data$park_type)
knitr::kable(parks.shapes@data[1:20,])

