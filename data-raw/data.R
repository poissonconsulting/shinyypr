library(ypr)
library(purrr)

params <- ypr::ypr_population()
parameters <- names(params)
attributes <- ypr:::.parameters
template <- ypr_tabulate_parameters(ypr_population())
schedule <- ypr::ypr_schedule(ypr_population())

desc <- template[, c("Parameter", "Description")]

datasets <- data(package = "ypr")$results[, c("Item", "Title")] %>%
  as.data.frame()
datasets$Item <- as.character(datasets$Item)
datasets$Title <- as.character(datasets$Title)

dropdown_list <- lapply(datasets$Item, function(x) {
  y <- eval(parse(text = x))
  if (class(y) == "ypr_populations") {
    return(names(y))
  } else {
    return(x)
  }
})
names(dropdown_list) <- datasets$Title

datasets <- map2_dfr(datasets$Item, datasets$Title, function(a, b) {
  y <- eval(parse(text = a))
  if (class(y) == "ypr_populations") {
    return(data.frame(Item = names(y), Title = b, Key = paste(a, names(y), sep = "$")))
  } else {
    return(data.frame(Item = a, Title = b, Key = a))
  }
})

fish.y <- c("Survivors", "Spawners", "Caught", "Harvested", "Released", "HandlingMortalities")
fish.x <- c("Age", "Length", "Weight")

usethis::use_data(parameters, params, attributes, template, desc, schedule,
  dropdown_list, datasets, fish.x, fish.y,
  internal = TRUE, overwrite = TRUE
)
