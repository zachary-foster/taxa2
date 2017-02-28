## Testing `taxmap` class

library(taxa)
context("Testing the `taxmap` object")

## Creating test data
notoryctidae <- taxon(
  name = taxon_name("Notoryctidae"),
  rank = taxon_rank("family"),
  id = taxon_id(4479)
)
notoryctes <- taxon(
  name = taxon_name("Notoryctes"),
  rank = taxon_rank("genus"),
  id = taxon_id(4544)
)
typhlops <- taxon(
  name = taxon_name("typhlops"),
  rank = taxon_rank("species"),
  id = taxon_id(93036)
)
mammalia <- taxon(
  name = taxon_name("Mammalia"),
  rank = taxon_rank("class"),
  id = taxon_id(9681)
)
felidae <- taxon(
  name = taxon_name("Felidae"),
  rank = taxon_rank("family"),
  id = taxon_id(9681)
)
puma <- taxon(
  name = taxon_name("Puma"),
  rank = taxon_rank("genus"),
  id = taxon_id(146712)
)
concolor <- taxon(
  name = taxon_name("concolor"),
  rank = taxon_rank("species"),
  id = taxon_id(9696)
)
panthera <- taxon(
  name = taxon_name("Panthera"),
  rank = taxon_rank("genus"),
  id = taxon_id(146712)
)
tigris <- taxon(
  name = taxon_name("tigris"),
  rank = taxon_rank("species"),
  id = taxon_id(9696)
)
plantae <- taxon(
  name = taxon_name("Plantae"),
  rank = taxon_rank("kingdom"),
  id = taxon_id(33090)
)
solanaceae <- taxon(
  name = taxon_name("Solanaceae"),
  rank = taxon_rank("family"),
  id = taxon_id(4070)
)
solanum <- taxon(
  name = taxon_name("Solanum"),
  rank = taxon_rank("genus"),
  id = taxon_id(4107)
)
lycopersicum <- taxon(
  name = taxon_name("lycopersicum"),
  rank = taxon_rank("species"),
  id = taxon_id(49274)
)
tuberosum <- taxon(
  name = taxon_name("tuberosum"),
  rank = taxon_rank("species"),
  id = taxon_id(4113)
)
unidentified <- taxon(
  name = taxon_name("unidentified")
)

tiger <- hierarchy(mammalia, felidae, panthera, tigris)
cougar <- hierarchy(mammalia, felidae, puma, concolor)
mole <- hierarchy(mammalia, notoryctidae, notoryctes, typhlops)
tomato <- hierarchy(plantae, solanaceae, solanum, lycopersicum)
potato <- hierarchy(plantae, solanaceae, solanum, tuberosum)
potato_partial <- hierarchy(solanaceae, solanum, tuberosum)
unidentified_animal <- hierarchy(mammalia, unidentified)
unidentified_plant <- hierarchy(plantae, unidentified)

# create object used by multiple tests
abund <- data.frame(name = c("tiger", "cougar", "mole"),
                    count = 1:3)
counts <- tibble::as_tibble(data.frame(taxon = c("T", "C", "M"),
                                       num = c(2, 3, 4),
                                       num_2 = c(4, 5, 6)))
a_list <- list("a", "b", "c", "a", "b", "c")
a_vector <- 1:3
a_func <- function(x) {paste0(x$data$abund$name, "!!!")}

x <- taxmap(tiger, cougar, mole,
            data = list(counts = counts,
                        a_list = a_list,
                        a_vector = a_vector,
                        abund = abund),
            funcs = list(loud_names = a_func))

mutate_obs(x, "counts",
           new_col = paste(taxon, num, loud_names),
           newer_col = paste("better than", new_col))

transmute_obs(x, "counts",
              new_col = paste(taxon, num, loud_names),
              newer_col = paste("better than", new_col))

arrange_obs(x, "counts", 3:1)
arrange_obs(x, "counts", taxon, num)
arrange_obs(x, "counts", loud_names)
arrange_taxa(x, 1:9)



test_that("Simple usage", {
  expect_length(x$taxa, 9)
  expect_equal(dim(x$edge_list), c(9, 2))
  expect_length(x$roots(), 1)
})


test_that("Finding names and values usable for NSE", {
  # These functions are used to find the relevant inforamtion in an expression passed to a maniulation function like `filter_taxa`
  x$all_names()
  x$names_used(num == 3, length(c(count)) > num, loud_names == "quiet")
  x$data_used(num == 3, length(c(count)) > num, loud_names == "quiet")
})



test_that("Filtering by taxon", {
  filter_taxa(x, 2, subtaxa = TRUE, supertaxa = TRUE, reassign_obs = c(abund = TRUE, a_vector = FALSE), taxonless = c(abund = F, a_vector = T))
})


# TODO: data as single frame and unnamed list
