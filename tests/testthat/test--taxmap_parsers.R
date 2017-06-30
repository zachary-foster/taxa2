library(taxa)
context("taxmap parsers")

test_that("Taxmap can be intialized from complex data", {

  # Basic parsing
  my_vector <- c("A;B;C;D", "A;E;F;G", "A;B;H;I")
  my_list_1 <- list("A;B;C;D", "A;E;F;G", c("A;B", "H;I"))
  my_list_2 <- list(c("A", "B", "C", "D"),
                    c("A", "E", "F", "G"),
                    c("A", "B", "H", "I"))
  my_frame <- data.frame(tax = c("A;B;C", "A;E;F", "A;B;H"),
                         species = c("D", "G", "I"))
  my_frames <- list(data.frame(tax = c("A", "B", "C", "D")),
                    data.frame(tax = c("A", "E", "F", "G")),
                    data.frame(tax = c("A", "B", "H", "I")))

  vector_result <- parse_tax_data(my_vector, include_tax_data = FALSE)
  list_1_result <- parse_tax_data(my_list_1, include_tax_data = FALSE)
  list_2_result <- parse_tax_data(my_list_2, include_tax_data = FALSE)
  frame_result <- parse_tax_data(my_frame, class_cols = c("tax", "species"),
                                 include_tax_data = FALSE)

  expect_equal(length(vector_result$taxon_ids()), 9)
  expect_equal(length(vector_result$roots()), 1)
  expect_equal(vector_result, list_1_result)
  expect_equal(vector_result, list_2_result)
  expect_equal(vector_result, frame_result)

  # Basic parsing with datasets
  test_obj <- parse_tax_data(my_vector, list(test = letters[1:3]),
                             mappings = c("{{index}}" = "{{index}}"))
  expect_equal(test_obj$map_data("taxon_names", "test"),
               structure(c("D", "G", "I"), .Names = c("a", "b", "c")))

  a_dataset <- data.frame(my_index = c(3, 2),
                          dataset_key = c("key_3", "key_2"))
  rownames(a_dataset) <- c("name_3", "name_2")
  a_tax_data <- data.frame(tax = c("A;B;C", "A;E;F", "A;B;H"),
                           species = c("D", "G", "I"),
                           tax_key = c("key_1", "key_2", "key_3"))
  rownames(a_tax_data) <- c("name_1", "name_2", "name_3")
  test_obj <- parse_tax_data(a_tax_data, class_cols = c("tax", "species"),
                             datasets = list(my_data = a_dataset),
                             mappings = c("{{index}}" = "my_index"))
  expect_equal(test_obj$data$my_data$taxon_id, c("9", "8"))
  test_obj <- parse_tax_data(a_tax_data, class_cols = c("tax", "species"),
                             datasets = list(my_data = a_dataset),
                             mappings = c("{{name}}" = "{{name}}"))
  expect_equal(test_obj$data$my_data$taxon_id, c("9", "8"))
  test_obj <- parse_tax_data(a_tax_data, class_cols = c("tax", "species"),
                             datasets = list(my_data = a_dataset),
                             mappings = c("tax_key" = "dataset_key"))
  expect_equal(test_obj$data$my_data$taxon_id, c("9", "8"))

  # Parsing lists of data frames with data
  my_frames <- list(data.frame(tax = c("A", "B", "C", "D"),
                               rank = c("P", "C", "O", "F")),
                    data.frame(tax = c("A", "E", "F", "G"),
                               rank = c("P", "C", "O", "F")),
                    data.frame(tax = c("A", "B", "H", "I"),
                               rank = c("P", "C", "O", "F")))
  test_obj <- parse_tax_data(my_frames, class_cols = "tax")
  expect_equal(length(test_obj$taxon_ids()), nrow(test_obj$data$tax_data))
  expect_true("rank" %in% colnames(test_obj$data$tax_data))
})


test_that("Taxmap can be intialized from queried data", {
  # Make test data
  raw_data <- data.frame(taxonomy = c("Mammalia;Carnivora;Felidae",
                                      "Mammalia;Carnivora;Felidae",
                                      "Mammalia;Carnivora;Ursidae"),
                         species = c("Panthera leo",
                                     "Panthera tigris",
                                     "Ursus americanus"),
                         my_tax_id = c("9689", "9694", "9643"),
                         my_seq = c("AB548412", "FJ358423", "DQ334818"),
                         species_id = c("A", "B", "C"))
  abundance <- data.frame(id = c("A", "B", "C", "A", "B", "C"),
                          sample = c(1, 1, 1, 2, 2, 2),
                          counts = c(23, 4, 3, 34, 5, 13))
  common_names <- c(A = "Lion", B = "Tiger", C = "Bear", "Oh my!")
  foods <- list(c("ungulates", "boar"),
                c("ungulates", "boar"),
                c("salmon", "fruit", "nuts"))

  # Parsing with taxon names
  name_result = lookup_tax_data(raw_data,
                                type = "taxon_name",
                                datasets = list(counts = abundance,
                                                names = common_names,
                                                foods = foods),
                                mappings = c("species_id" = "id",
                                             "species_id" = "{{name}}",
                                             "{{index}}" = "{{index}}"),
                                column = "species")

  # Parsing with taxon ids
  id_result = lookup_tax_data(raw_data,
                              type = "taxon_id",
                              datasets = list(counts = abundance,
                                              names = common_names,
                                              foods = foods),
                              mappings = c("species_id" = "id",
                                           "species_id" = "{{name}}",
                                           "{{index}}" = "{{index}}"),
                              column = "my_tax_id")

  # Parsing with sequence ids
  seq_result = lookup_tax_data(raw_data,
                               type = "seq_id",
                               datasets = list(counts = abundance,
                                               names = common_names,
                                               foods = foods),
                               mappings = c("species_id" = "id",
                                            "species_id" = "{{name}}",
                                            "{{index}}" = "{{index}}"),
                               column = "my_seq")

  expect_equal(name_result, id_result)
  expect_equal(name_result, seq_result)
})



test_that("Taxmap can be intialized from raw strings", {
  raw_data <- c(">var_1:A--var_2:1--non_target--tax:K__Mammalia;P__Carnivora;C__Felidae;G__Panthera;S__leo",
                ">var_1:B--var_2:2--non_target--tax:K__Mammalia;P__Carnivora;C__Felidae;G__Panthera;S__tigris",
                ">var_1:C--var_2:3--non_target--tax:K__Mammalia;P__Carnivora;C__Felidae;G__Ursus;S__americanus")

})