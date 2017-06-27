#' Convert one or more data sets to taxmap
#'
#' Parses taxonomic information and assoiacted data and stores it in a
#' [taxa::taxmap()] object. [Taxonomic
#' classifications](https://en.wikipedia.org/wiki/Taxonomy_(biology)#Classifying_organisms)
#' must be present somewhere in the first input.
#'
#' @param tax_data A table, list, or vector that contains the names of taxa that
#'   represent [taxonomic classifications](https://en.wikipedia.org/wiki/Taxonomy_(biology)#Classifying_organisms).
#'    Accepted representations of classifications include:
#'   * A list/vector or table with column(s) of taxon names: Something like
#'   `"Animalia;Chordata;Mammalia;Primates;Hominidae;Homo"`. What separator(s)
#'   is used (";" in this example) can be changed with the `class_sep` option.
#'   For tables, the classification can be spread over multiple columns and the
#'   separator(s) will be applied to each column, although each column could
#'   just be single taxon names with no separator. Use the `class_cols` option
#'   to specify which columns have taxon names.
#'   * A list in which each entry is a classificaiton. For example,
#'   `list(c("Animalia", "Chordata", "Mammalia", "Primates", "Hominidae",
#'   "Homo"), ...)`.
#'   * A list of data.frames where each represents a classification with one
#'   taxon per row. The column that contains taxon names is specified using the
#'   `class_cols` option. In this instance, it only makes sense to specify a
#'   single column.
#' @param datasets Additional lists/vectors/tables that should be included in
#'   the resulting `taxmap` object. The `mappings` option is use to specify how
#'   these data sets relate to the `tax_data` and, by inference, what taxa apply
#'   to each item.
#' @param class_cols (`character` or `integer`) The names or indexes of columns
#'   that contain classifications if the first input is a table. If mutliple
#'   columns are specified, they will be combined in the order given.
#' @param class_sep (`character`) One or more separators that delineate taxon
#'   names in a classification. For example, if one column had `"Homo sapiens"`
#'   and another had `"Animalia;Chordata;Mammalia;Primates;Hominidae"`, then
#'   `class_sep = c(" ", ";")`. All separators are applied to each column so
#'   order does not matter.
#' @param sep_is_regex (`TRUE`/`FALSE`) Whether or not `class_sep` should be
#'   used as a [regular
#'   expression](https://en.wikipedia.org/wiki/Regular_expression).
#' @param mappings (named `character`) This defines how the taxonomic
#'   information in `tax_data` applies to data set in `datasets`. This option
#'   should have the same number of inputs as `datasets`, with values
#'   corresponding to each data set. The names of the character vector specify
#'   what information in `tax_data` is shared with info in each `dataset`, which
#'   is specified by the corresponding values of the character vector. If there
#'   are no shared variables, you can add `NA` as a placeholder, but you could
#'   just leave that data out since it is not benifiting from being in the
#'   taxmap object. The names/values can be one of the following:
#'   * For tables, the names of columns can be used.
#'   * `"{{index}}"` : This means to use the index of rows/items
#'   * `"{{name}}"`  : This means to use row/item names.
#'   * `"{{value}}"` : This means to use the values in vectors or lists. Lists
#'   will be converted to vectors using [unlist()].
#' @param include_tax_data (`TRUE`/`FALSE`) Whether or not to include `tax_data`
#'   as a dataset, like those in `datasets`.
#'
#' @examples
#'   # Make example data with taxonomic classifications
#'   species_data <- data.frame(taxonomy = c("Mammalia;Carnivora;Felidae",
#'                                           "Mammalia;Carnivora;Felidae",
#'                                           "Mammalia;Carnivora;Ursidae"),
#'                              species = c("Panthera leo",
#'                                          "Panthera tigris",
#'                                          "Ursus americanus"),
#'                              species_id = c("A", "B", "C"))
#'
#'   # Make example data associated with the taxonomic data
#'   # Note how this does not contain classifications, but
#'   # does have a varaible in common with "species_data" ("id" = "species_id")
#'   abundance <- data.frame(id = c("A", "B", "C", "A", "B", "C"),
#'                           sample = c(1, 1, 1, 2, 2, 2),
#'                           counts = c(23, 4, 3, 34, 5, 13))
#'
#'   # Make another related data set named by species id
#'   common_names <- c(A = "Lion", B = "Tiger", C = "Bear", "Oh my!")
#'
#'   # Make another related data set with no names
#'   foods <- list(c("ungulates", "boar"),
#'                 c("ungulates", "boar"),
#'                 c("salmon", "fruit", "nuts"))
#'
#'   # Make a taxmap object with these three datasets
#'   x = parse_tax_data(species_data,
#'                      datasets = list(counts = abundance,
#'                                      names = common_names,
#'                                      foods = foods),
#'                      mappings = c("species_id" = "id",
#'                                   "species_id" = "{{name}}",
#'                                   "{{index}}" = "{{index}}"),
#'                      class_cols = c("taxonomy", "species"),
#'                      class_sep = c(" ", ";"))
#'
#'   # Note how all the datasets have taxon ids now
#'   x$data
#'
#'   # This allows for complex mappings between variables that other functions use
#'   map_data(x, "foods", "names")
#'   map_data(x, "names", "counts")
#'
#' @export
parse_tax_data <- function(tax_data, datasets = list(), class_cols = 1,
                           class_sep = ";", sep_is_regex = FALSE,
                           mappings = c(), include_tax_data = TRUE) {

  # Check for nonsensical options
  if (length(datasets) != length(mappings)) {
    stop(paste0('The `mappings` option must have the same number of values (',
                length(datasets), ') as the `datasets` option.'))
  }
  if (length(mappings) > 0 && is.null(names(mappings))) {
    stop(paste0('The mapping options must be named.'))
  }
  for (i in seq_len(length(datasets))) {
    valid_mappings <- c("{{index}}", "{{name}}", "{{value}}")
    if (is.data.frame(datasets[[i]])) {
      valid_mappings <- c(valid_mappings, colnames(datasets[[i]]))
    }
    if (is.data.frame(tax_data)) {
      valid_mappings <- c(valid_mappings, colnames(tax_data))
    }
    mappings_used <- c(mappings[[i]], names(mappings[[i]]))
    invalids <- mappings_used[! mappings_used %in% valid_mappings]
    if (length(invalids) > 0) {
      stop(paste0('Invalid inputs to the `mappings` found for input "', invalids[1], '". ',
                  'The names and values of `mappings` must be one of the following: ',
                  paste0(valid_mappings, collapse = ", ")))
    }
  }

  # Deal with edge cases
  if (length(tax_data) == 0) {
    return(taxmap())
  }

  # Get classificaitons
  is_list_of_frames <- FALSE
  if (is.character(tax_data)) { # is a character vector
    parsed_tax <- multi_sep_split(tax_data, fixed = !sep_is_regex, split = class_sep)
  } else if (is.data.frame(tax_data)) { # is a data.frame
    parsed_tax <- lapply(seq_len(nrow(tax_data)),
                         function(i) {
                           class_source <- as.character(unlist(tax_data[i, class_cols]))
                           unname(unlist(multi_sep_split(class_source,
                                                         fixed = !sep_is_regex,
                                                         split = class_sep)))
                         })
  } else if (is.list(tax_data) && is.data.frame(tax_data[[1]])) { # is a list of data.frames
    if (length(class_cols) > 1) {
      stop("When the taxonomy source is a list of data.frames, it does not make sense to specify multiple columns.")
    }
    parsed_tax <- unlist(lapply(tax_data, function(x) {
      lapply(seq_len(nrow(x)), function(i) {
        as.character(x[1:i, class_cols])
      })
    }), recursive = FALSE)
    tax_data <- do.call(rbind, tax_data)
    is_list_of_frames <- TRUE
  } else if (is.list(tax_data) && is.character(tax_data[[1]])) { # is a list of characters
    parsed_tax <- lapply(tax_data, function(x) unlist(multi_sep_split(x, split = class_sep)))
  } else {
    stop("Unknown format for first input. Cannot parse taxonomic information.")
  }

  # Create taxmap object
  output <- do.call(taxmap, parsed_tax)

  # Add taxonomic source to datasets
  if (include_tax_data) {
    datasets <- c(list(tax_data = tax_data), datasets)
    mappings <- c("{{index}}" = "{{index}}", mappings)
  }

  # Convert additional tables to tibbles
  are_tables <- vapply(datasets, is.data.frame, logical(1))
  datasets[are_tables] <- lapply(datasets[are_tables], dplyr::as.tbl)

  # Add additional data sets
  name_datset <- function(dataset, sort_var) {
    target_value <- get_sort_var(dataset, sort_var)
    source_value <- get_sort_var(tax_data, names(sort_var))
    obs_taxon_ids <- output$input_ids[match(target_value, source_value)]
    if (is.data.frame(dataset)) {
      dataset <- dplyr::bind_cols(dplyr::tibble(taxon_id = obs_taxon_ids),
                                  dataset)
    } else {
      names(dataset) <- obs_taxon_ids
    }
    return(dataset)
  }

  output$data <- lapply(seq_len(length(datasets)),
                        function(i) name_datset(datasets[[i]], mappings[i]))

  # Name additional datasets
  names(output$data) <- names(datasets)

  # Fix incorrect taxon ids in data if a list of data.frames is given
  if (is_list_of_frames && include_tax_data) {
    output$data[[1]] <- output$data[[1]][!duplicated(output$data[[1]]), ]
  }

  return(output)
}



#' Convert one or more data sets to taxmap
#'
#'
#' Looks up taxonomic data from NCBI sequence IDs, taxon IDs, or taxon names
#' that are present in a dataset. Also can incorperate additional assocaited
#' datasets.
#'
#' @param tax_data A table, list, or vector that contain sequence IDs, taxon
#'   IDs, or taxon names.
#'   * tables: The `column` option must be used to specify which column
#'   contains the sequence IDs, taxon IDs, or taxon names.
#'   * lists: There must be only one item per list entry unless the `column`
#'   option is used to specify what item to use in each list entry.
#'   * vectors: simply a vector of sequence IDs, taxon IDs, or taxon names.
#' @param type (`"seq_id"`, `"taxon_id"`, `"taxon_name"`) What type of
#'   information can be used to look up the classifications.
#' @param column (`character` or `integer`) The name or index of the column that
#'   contains information used to lookup classifications. This only applies when
#'   a table or list is supplied to `tax_data`.
#' @param datasets Additional lists/vectors/tables that should be included in
#'   the resulting `taxmap` object. The `mappings` option is use to specify how
#'   these data sets relate to the `tax_data` and, by inference, what taxa apply
#'   to each item.
#' @param mappings (named `character`) This defines how the taxonomic
#'   information in `tax_data` applies to data set in `datasets`. This option
#'   should have the same number of inputs as `datasets`, with values
#'   corresponding to each data set. The names of the character vector specify
#'   what information in `tax_data` is shared with info in each `dataset`, which
#'   is specified by the corresponding values of the character vector. If there
#'   are no shared variables, you can add `NA` as a placeholder, but you could
#'   just leave that data out since it is not benifiting from being in the
#'   taxmap object. The names/values can be one of the following:
#'   * For tables, the names of columns can be used.
#'   * `"{{index}}"` : This means to use the index of rows/items
#'   * `"{{name}}"`  : This means to use row/item names.
#'   * `"{{value}}"` : This means to use the values in vectors or lists. Lists
#'   will be converted to vectors using [unlist()].
#' @param database (`character`) The name of a database to use to look up
#'   classifications. Options include "ncbi", "itis", "eol", "col", "tropicos",
#'  and "nbn".
#' @param include_tax_data (`TRUE`/`FALSE`) Whether or not to include `tax_data`
#'   as a dataset, like those in `datasets`.
#'
#' @examples
#'   # Make example data with taxonomic classifications
#'   species_data <- data.frame(taxonomy = c("Mammalia;Carnivora;Felidae",
#'                                           "Mammalia;Carnivora;Felidae",
#'                                           "Mammalia;Carnivora;Ursidae"),
#'                              species = c("Panthera leo",
#'                                          "Panthera tigris",
#'                                          "Ursus americanus"),
#'                              species_id = c("A", "B", "C"))
#'
#'   # Make example data associated with the taxonomic data
#'   # Note how this does not contain classifications, but
#'   # does have a varaible in common with "species_data" ("id" = "species_id")
#'   abundance <- data.frame(id = c("A", "B", "C", "A", "B", "C"),
#'                           sample = c(1, 1, 1, 2, 2, 2),
#'                           counts = c(23, 4, 3, 34, 5, 13))
#'
#'   # Make another related data set named by species id
#'   common_names <- c(A = "Lion", B = "Tiger", C = "Bear", "Oh my!")
#'
#'   # Make another related data set with no names
#'   foods <- list(c("ungulates", "boar"),
#'                 c("ungulates", "boar"),
#'                 c("salmon", "fruit", "nuts"))
#'
#'   # Make a taxmap object with these three datasets
#'   x = lookup_tax_data(species_data,
#'                       type = "taxon_name",
#'                       datasets = list(counts = abundance,
#'                                       names = common_names,
#'                                       foods = foods),
#'                       mappings = c("species_id" = "id",
#'                                    "species_id" = "{{name}}",
#'                                    "{{index}}" = "{{index}}"),
#'                       column = "species")
#'
#'   # Note how all the datasets have taxon ids now
#'   x$data
#'
#'   # This allows for complex mappings between variables that other functions use
#'   map_data(x, "foods", "names")
#'   map_data(x, "names", "counts")
#'
#' @export
lookup_tax_data <- function(tax_data, type, column = 1, datasets = list(),
                            mappings = c(), database = "ncbi",
                            include_tax_data = TRUE) {
  # Hidden parameters
  batch_size <- 100
  max_print <- 10
  internal_class_sep <- "||||"
  internal_class_name <- "___class_string___"

  # Define lookup functions
  use_taxon_id <- function(ids) {
    result <- map_unique(ids, taxize::classification, ask = FALSE, rows = 1,
                         db = database)
    # Rename columns of result
    failed_queries <- is.na(result)
    result[! failed_queries] <- lapply(result[! failed_queries],
                                       function(x) stats::setNames(x, c(paste0(database, "_name"),
                                                                        paste0(database, "_rank"),
                                                                        paste0(database, "_id"))))
    return(result)
  }

  use_seq_id <- function(ids) {
    # Look up classifications
    result <- stats::setNames(taxize::classification(taxize::genbank2uid(ids, batch_size = batch_size)),
                              ids)
    # Rename columns of result
    failed_queries <- is.na(result)
    result[! failed_queries] <- lapply(result[! failed_queries],
                                       function(x) stats::setNames(x, c(paste0(database, "_name"),
                                                                        paste0(database, "_rank"),
                                                                        paste0(database, "_id"))))
  }

  use_taxon_name <- function(names) {
    # Look up classifications
    result <- map_unique(names, taxize::classification, ask = FALSE, rows = 1, db = database)
    # Complain about failed queries
    failed_queries <- is.na(result)
    if (sum(failed_queries) > 0) {
      warning(paste0('The following ', sum(failed_queries),
                     ' taxon name could not be looked up:\n  ',
                     limited_print(names(failed_queries), type = "silent")))
    }
    # Rename columns of result
    result[! failed_queries] <- lapply(result[! failed_queries],
                                       function(x) stats::setNames(x, c(paste0(database, "_name"),
                                                                        paste0(database, "_rank"),
                                                                        paste0(database, "_id"))))
    return(result)
  }

  lookup_funcs <- list("seq_id" = use_seq_id,
                       "taxon_id" = use_taxon_id,
                       "taxon_name" = use_taxon_name)

  # Get query information
  if (is.data.frame(tax_data)) { # is table
    query <- as.character(tax_data[[column]])
  } else if (is.list(tax_data)) { # is list
    query <- vapply(tax_data,
                    function(x) as.character(x[[column]]),
                    character(1))
  } else if (is.vector(tax_data)) { # is vector
    query <- as.character(tax_data)
  }

  # Look up taxonomic classifications
  if (! type %in% names(lookup_funcs)) {
    stop(paste0('Invalid "type" option. It must be one of the following:\n  ',
                paste0(names(lookup_funcs), collapse = ", ")))
  }
  classifications <- lookup_funcs[[type]](query)
  class_strings <- unlist(lapply(classifications, function(x) {
    lapply(seq_len(nrow(x)), function(i) {
      paste(as.character(x[1:i, 1]), collapse = internal_class_sep)
    })
  }))
  combined_class <- do.call(rbind, unname(classifications))
  internal_class_frame <- stats::setNames(data.frame(class_strings,
                                                     stringsAsFactors = FALSE),
                                          internal_class_name)
  combined_class <- cbind(internal_class_frame, combined_class)

  # Add mapping columns to classfication data
  tax_data_indexes <- cumsum(vapply(classifications, nrow, numeric(1)))
  mappping_cols <- unique(c(names(mappings), "{{index}}", "{{name}}"))
  if (!is.data.frame(tax_data)) {
    mappping_cols <- c(mappping_cols, "{{value}}")
  }
  for (col in mappping_cols) {
    combined_class[tax_data_indexes, col] <- get_sort_var(tax_data, col)
  }

  # Add input data to datasets included in the resulting object
  if (include_tax_data) {
    datasets <- c(list(query_data = tax_data), datasets)
    mappings <- c("{{index}}" = "{{index}}", mappings)
  }

  # Make taxmap object
  output <- parse_tax_data(tax_data = combined_class,
                           datasets = datasets,
                           class_cols = 1,
                           class_sep = internal_class_sep,
                           mappings = mappings,
                           include_tax_data = include_tax_data)

  # Remove mapping columns from output
  output$data$tax_data[mappping_cols] <- NULL

  # Remove class column from output
  output$data$tax_data[internal_class_name] <- NULL

  # Fix incorrect taxon ids for tax_data
  #   This is due to the "{{index}}" being interpreted as a column name,
  #   which is needed for the user-defined data sets to be parsed right.
  output$data$tax_data$taxon_id <- output$input_ids

  # Remove duplicates from `tax_data`
  output$data[[1]] <- output$data[[1]][!duplicated(output$data[[1]]), ]
  if (include_tax_data) {
    output$data$tax_data <- output$data$tax_data[!duplicated(output$data$tax_data), ]
  }
  return(output)
}


#' Get a vector from a vector/list/table to be used in mapping
#'
#' @param data A vector/list/table
#' @param var What to get.
#'   * For tables, the names of columns can be used.
#'   * `"{{index}}"` : This means to use the index of rows/items
#'   * `"{{name}}"`  : This means to use row/item names.
#'   * `"{{value}}"` : This means to use the values in vectors or lists. Lists
#'
#' @keywords internal
get_sort_var <- function(data, var) {
  if (is.data.frame(data) && var %in% colnames(data)) {
    return(data[[var]])
  } else if (var == "{{index}}") {
    if (is.data.frame(data)) {
      return(seq_len(nrow(data)))
    } else {
      return(seq_len(length(data)))
    }
  } else if (var == "{{name}}") {
    if (is.data.frame(data)) {
      return(rownames(data))
    } else {
      return(names(data))
    }
  }  else if (var == "{{value}}") {
    if (is.data.frame(data)) {
      stop("The `{{value}}` setting of the `mappings` option cannot be used with data.frames.")
    } else {
      return(unlist(data))
    }
  }  else {
    stop(paste0('No column named "', var, '"."'))
  }
}
