#' Convert one or more data sets to taxmap
#'
#' Parses taxonomic information and associated data and stores it in a
#' [taxa::taxmap()] object. [Taxonomic classifications](https://en.wikipedia.org/wiki/Taxonomy_(biology)#Classifying_organisms)
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
#'   * A list in which each entry is a classifications. For example,
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
#'   that contain classifications if the first input is a table. If multiple
#'   columns are specified, they will be combined in the order given.
#' @param class_sep (`character`) One or more separators that delineate taxon
#'   names in a classification. For example, if one column had `"Homo sapiens"`
#'   and another had `"Animalia;Chordata;Mammalia;Primates;Hominidae"`, then
#'   `class_sep = c(" ", ";")`. All separators are applied to each column so
#'   order does not matter.
#' @param sep_is_regex (`TRUE`/`FALSE`) Whether or not `class_sep` should be
#'   used as a [regular expression](https://en.wikipedia.org/wiki/Regular_expression).
#' @param class_key (`character` of length 1) The identity of the capturing
#'   groups defined using `class_regex`. The length of `class_key` must be equal
#'   to the number of capturing groups specified in `class_regex`. Any names
#'   added to the terms will be used as column names in the output. At least
#'   one `"taxon_name"` must be specified. Only `"info"` can be used
#'   multiple times. Each term must be one of those described below:
#'   * `taxon_name`: The name of a taxon. Not necessarily unique, but are
#'   interpretable by a particular `database`. Requires an internet connection.
#'   * `info`: Arbitrary taxon info you want included in the output. Can be used
#'   more than once.
#' @param class_regex (`character` of length 1)
#'   A regular expression with capturing groups indicating the locations of data
#'   for each taxon in the `class` term in the `key` argument. The identity of
#'   the information must be specified using the `class_key` argument. The
#'   `class_sep` option can be used to split the classification into data for
#'   each taxon before matching. If `class_sep` is `NULL`, each match of
#'   `class_regex` defines a taxon in the classification.
#' @param include_match (`logical` of length 1) If `TRUE`, include the part of
#'   the input matched by `class_regex` in the output object.
#' @param mappings (named `character`) This defines how the taxonomic
#'   information in `tax_data` applies to data set in `datasets`. This option
#'   should have the same number of inputs as `datasets`, with values
#'   corresponding to each data set. The names of the character vector specify
#'   what information in `tax_data` is shared with info in each `dataset`, which
#'   is specified by the corresponding values of the character vector. If there
#'   are no shared variables, you can add `NA` as a placeholder, but you could
#'   just leave that data out since it is not benefiting from being in the
#'   taxmap object. The names/values can be one of the following:
#'   * For tables, the names of columns can be used.
#'   * `"{{index}}"` : This means to use the index of rows/items
#'   * `"{{name}}"`  : This means to use row/item names.
#'   * `"{{value}}"` : This means to use the values in vectors or lists. Lists
#'   will be converted to vectors using [unlist()].
#' @param include_tax_data (`TRUE`/`FALSE`) Whether or not to include `tax_data`
#'   as a dataset, like those in `datasets`.
#'
#' @family parsers
#'
#' @examples
#'   # Make example data with taxonomic classifications
#'   species_data <- data.frame(tax = c("Mammalia;Carnivora;Felidae",
#'                                      "Mammalia;Carnivora;Felidae",
#'                                      "Mammalia;Carnivora;Ursidae"),
#'                              species = c("Panthera leo",
#'                                          "Panthera tigris",
#'                                          "Ursus americanus"),
#'                              species_id = c("A", "B", "C"))
#'
#'   # Make example data associated with the taxonomic data
#'   # Note how this does not contain classifications, but
#'   # does have a varaible in common with "species_data" ("id" = "species_id")
#'   abundance <- data.frame(id = c("A", "B", "C", "A", "B", "C"),
#'                           sample_id = c(1, 1, 1, 2, 2, 2),
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
#'                                      my_names = common_names,
#'                                      foods = foods),
#'                      mappings = c("species_id" = "id",
#'                                   "species_id" = "{{name}}",
#'                                   "{{index}}" = "{{index}}"),
#'                      class_cols = c("tax", "species"),
#'                      class_sep = c(" ", ";"))
#'
#'   # Note how all the datasets have taxon ids now
#'   x$data
#'
#'   # This allows for complex mappings between variables that other functions use
#'   map_data(x, my_names, foods)
#'   map_data(x, counts, my_names)
#'
#' @export
parse_tax_data <- function(tax_data, datasets = list(), class_cols = 1,
                           class_sep = ";", sep_is_regex = FALSE,
                           class_key = "taxon_name", class_regex = "(.*)",
                           include_match = TRUE,
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
  if (! "taxon_name" %in% class_key) {
    stop('At least one value in "class_key" must be "taxon_name".')
  }

  # Deal with edge cases
  if (length(tax_data) == 0) {
    return(taxmap())
  }

  # Get classificatons
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

  # Extract out any taxon info
  taxon_info <- lapply(parsed_tax, function(x)
    data.frame(stringr::str_match(x, class_regex), stringsAsFactors = FALSE))
  taxon_info <- do.call(rbind, taxon_info)
  names(taxon_info) <- c("match", names(class_key))
  parsed_tax <- split(taxon_info[, which(class_key == "taxon_name") + 1],
                      rep(seq_len(length(parsed_tax)),
                          vapply(parsed_tax, length, integer(1))))

  # Create taxmap object
  output <- taxmap(.list = parsed_tax)

  # Add taxon ids to extracted info and add to data
  if (ncol(taxon_info) > 2) {
    taxon_info$taxon_id <- unlist(lapply(output$supertaxa(output$input_ids,
                                                          value = "taxon_ids",
                                                          include_input = TRUE),
                                          rev))
    output$data$class_data <- taxon_info
    if (!include_match) {
      output$data$class_data$match  <- NULL
    }
  }

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

  output$data <- c(output$data,
                   stats::setNames(lapply(seq_len(length(datasets)),
                                          function(i) name_datset(datasets[[i]],
                                                                  mappings[i])),
                                   names(datasets)))

  # Fix incorrect taxon ids in data if a list of data.frames is given
  if (is_list_of_frames && include_tax_data) {
    output$data$tax_data <- output$data$tax_data[!duplicated(output$data[[1]]), ]
  }

  # Check format of data sets
  check_taxmap_data(output)

  return(output)
}



#' Convert one or more data sets to taxmap
#'
#'
#' Looks up taxonomic data from NCBI sequence IDs, taxon IDs, or taxon names
#' that are present in a dataset. Also can incorporate additional associated
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
#'   information in `tax_data` applies to data in `datasets`. This option
#'   should have the same number of inputs as `datasets`, with values
#'   corresponding to each dataset. The names of the character vector specify
#'   what information in `tax_data` is shared with info in each `dataset`, which
#'   is specified by the corresponding values of the character vector. If there
#'   are no shared variables, you can add `NA` as a placeholder, but you could
#'   just leave that data out since it is not benefiting from being in the
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
#' @param use_database_ids (`TRUE`/`FALSE`) Whether or not to use downloaded
#'   database taxon ids instead of arbitrary, automatically-generated taxon ids.
#'
#' @family parsers
#'
#' @examples \dontrun{
#'
#'   # Make example data with taxonomic classifications
#'   species_data <- data.frame(tax = c("Mammalia;Carnivora;Felidae",
#'                                      "Mammalia;Carnivora;Felidae",
#'                                      "Mammalia;Carnivora;Ursidae"),
#'                              species = c("Panthera leo",
#'                                          "Panthera tigris",
#'                                          "Ursus americanus"),
#'                              species_id = c("A", "B", "C"))
#'
#'   # Make example data associated with the taxonomic data
#'   # Note how this does not contain classifications, but
#'   # does have a varaible in common with "species_data" ("id" = "species_id")
#'   abundance <- data.frame(id = c("A", "B", "C", "A", "B", "C"),
#'                           sample_id = c(1, 1, 1, 2, 2, 2),
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
#'                                       my_names = common_names,
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
#'   map_data(x, my_names, foods)
#'   map_data(x, counts, my_names)
#' }
#' @export
lookup_tax_data <- function(tax_data, type, column = 1, datasets = list(),
                            mappings = c(), database = "ncbi",
                            include_tax_data = TRUE, use_database_ids = TRUE) {
  # Make sure taxize is installed
  check_for_pkg("taxize")

  # Check that a supported database is being used
  supported_databases <- names(database_list)
  if (! database %in% supported_databases) {
    stop(paste0('The database "', database,
                '" is not a valid database for looking up that taxonomy of ',
                'sequnece ids. Valid choices include:\n',
                limited_print(supported_databases, type = "silent")))
  }

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
    # Check that a supported database is being used
    supported_databases <- c("ncbi")
    if (! database %in% supported_databases) {
      stop(paste0('The database "', database,
                  '" is not a valid database for looking up that taxonomy of ',
                  'sequnece ids. Valid choices include:\n',
                  limited_print(supported_databases, type = "silent")))
    }

    # Look up classifications
    result <- stats::setNames(
      unlist(lapply(ids, function(i) {
        taxize::classification(taxize::genbank2uid(i)[1], db = database)
      }), recursive = FALSE),
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

  # Add mapping columns to classification data
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

  if (include_tax_data) {
    # Remove mapping columns from output
    output$data$tax_data[mappping_cols] <- NULL

    # Remove class column from output
    output$data$tax_data[internal_class_name] <- NULL

    # Fix incorrect taxon ids for tax_data
    #   This is due to the "{{index}}" being interpreted as a column name,
    #   which is needed for the user-defined data sets to be parsed right.
    output$data$tax_data$taxon_id <- output$input_ids

    # Remove duplicates from `tax_data`
    output$data$tax_data <- output$data$tax_data[!duplicated(output$data$tax_data), ]
  }

  # Replace standard taxon ids with database taxon ids
  if (use_database_ids) {
    taxon_id_col <- paste0(database, "_id")
    # I am not sure why the following line works...
    new_ids <- unique(combined_class[[taxon_id_col]])[match(output$taxon_ids(),
                                                            unique(output$input_ids))]
    output$replace_taxon_ids(new_ids)
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
      if (is.null(names(data))) {
        return(rep(NA_character_, length(data)))
      } else {
        return(names(data))
      }
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



#' Extracts taxonomy info from vectors with regex
#'
#' Parse taxonomic information in a character vector into a [taxmap()] object.
#' The location and identity of important information in the input is specified
#' using a regular expression with capture groups and a corresponding key. An
#' object of type [taxmap()] is returned containing the specified information.
#' See the `key` option for accepted sources of taxonomic information.
#'
#' @param tax_data A vector from which to extract taxonomy information.
#' @param key (`character`) The identity of the capturing groups defined using
#'   `regex`. The length of `key` must be equal to the number of capturing
#'   groups specified in `regex`. Any names added to the terms will be used as
#'   column names in the output. Only `"info"` can be used multiple times. Each
#'   term must be one of those described below:
#'   * `taxon_id`: A unique numeric id for a taxon for a particular `database`
#'   (e.g. ncbi accession number). Requires an internet connection.
#'   * `taxon_name`: The name of a taxon (e.g. "Mammalia" or "Homo sapiens").
#'   Not necessarily unique, but interpretable by a particular `database`.
#'   Requires an internet connection.
#'   * `class`: A list of taxon information that constitutes the full taxonomic
#'   classification (e.g. "K_Mammalia;P_Carnivora;C_Felidae"). Individual
#'   taxa are separated by the `class_sep` argument and the information is
#'   parsed by the `class_regex` and `class_key` arguments.
#'   * `seq_id`: Sequence ID for a particular database that is associated with a
#'   taxonomic classification. Currently only works with the "ncbi" database.
#'   * `info`: Arbitrary taxon info you want included in the output. Can be used
#'   more than once.
#' @param regex (`character` of length 1) A regular expression with capturing
#'   groups indicating the locations of relevant information. The identity of
#'   the information must be specified using the `key` argument.
#' @param class_key (`character` of length 1) The identity of the capturing
#'   groups defined using `class_regex`. The length of `class_key` must be equal
#'   to the number of capturing groups specified in `class_regex`. Any names
#'   added to the terms will be used as column names in the output. Only
#'   `"info"` can be used multiple times. Each term must be one of those
#'   described below:
#'   * `taxon_name`: The name of a taxon. Not necessarily unique.
#'   * `info`: Arbitrary taxon info you want included in the output. Can be used
#'   more than once.
#' @param class_regex (`character` of length 1)
#'   A regular expression with capturing groups indicating the locations of data
#'   for each taxon in the `class` term in the `key` argument. The identity of
#'   the information must be specified using the `class_key` argument. The
#'   `class_sep` option can be used to split the classification into data for
#'   each taxon before matching. If `class_sep` is `NULL`, each match of
#'   `class_regex` defines a taxon in the classification.
#' @param class_sep (`character` of length 1)
#'   Used with the `class` term in the `key` argument. The character(s) used to
#'   separate individual taxa within a classification. After the string defined
#'   by the `class` capture group in `regex` is split by `class_sep`, its
#'   capture groups are extracted by `class_regex` and defined by `class_key`.
#'   If `NULL`, every match of `class_regex` is used instead with first
#'   splitting by `class_sep`.
#' @param sep_is_regex (`TRUE`/`FALSE`) Whether or not `class_sep` should be
#'   used as a [regular expression](https://en.wikipedia.org/wiki/Regular_expression).
##' @param class_rev (`logical` of length 1)
#'   Used with the `class` term in the `key` argument. If `TRUE`, the order of
#'   taxon data in a classification is reversed to be specific to broad.
#' @param database (`character` of length 1) The name of the database that
#'   patterns given in `parser` will apply to. Valid databases include "ncbi",
#'   "itis", "eol", "col", "tropicos", "nbn", and "none". `"none"` will cause no
#'   database to be quired; use this if you want to not use the internet. NOTE:
#'   Only `"ncbi"` has been tested extensively so far.
#' @param include_match (`logical` of length 1) If `TRUE`, include the part of
#'   the input matched by `regex` in the output object.
#' @param include_tax_data (`TRUE`/`FALSE`) Whether or not to include `tax_data`
#'   as a dataset.
#'
#' @family parsers
#'
#' @return Returns an object of type [taxmap()]
#'
#' @examples \dontrun{
#'
#'   # For demonstration purposes, the following example dataset has all the
#'   # types of data that can be used, but any one of them alone would work.
#'   raw_data <- c(
#'   ">id:AB548412-tid:9689-Panthera leo-tax:K_Mammalia;P_Carnivora;C_Felidae;G_Panthera;S_leo",
#'   ">id:FJ358423-tid:9694-Panthera tigris-tax:K_Mammalia;P_Carnivora;C_Felidae;G_Panthera;S_tigris",
#'   ">id:DQ334818-tid:9643-Ursus americanus-tax:K_Mammalia;P_Carnivora;C_Felidae;G_Ursus;S_americanus"
#'   )
#'
#'   # Build a taxmap object from classifications
#'   extract_tax_data(raw_data,
#'                    key = c(my_seq = "info", my_tid = "info", org = "info", tax = "class"),
#'                    regex = "^>id:(.+)-tid:(.+)-(.+)-tax:(.+)$",
#'                    class_sep = ";", class_regex = "^(.+)_(.+)$",
#'                    class_key = c(my_rank = "info", tax_name = "taxon_name"))
#'
#'   # Build a taxmap object from taxon ids
#'   # Note: this requires an internet connection
#'   extract_tax_data(raw_data,
#'                    key = c(my_seq = "info", my_tid = "taxon_id", org = "info", tax = "info"),
#'                    regex = "^>id:(.+)-tid:(.+)-(.+)-tax:(.+)$")
#'
#'   # Build a taxmap object from ncbi sequence accession numbers
#'   # Note: this requires an internet connection
#'   extract_tax_data(raw_data,
#'                    key = c(my_seq = "seq_id", my_tid = "info", org = "info", tax = "info"),
#'                    regex = "^>id:(.+)-tid:(.+)-(.+)-tax:(.+)$")
#'
#'   # Build a taxmap object from taxon names
#'   # Note: this requires an internet connection
#'   extract_tax_data(raw_data,
#'                    key = c(my_seq = "info", my_tid = "info", org = "taxon_name", tax = "info"),
#'                    regex = "^>id:(.+)-tid:(.+)-(.+)-tax:(.+)$")
#' }
#' @export
extract_tax_data <- function(tax_data, key, regex, class_key = "taxon_name",
                             class_regex = "(.*)", class_sep = NULL,
                             sep_is_regex = FALSE,
                             class_rev = FALSE, database = "ncbi",
                             include_match = FALSE, include_tax_data = TRUE) {
  # Check regex/keys
  key <- validate_regex_key_pair(regex, key, multiple_allowed = "info")
  class_key <- validate_regex_key_pair(class_regex, class_key, multiple_allowed = "info")
  # classification sep
  if (!is.null(class_sep) && (class(class_sep) != "character" | length(class_sep) != 1)) {
    stop('"class_sep" must be a character vector of length 1 or NULL')
  }
  # Boolean options
  if (class(class_rev) != "logical" | length(class_rev) != 1) {
    stop('"class_rev" must be TRUE/FALSE')
  }
  if (class(include_match) != "logical" | length(include_match) != 1) {
    stop('"include_match" must be TRUE/FALSE')
  }
  if (class(include_tax_data) != "logical" | length(include_tax_data) != 1) {
    stop('"include_tax_data" must be TRUE/FALSE')
  }
  # database
  valid_databases <- c(names(database_list), "none")
  if (! database %in% valid_databases) {
    stop(paste0('Unknown database "', database,
                '". Accepted database names include:\n    "',
                paste0(valid_databases, collapse = ", ")))
  }

  # Extract capture groups
  parsed_input <- data.frame(stringr::str_match(tax_data, regex), stringsAsFactors = FALSE)
  colnames(parsed_input) <- c("input", names(key))

  # Use parse_tax_data if the input is a classification
  if ("class" %in% key) {
    output <- parse_tax_data(tax_data = parsed_input,
                             class_cols = which(key == "class") + 1,
                             class_sep = class_sep, class_key = class_key,
                             sep_is_regex = sep_is_regex,
                             class_regex = class_regex,
                             include_match = include_match,
                             include_tax_data = include_tax_data)
    if (!include_match) {
      output$data$tax_data$match <- NULL
    }
  }


  # Use lookup_tax_data for taxon names, ids, and sequence ids
  if (any(key %in% c("taxon_name", "taxon_id", "seq_id"))) {
    my_type <- key[key != "info"][1]
    output <- lookup_tax_data(tax_data = parsed_input, type = my_type,
                              column = which(key == my_type) + 1,
                              database = database,
                              include_tax_data = include_tax_data)

    if (!include_match) {
      output$data$query_data$match <- NULL
    }
  }

  return(output)
}



#' Check that all match input
#'
#' Ensure that all of a character vector matches a regex.
#' Inputs that do not match are excluded.
#'
#' @param input (\code{character})
#' @param regex (\code{character} of length 1)
#' @param max_print  (\code{numeric} of length 1)
#' The maximum number of lines to print in error/warning messages.
#'
#' @return \code{character} Parts of \code{input} matching \code{regex}
#'
#' @keywords internal
validate_regex_match <- function(input, regex, max_print = 10) {
  # check which input values match
  input <- as.character(input)
  not_matching <- ! grepl(pattern = regex, x = input)
  # complain about those that dont
  if (sum(not_matching) > 0) {
    invalid_list <- paste("   ", which(not_matching), ": ", input[not_matching],
                          "\n")
    if (length(invalid_list) > max_print) {
      invalid_list <- c(invalid_list[1:max_print], "    ...")
    }
    warning(paste0(collapse = "",
                   c("The following ", sum(not_matching), " of ", length(input),
                     " input(s) could not be matched by the regex supplied:\n",
                     invalid_list)))
  }
  # return matching inputs
  return(input[! not_matching])
}


#' Check a regex-key pair
#'
#' Checks that the number of capture groups in the regex matches the length of the key.
#' Checks that only certain values of \code{key} can appear more that once.
#' Adds names to keys that will be used for column names in the output of \code{extract_taxonomy}.
#' Uses non-standard evaluation to get the name of input variables.
#'
#' @param regex (\code{character})
#' A regex with capture groups
#' @param key (\code{character})
#' A key corresponding to \code{regex}
#' @param multiple_allowed (\code{character})
#' Values of \code{key_options} that can appear more than once.
#'
#' @return Returns the result of \code{\link{match.arg}} on the key.
#'
#' @keywords internal
#' @rdname validate_regex_key_pair
validate_regex_key_pair <- function(regex, key, multiple_allowed) {

  # Non-standard evaluation
  regex_var_name <- deparse(substitute(regex))
  key_var_name <- deparse(substitute(key))

  # Check that the keys used are valid
  allowed <- c("taxon_id", "taxon_name", "info", "class", "seq_id")
  invalid_keys <- key[! key %in% allowed]
  if (length(invalid_keys) > 0) {
    stop(paste0('Invalid key value "', invalid_keys[1], '" given.\n',
                'Valid options are: ', paste0(collapse = ", ", allowed)))
  }

  # Check key length
  regex_capture_group_count <- count_capture_groups(regex)
  key_length <- length(key)
  if (key_length != regex_capture_group_count) {
    stop(paste0(collapse = "",
                'The number of capture groups in "', regex_var_name, '" and the length of "',
                key_var_name, '" do not match.\n',
                'The key has ', key_length, ' term(s) and the regex has ', regex_capture_group_count))
  }

  # Check that only keys in `multiple_allowed` appear more than once
  counts <- table(key)
  duplicated_keys <- names(counts[counts > 1])
  invalid_duplicates <- duplicated_keys[! duplicated_keys %in% multiple_allowed]
  if (length(invalid_duplicates) > 0) {
    stop(paste0(collapse = "",
                'The following values in `', key_var_name, '` have been used more than once: ',
                paste0(collapse =", ", invalid_duplicates), '\n',
                '  Only the following keys can be duplicated: ',
                paste0(collapse =", ", multiple_allowed)))
  }

  # Apply key name defaults
  key_names <- names(key)
  if (is.null(key_names)) { key_names <- rep("", length(key)) }
  key_names[key_names == ""] <- key[key_names == ""]

  names(key) <- key_names
  return(key)
}

#' Count capture groups
#'
#' Count the number of capture groups in a regular expression.
#'
#' @param regex (\code{character} of length 1)
#'
#' @return \code{numeric} of length 1
#'
#' @source http://stackoverflow.com/questions/16046620/regex-to-count-the-number-of-capturing-groups-in-a-regex
#'
#' @keywords internal
count_capture_groups <- function(regex) {
  new_regex <- paste0(regex, "|") # Allow the regex to match nothing
  ncol(stringr::str_match(string = "", pattern = new_regex)) - 1
}

