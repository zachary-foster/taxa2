#' Check regex validity
#'
#' Check if a regular expression is valid
#'
#' @param text The putative regex to check.
#'
#' @keywords internal
is_valid_regex <- function(text)
{
  out <- suppressWarnings(try(grepl(pattern = text, "x"), silent = TRUE))
  return(! inherits(out, "try-error"))
}



#' @keywords internal
TaxonDatabase <- R6::R6Class(
  "TaxonDatabase",
  public = list(

    initialize = function(
      name = NULL, url = NULL, description = NULL, id_regex = NULL
    ) {
      self$name <- name
      self$url <- url
      self$description <- description
      self$id_regex <- id_regex
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<database> %s\n", char_or_placeholder(self$name))))
      cat(paste0(indent, paste0("  url: ", char_or_placeholder(self$url), "\n")))
      cat(paste0(indent, paste0("  description: ", char_or_placeholder(self$description), "\n")))
      cat(paste0(indent, paste0("  id regex: ", char_or_placeholder(self$id_regex), "\n")))
      invisible(self)
    },

    is_valid_id = function(id) {
      if (is.null(self$id_regex)) {
        return(TRUE)
      } else {
        return(grepl(id, pattern = self$id_regex))
      }
    }

  ),


  active = list(

    name = function(value) {
      if (missing(value)) { # GET
        return(private$my_name)
      }
      else { # SET
        if (is.null(value)) {
          private$my_name <- NULL
        } else {
          # check_arg_class(value, c("character", "factor", "TaxonDatabase"), "database name")
          private$my_name <- as.character(value)
        }
      }
    },

    url = function(value) {
      if (missing(value)) { # GET
        return(private$my_url)
      }
      else { # SET
        if (is.null(value)) {
          private$my_url <- NULL
        } else {
          # check_arg_class(value, c("character", "factor"), "database url")
          private$my_url <- as.character(value)
        }
      }
    },

    description = function(value) {
      if (missing(value)) { # GET
        return(private$my_description)
      }
      else { # SET
        if (is.null(value)) {
          private$my_description <- NULL
        } else {
          # check_arg_class(value, c("character", "factor"), "database description")
          private$my_description <- as.character(value)
        }
      }
    },

    id_regex = function(value) {
      if (missing(value)) { # GET
        return(private$my_id_regex)
      }
      else { # SET
        if (is.null(value)) {
          private$my_id_regex <- NULL
        } else {
          # check_arg_class(value, c("character", "factor"), '"id_regex"')
          value <- as.character(value)
          if (!is_valid_regex(value)) {
            stop(call. = FALSE, '"id_regex" must be a valid regular expression. "', value, '" is not a valid regular expression according to R.')
          }
          private$my_id_regex <- value
        }
      }
    },

    valid_ranks = function(value) {
      if (missing(value)) { # GET
        return(private$my_valid_ranks)
      }
      else { # SET
        if (is.null(value)) {
          private$my_valid_ranks <- NULL
        } else {
          check_arg_class(value, c("character", "factor"), '"valid_ranks"')
          if (is.factor(value) && ! is.ordered(value)) {
            value <- as.character(value)
          }
          private$my_valid_ranks <- value
        }
      }
    }

  ),


  private = list(
    my_name = NULL,
    my_url = NULL,
    my_description = NULL,
    my_id_regex = NULL,
    my_valid_ranks = NULL
  )

)


#' Database list
#'
#' The list of known databases. Not currently used much, but will be when we add
#' more check for taxon IDs and taxon ranks from particular databases.
#'
#' @keywords internal
database_list <- list(
  ncbi = TaxonDatabase$new(
    "ncbi",
    "http://www.ncbi.nlm.nih.gov/taxonomy",
    "NCBI Taxonomy Database",
    ".*"
  ),

  gbif = TaxonDatabase$new(
    "gbif",
    "http://www.gbif.org/developer/species",
    "GBIF Taxonomic Backbone",
    ".*"
  ),

  bold = TaxonDatabase$new(
    "bold",
    "http://www.boldsystems.org",
    "Barcode of Life",
    ".*"
  ),

  col = TaxonDatabase$new(
    "col",
    "http://www.catalogueoflife.org",
    "Catalogue of Life",
    ".*"
  ),

  eol = TaxonDatabase$new(
    "eol",
    "http://eol.org",
    "Encyclopedia of Life",
    ".*"
  ),

  nbn = TaxonDatabase$new(
    "nbn",
    "https://nbn.org.uk",
    "UK National Biodiversity Network",
    ".*"
  ),

  tps = TaxonDatabase$new(
    "tps",
    "http://www.tropicos.org/",
    "Tropicos",
    ".*"
  ),

  itis = TaxonDatabase$new(
    "itis",
    "http://www.itis.gov",
    "Integrated Taxonomic Information System",
    "[0-9]+"
  )
)

#' Defines valid taxonomic databases
#'
#' @param name (character) name of the database
#' @param url (character) url for the database
#' @param description (character) description of the database
#' @param id_regex (character) id regex
#'
#' @section Attribution:
#'
#' This code is copied from the code handling options in [knitr].
#'
#' @keywords internal
default_database_definitions <- function(defaults = list()) {
  definitions <- defaults

  merge_list = function(x, y) {
    x[names(y)] = y
    x
  }

  get <- function(name = NULL) {
    if (is.null(name)) {
      return(definitions)
    } else {
      if (name %in% names(definitions)) {
        return(definitions[[name]])
      } else {
        stop(call. = FALSE, 'Unknown database: "', name, '"')
      }
    }
  }

  set <- function(name = NULL, url = NULL, description = NULL, id_regex = NULL) {
    addition <- list(TaxonDatabase$new(
      name = name,
      url = url,
      description = description,
      id_regex = id_regex
    ))
    names(addition) <- name
    definitions <<- merge_list(definitions, addition)
  }

  reset <- function() {
    definitions <<- initial_value
  }

  list(get = get, set = set, reset = reset)
}


#' Valid taxonomy databases
#'
#' This defines the valid taxonomic databases that can be used in `taxa_database` objects as a list of
#'
#' @param name (character) name of the database
#' @param url (character) url for the database
#' @param description (character) description of the database
#' @param id_regex (character) id regex
#'
#' @section Attribution:
#'
#' This code is based on the code handling options in [knitr].
#'
#' @examples
#'
#' # List all database definitions
#' database_definitions$get()
#'
#' # Get a specific database definition
#' database_definitions$get('ncbi')
#'
#' # Add or overwrite a database definition
#' database_definitions$set(
#'   name = "my_new_database",
#'   url = "http://www.my_tax_database.com",
#'   description = "I just made this up",
#'   id_regex = ".*"
#' )
#'
#' # Reset definitions to default values
#' database_definitions$reset()
#'
#' @export
database_definitions <- default_database_definitions(database_list)



