june <- 'ecotox_ascii_06_11_2026.zip'
september <- 'ecotox_ascii_09_15_2026.zip'

embedded_lifestage_v2 <- function() {
  # Reads the file, not read_release_table(), which the tests mock.
  path <- file.path(find_release_dir(), 'lifestage_dictionary.rds')
  'eco_group' %in% names(readRDS(path))
}

# Replace release_metadata() with a copy whose lifestage releases use `schema`.
local_lifestage_schema <- function(schema, env = parent.frame()) {
  metadata <- release_metadata()
  metadata$ecotox$schema_version <- schema
  metadata$ecotox$species_groups <- metadata$ecotox$species_groups %||%
    list(table = 'lifestage_species_groups', applies_to = list(june, september))
  for (r in names(metadata$ecotox$additional_releases)) {
    metadata$ecotox$additional_releases[[r]]$schema_version <- schema
  }
  testthat::local_mocked_bindings(release_metadata = function() metadata, .env = env)
}

# Schema 2 stand-in while the embedded data is schema 1: each dictionary row is
# repeated for every eco group, and hatch for Birds becomes Juvenile, so a join
# that ignores eco_group is detectable. Species 1, 2 and 3 are Fish, Birds and
# Unknown. Schema 2 data passes through unchanged.
local_lifestage_v2 <- function(env = parent.frame()) {
  local_lifestage_schema('2.0.0', env)
  read <- read_release_table
  testthat::local_mocked_bindings(
    read_release_table = function(name) {
      if (name == 'lifestage_species_groups' &&
        !file.exists(file.path(find_release_dir(), 'lifestage_species_groups.rds'))) {
        return(tibble::tibble(
          species_number = c('1', '2', '3'),
          eco_group = c('Fish', 'Birds', 'Unknown')
        ))
      }
      table <- read(name)
      if (!startsWith(name, 'lifestage_dictionary') || 'eco_group' %in% names(table)) {
        return(table)
      }
      out <- table[rep(seq_len(nrow(table)), each = length(ECO_GROUPS)), ]
      out$eco_group <- rep(ECO_GROUPS, nrow(table))
      hatch <- out$org_lifestage == 'New, newly or recent hatch' & out$eco_group == 'Birds'
      out$harmonized_life_stage[hatch] <- 'Juvenile'
      out
    },
    .env = env
  )
}
