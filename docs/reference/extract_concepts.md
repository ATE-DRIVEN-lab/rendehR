# Extract atomic and handcrafted concepts for the configured source

Extract atomic and handcrafted concepts for the configured source

## Usage

``` r
extract_concepts(cfg, user_concepts = NULL, ...)
```

## Arguments

- cfg:

  Config list from ehr_load_config().

- user_concepts:

  Optional path to a supplemental concept CSV. User rows override
  package concept definitions with the same concept_name.

- ...:

  Additional arguments passed to run_concepts().

## Value

Invisibly NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- ehr_load_config("config/ehr_config.json")
extract_concepts(cfg)
} # }
```
