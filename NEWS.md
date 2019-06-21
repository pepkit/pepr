# pepr 0.2.1 - 2019-06-21

## Changed

* `checkSection` method can be used with a mixture of section names _and_ indices.

# pepr 0.2 - 2019-04-17

## Changed

* keys in the config file: `sample_annotation` to `sample_table`, `sample_subannotation` to `subsample_table`. Backwards compatibility is preserved.

# pepr 0.1 - 2019-02-01

## Added

* add `activateSubproject` method
* add `fetchSamples` function
* add `checkSection` method on `Config` object

## Changed

* if the `subproject` argument of the `Project()` function is not present in the config, the original project is returned
* paths in the `bioconductor` section of the config are made aboslute and environment varaiables are read
* no sample annotation is allowed if any suprojects are defined in the config
* fixed the problem with paths expansions in sample subannotaitons case


# pepr 0.0.4 - 2018-11-14

## Changed

* change the `Project` object construction, the subproject can be activated at construction time
* change `implied/derived_columns` to `implied/derived_attributes`. Backwards compatible
* change `constants` to `constantAttributes`. Backwards compatible
* fix `expandPath()` function, add error when environment variable not found


# pepr 0.0.3 - 2018-09-12

## Added

* add `derived_columns` functionality
* add `implied_columns` functionality
* add `subannotation` functionality
	
#  pepr 0.0.2 - 2018-09-06

## Added

* first release, includes basic [PEP](https://pepkit.github.io/) reading functions