# pepr 0.0.5

## TBA

## Added

* add `activateSubproject` method

## Changed

* if the `subproject` argument of the `Project()` function is not present in the config, the original project is returned
* paths in the `bioconductor` section of the config are made aboslute and environment varaiables are read
* no sample annoptation is allowed if any suprojects are defined in the config


# pepr 0.0.4

## 2018-11-14

## Added

## Changed

* change the `Project` object construction, the subproject can be activated at construction time
* change `implied/derived_columns` to `implied/derived_attributes`. Backwards compatible
* change `constants` to `constantAttributes`. Backwards compatible
* fix `expandPath()` function, add error when environment variable not found


# pepr 0.0.3

## 2018-09-12

## Added

* add `derived_columns` functionality
* add `implied_columns` functionality
* add `subannotation` functionality
	
## Changed

# pepr 0.0.2 

## 2018-09-06

## Added

* first release, includes basic [PEP](https://pepkit.github.io/) reading functions

## Changed

