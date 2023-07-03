# adaptalint

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/adaptalint
* Date/Publication: 2019-10-05 04:10:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::revdep_details(, "adaptalint")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘adaptalint-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: extract_style
    > ### Title: Find style of a file
    > ### Aliases: extract_style
    > 
    > ### ** Examples
    > 
    > 
    ...
     12.         ├─tibble:::subclass_name_repair_errors(...)
     13.         │ └─base::withCallingHandlers(...)
     14.         └─vctrs::vec_as_names(...)
     15.           └─vctrs (local) `<fn>`()
     16.             └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     17.               └─vctrs:::stop_names_cannot_be_empty(names, call = call)
     18.                 └─vctrs:::stop_names(...)
     19.                   └─vctrs:::stop_vctrs(...)
     20.                     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("adaptalint")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 11 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-basics.R:29:3'): extract works in simple case ──────────────────
      <tibble_error_column_names_cannot_be_empty/tibble_error/rlang_error/error/condition>
      Error in `as_tibble(.)`: Columns 1, 2, 3, 4, 5, and 14 more must be named.
      Use `.name_repair` to specify repair.
      Caused by error in `repaired_names()`:
      ! Names can't be empty.
      ✖ Empty names found at locations 1, 2, 3, 4, 5, etc.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

# fstcore

<details>

* Version: 0.9.14
* GitHub: https://github.com/fstpackage/fst
* Source code: https://github.com/cran/fstcore
* Date/Publication: 2023-01-12 09:00:12 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::revdep_details(, "fstcore")` for more info

</details>

## Newly broken

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        libs   4.9Mb
    ```

## Newly fixed

*   checking whether package ‘fstcore’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/michael/github/lintr/revdep/checks/fstcore/old/fstcore.Rcheck/00install.out’ for details.
    ```

# simaerep

<details>

* Version: 0.4.3
* GitHub: https://github.com/openpharma/simaerep
* Source code: https://github.com/cran/simaerep
* Date/Publication: 2023-03-03 10:30:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::revdep_details(, "simaerep")` for more info

</details>

## Newly broken

*   checking whether package ‘simaerep’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/michael/github/lintr/revdep/checks/simaerep/new/simaerep.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘simaerep’ ...
** package ‘simaerep’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
Warning in FUN(X[[i]], ...) : internal error -3 in R_decompress1
*** copying figures
...
** building package indices
Warning in FUN(X[[i]], ...) : internal error -3 in R_decompress1
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
Error in base::serialize(base::as.list(base::getNamespace("simaerep"),  : 
  error writing to connection
Calls: <Anonymous> -> withCallingHandlers -> <Anonymous>
Execution halted
ERROR: loading failed
* removing ‘/home/michael/github/lintr/revdep/checks/simaerep/new/simaerep.Rcheck/simaerep’


```
### CRAN

```
* installing *source* package ‘simaerep’ ...
** package ‘simaerep’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (simaerep)


```
# stencilaschema

<details>

* Version: 1.0.0
* GitHub: https://github.com/stencila/schema
* Source code: https://github.com/cran/stencilaschema
* Date/Publication: 2021-02-22 10:40:03 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::revdep_details(, "stencilaschema")` for more info

</details>

## Newly broken

*   checking R files for non-ASCII characters ... WARNING
    ```
    Found the following file with non-ASCII characters:
      Fatal error: cannot create 'R_TempDir'
    Portable packages must use only ASCII characters in their R code,
    except perhaps in comments.
    Use \uxxxx escapes for other characters.
    ```

*   checking R files for syntax errors ... WARNING
    ```
    Fatal error: cannot create 'R_TempDir'
    ```

## Newly fixed

*   checking Rd files ... WARNING
    ```
    Any.Rd: Sections \title, and \name must exist and be unique in Rd files
    Array.Rd: Sections \title, and \name must exist and be unique in Rd files
    ArrayValidator.Rd: Sections \title, and \name must exist and be unique in Rd files
    Article.Rd: Sections \title, and \name must exist and be unique in Rd files
    AudioObject.Rd: Sections \title, and \name must exist and be unique in Rd files
    BlockContent.Rd: Sections \title, and \name must exist and be unique in Rd files
    BooleanValidator.Rd: Sections \title, and \name must exist and be unique in Rd files
    Brand.Rd: Sections \title, and \name must exist and be unique in Rd files
    Cite.Rd: Sections \title, and \name must exist and be unique in Rd files
    CiteGroup.Rd: Sections \title, and \name must exist and be unique in Rd files
    ...
    Grant.Rd: Sections \title, and \name must exist and be unique in Rd files
    GrantTypes.Rd: Sections \title, and \name must exist and be unique in Rd files
    Heading.Rd: Sections \title, and \name must exist and be unique in Rd files
    ImageObject.Rd: Sections \title, and \name must exist and be unique in Rd files
    Include.Rd: Sections \title, and \name must exist and be unique in Rd files
    InlineContent.Rd: Sections \title, and \name must exist and be unique in Rd files
    IntegerValidator.Rd: Sections \title, and \name must exist and be unique in Rd files
    Link.Rd: Sections \title, and \name must exist and be unique in Rd files
    List.Rd: Sections \title, and \name must exist and be unique in Rd files
    problems found in ‘Any.Rd’, ‘Array.Rd’, ‘ArrayValidator.Rd’, ‘Article.Rd’, ‘AudioObject.Rd’, ‘BlockContent.Rd’, ‘BooleanValidator.Rd’, ‘Brand.Rd’, ‘Cite.Rd’, ‘CiteGroup.Rd’, ‘Code.Rd’, ‘CodeBlock.Rd’, ‘CodeFragment.Rd’, ‘ConstantValidator.Rd’, ‘ContactPoint.Rd’, ‘CreativeWorkTypes.Rd’, ‘Datatable.Rd’, ‘DatatableColumn.Rd’, ‘Date.Rd’, ‘DefinedTerm.Rd’, ‘Delete.Rd’, ‘Emphasis.Rd’, ‘Entity.Rd’, ‘EntityTypes.Rd’, ‘Enum.Rd’, ‘EnumValidator.Rd’, ‘Figure.Rd’, ‘Function.Rd’, ‘Grant.Rd’, ‘GrantTypes.Rd’, ‘Heading.Rd’, ‘ImageObject.Rd’, ‘Include.Rd’, ‘InlineContent.Rd’, ‘IntegerValidator.Rd’, ‘Link.Rd’, ‘List.Rd’
    ```

