# This file is in 'exclusions' & nothing lints under R config.
# Under DCF config, '# SKIP_LINT' is the exclusion & this line won't lint
1+1 # SKIP_LINT
# This is included as a linter in the DCF, thus this should lint
expect_equal(foo(x), NULL)

# trailing blank line next will lint under DCF

