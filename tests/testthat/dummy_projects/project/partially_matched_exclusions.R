# nolint start: assign.
a = 2
# nolint end

# nolint start: s. warn (and lint) because of non-unique identifier
x <- 42; y <- 2
# nolint end

# nolint start: bogus_linter. warn because of nonexistent identifier

# nolint end

# nolint: hocus_pocus, bogus.
