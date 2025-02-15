# config excludes assignment_linter() so this doesn't lint
a = 1
# default config includes infix_spaces_linter() so this lints
b=a + 2
# config extends defaults with any_duplicated_linter() so this lints
any(duplicated(b))
# custom exclude setting is also picked up so this doesn't lint
1+1 # NOLINT
