# use_lintr add .lintr to .Rbuildignore for packages

    Code
      cat(brio::read_file(file.path(tmp_package_dir, ".Rbuildignore")))
    Output
      ^lintr\.Rproj$
      ^\.Rproj\.user$
      ^\.lintr$

