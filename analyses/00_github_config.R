##### Github configuration

## Install usethis package ----
install.packages("usethis")
library(usethis)

usethis::use_git()

## Open ~/.Renviron file ----
usethis::edit_r_environ()
# • Modify '~/.Renviron'
# • Restart R for changes to take effect

## Open ~/.Rprofile file ----
usethis::edit_r_profile()
# • Modify '~/.Rprofile'
# • Restart R for changes to take effect

## Edit .gitignore file
usethis::edit_git_ignore(scope = "project")


# The .Renviron file is a good place to store secrets (API tokens, password, etc.). This file is read by at startup. To access these variables, you can use the function Sys.getenv("GITHUB_PAT").
# The .Rprofile file allows you to run command lines at startup. You can customize the default behaviour of and define some variables with options(). To access these variables, you can use options()$"usethis.protocol".

## Installation de 'gert' ----
install.packages("gert")
library(gert)

## Configuration de git ----
gert::git_config_global()
