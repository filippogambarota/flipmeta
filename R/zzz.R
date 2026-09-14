.onAttach <- function(libname, pkgname) {
    msg <- sprintf("Welcome to %s v%s! :)", pkgname, utils::packageVersion(pkgname))
    packageStartupMessage(msg)
}
