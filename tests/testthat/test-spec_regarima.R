test_that("add_usrdefvar works", {
    sp0 <- x13_spec_default$regarima
    sp1 <- add_usrdefvar(sp0, name = c("A", "B", "C"))
    expect_length(sp1$regression$users, 3L)
    expect_identical(sp1$regression$users[[1L]]$name, "r.A")
    expect_identical(sp1$regression$users[[3L]]$id, "r.C")
})
