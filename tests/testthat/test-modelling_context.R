test_that("modelling-context works", {
    mc1 <- list(
        calendars = list(),
        variables = list(
            v1 = list(x1 = AirPassengers),
            v2 = list(
                DriversKilled = Seatbelts[, "DriversKilled"],
                drivers = Seatbelts[, "drivers"]
            )
        )
    )
    mc2 <- list(
        calendars = list(),
        variables = list(
            v1 = list(
                X0_2_09_10_M = ABS$X0.2.09.10.M,
                X0_2_08_10_M = ABS$X0.2.08.10.M
            )
        )
    )
    mc3 <- list(
        calendars = list(),
        variables = list(v1 = list(x1 = AirPassengers))
    )
    mc4 <- list(
        calendars = list(),
        variables = list(r = list(x1 = AirPassengers))
    )

    expect_no_message(expect_identical(
        object = modelling_context(
            variables = list(
                v1 = list(x1 = AirPassengers),
                v2 = Seatbelts[, 1:2]
            )
        ),
        mc1
    ))
    expect_message(expect_identical(
        object = modelling_context(variables = list(v1 = ABS[, 1:2])),
        mc2
    ))
    expect_message(expect_identical(
        object = modelling_context(variables = list(v1 = AirPassengers)),
        mc3
    ))
    expect_message(expect_identical(
        object = modelling_context(variables = list(v1 = AirPassengers)),
        mc3
    ))
    expect_message(expect_identical(
        object = modelling_context(variables = list(v1 = list(AirPassengers))),
        mc3
    ))
    expect_no_message(expect_identical(
        object = modelling_context(variables = list(list(x1 = AirPassengers))),
        mc4
    ))
    expect_message(expect_identical(
        object = modelling_context(variables = list(AirPassengers)),
        mc4
    ))
})

test_that("modelling-context works", {
    mc1 <- list(
        calendars = list(),
        variables = list(
            v1 = list(x1 = AirPassengers)
        )
    )
    mc2 <- list(
        calendars = list(),
        variables = list(
            v1 = list(x1 = AirPassengers),
            v2 = list(x1 = AirPassengers)
        )
    )

    expect_no_message(object = {
        my_context <- complete_modelling_context(
            modelling_context = mc1,
            y = AirPassengers,
            group = "v2",
            name = "x1"
        )
    })

    expect_message(object = {
        my_context <- complete_modelling_context(
            modelling_context = my_context,
            y = AirPassengers,
            group = "v2",
            name = "x1"
        )
    })

    expect_no_message(object = {
        my_context <- complete_modelling_context(
            modelling_context = my_context,
            y = AirPassengers,
            group = "v2",
            name = "x1",
            overwrite = TRUE
        )
    })

    expect_identical(mc2, my_context)
})

test_that("replace_wrong_names works", {
    expect_null(replace_wrong_names(x = NULL))
})

test_that("complete_names works", {
    expect_null(complete_names(x = NULL))

    expect_message({
        out1 <- complete_names(x = list(1, 2, 3))
    })
    expect_identical(out1, list(x1 = 1, x2 = 2, x3 = 3))
})

test_that("format_regressor stops", {
    expect_error(format_regressor(list()))
    expect_error(format_regressor(1L))
})

test_that("format_variable works", {
    expect_null(format_variable(list()))

    expect_message({
        out1 <- format_variable(list(AirPassengers))
    })
    expect_message({
        out2 <- format_variable(AirPassengers)
    })
    expect_identical(out1, list(x1 = AirPassengers))
    expect_identical(out2, list(x1 = AirPassengers))
})
