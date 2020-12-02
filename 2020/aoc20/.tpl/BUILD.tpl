cc_library(
    name = "day${day}",
    srcs = ["day${day}.cc"],
    hdrs = ["day${day}.h"],
    deps = [],
)

cc_test(
    name = "day${day}_test",
    srcs = ["day${day}_test.cc"],
    deps = [
        ":day${day}",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_binary(
    name = "day${day}_main",
    srcs = ["day${day}_main.cc"],
    deps = [
        ":day${day}",
    ],
)
