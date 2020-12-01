cc_library(
  name = "day01",
  hdrs = ["day01.h"],
  srcs = ["day01.cc"],
  deps = [],
)

cc_test(
  name = "day01_test",
  srcs = ["day01_test.cc"],
  deps = [
    ":day01",
    "@com_google_googletest//:gtest_main",
  ],
)

cc_binary(
  name = "day01_main",
  srcs = ["day01_main.cc"],
  deps = [
    ":day01",
  ],
)
