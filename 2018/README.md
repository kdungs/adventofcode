# Advent of Code 2018

https://adventofcode.com

## Set up

Since we will be doing the exercises in C++, it makes sense to have a nice C++
environment set up. From experience, [bazel](https://bazel.io), while having a
small barrier to entry, is the most convenient build system in the long run. It
also supports other languages.


### Prerequisites

You need to have
[XCode](https://itunes.apple.com/de/app/xcode/id497799835?l=en&mt=12) and
homebrew installed.


### Bazel

In order to install [bazel](https://bazel.io) and test the installation on your
Mac, run the following commands in the project directory. 

```bash
brew install bazel
bazel clean --expunge
sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
sudo xcodebuild -license
bazel clean --expunge
bazel test kd:day1_test --test_output=errors
bazel run -c opt kd:day1_main < inputs/day1
```


### Your code

Create a directory with your initials. For the project layout, have a look at
the `kd` directory.


## C++

 * https://google.github.io/styleguide/cppguide.html
