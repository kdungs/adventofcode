# adventofcode
My solutions to the Advent of Code challenge. I use C++14 with the
[`range-v3`](https://github.com/ericniebler/range-v3) library from Eric
Niebler. The idea is not to get the solutions out as quickly as possible but
rather take some pensive time in advent to play around with the library and get
used to it.

Until I realized time was short and started solving some of them using Python.

The input files are not provided in order to prevent clutter in the repository.
Also, as far as I understand, they are different for each participant. In some
solutions I got sloppy and hard-coded the input in. _This will be fixed once I
clean up this repository_.


## Compilation
Adjust the Makefile to contain the path to `range-v3` on your system. Then it's
as easy as `make`. All binary files end up in a folder called `bin` and take a
file name as input.


## Exceptions

 * `04.cc` requires OpenSSL. The linker looks for `libcrypto` in the global
   library path.
 * `06.cc` requires `boost::regex`. The linker looks for `libboost_regex-mt` in
   the global library path. _This will be replaced by `std::regex` once I clean
   up this repository. In all other instances `std::regex` is used._
