#define CATCH_CONFIG_MAIN

#include <lz/lz.h>

#include <catch2/catch.hpp>
#include <cstdlib>
#include <fstream>
#include <string>

static inline const char *hello() { return "hello"; }

static inline decltype(auto) range(int start = 0) {
  return [n = start]() mutable { return n++; };
}

static inline lz::OptionOrRef<std::ifstream> openNotExisted() {
  std::ifstream file("not existed file");
  if (file) {
    return std::move(file);
  }
  return lz::nullopt;
}

static inline lz::OptionOrRef<std::ifstream> openAndSeek10() {
  std::ifstream file(__FILE__);
  if (file) {
    file.seekg(10);
    return std::move(file);
  }
  return lz::nullopt;
}

struct NoCopy {
  std::string s;

  NoCopy() = default;
  NoCopy(NoCopy &&) = default;
  NoCopy &operator=(NoCopy &&) = default;

  NoCopy(const NoCopy &) = delete;
  NoCopy &operator=(const NoCopy &) = delete;
};

static inline lz::OptionOrRef<NoCopy> noCopy() {
  NoCopy nc;
  nc.s = "no copy";

  auto n = std::rand() % 100;
  if (n < 50) {
    return nc;
  }
  if (n < 80) {
    return NoCopy{s : nc.s};
  }
  return lz::nullopt;
}

static inline decltype(auto) noCopyByConstRef() {
  return [nc = NoCopy{s : "no copy"}]() -> lz::OptionOrRef<const NoCopy &> {
    auto n = std::rand() % 100;
    if (n < 50) {
      return std::cref(nc);
    }
    return lz::nullopt;
  };
}

static inline decltype(auto) noCopyByRef() {
  return [nc = NoCopy{s : "no copy"}]() mutable -> lz::OptionOrRef<NoCopy &> {
    auto n = std::rand() % 100;
    if (n < 50) {
      return std::ref(nc);
    }
    return lz::nullopt;
  };
}

static inline decltype(auto) getline(std::ifstream &file) {
  std::string line;
  while (file) {
    char c;
    if (file.get(c)) {
      line += c;
      if (c == '\n') {
        break;
      }
    }
  }

  return line;
}

static inline decltype(auto) getOneLine(const char *filename) {
  return [file = std::ifstream(filename),
          lines = std::vector<std::string>{}]() mutable
         -> lz::OptionOrRef<const std::vector<std::string> &> {
    if (!file) {
      return lz::nullopt;
    }

    lines.emplace_back(getline(file));
    return std::cref(lines);
  };
}

static inline decltype(auto) getAllLines(const char *filename) {
  return [filename]() -> lz::OptionOrRef<std::vector<std::string>> {
    auto file = std::ifstream(filename);
    if (!file) {
      return lz::nullopt;
    }

    std::vector<std::string> lines;
    while (file) {
      lines.emplace_back(getline(file));
    }
    return std::move(lines);
  };
}

static inline std::string readall(const char *filename) {
  std::stringstream ss;
  auto file = std::ifstream(filename);
  if (file) {
    ss << file.rdbuf();
    return std::move(ss.str());
  }
  return {};
}

SCENARIO("entry is a function returning T/OptionOrRef<T> without arguments",
         "[entry]") {
  GIVEN("() -> a") {
    WHEN("have an C-style function returns any value") {
      auto d0 = hello | lz::limit(0);
      auto d1 = hello | lz::limit(1);
      auto d2 = hello | lz::limit(10);

      THEN("got the last value") {
        REQUIRE(!d0);
        REQUIRE(!!d1);
        REQUIRE(!!d2);
        REQUIRE(strcmp(*d1, *d2) == 0);
      }
    }

    WHEN("have wrapped the C-style function by gen") {
      auto d0 = lz::gen(hello) | lz::limit(0);
      auto d1 = lz::gen(hello) | lz::limit(1);
      auto d2 = lz::gen(hello) | lz::limit(10);

      THEN("got same results") {
        REQUIRE(!d0);
        REQUIRE(!!d1);
        REQUIRE(!!d2);
        REQUIRE(strcmp(*d1, *d2) == 0);
      }
    }
  }

  GIVEN("(a) -> () -> b") {
    WHEN("have a range generator") {
      auto d0 = range(1) | lz::limit(0);
      auto d1 = range(1) | lz::limit(10);

      THEN("reaches the expected number") {
        REQUIRE(!d0);
        REQUIRE(!!d1);
        REQUIRE(*d1 == 10);
      }
    }

    WHEN("have wrapped the range lambda by gen") {
      auto d0 = lz::gen(range(1)) | lz::limit(0);
      auto d1 = lz::gen(range(1)) | lz::limit(10);

      THEN("got same results") {
        REQUIRE(!d0);
        REQUIRE(!!d1);
        REQUIRE(*d1 == 10);
      }
    }
  }

  GIVEN("() -> OptionalOrRef<T>") {
    WHEN("have functions returned a non-copyable local value") {
      THEN("got an ordinary refrence (of safety member data) if possible") {
        auto f0 = openAndSeek10 | lz::limit(1);
        REQUIRE(!!f0);
        REQUIRE((*f0).good());
        REQUIRE((*f0).tellg() == 10);
        auto t = std::is_same<std::ifstream &,
                              lz::detail::YieldType<decltype(*f0)>>::value;
        REQUIRE(t);

        auto f1 = openNotExisted | lz::limit(1);
        REQUIRE(!f1);

        for (auto i = 0; i < 100; ++i) {
          auto d = noCopy | lz::limit(1);
          if (d) {
            auto t = std::is_same<NoCopy &,
                                  lz::detail::YieldType<decltype(*d)>>::value;
            REQUIRE(t);
            REQUIRE(((*d).s == "no copy"));
          }
        }
      }
    }

    WHEN("have wrapped these functions by gen") {
      THEN("got same results") {
        auto f0 = lz::gen(openAndSeek10) | lz::limit(1);
        REQUIRE(!!f0);
        REQUIRE((*f0).good());
        REQUIRE((*f0).tellg() == 10);

        auto t = std::is_same<std::ifstream &,
                              lz::detail::YieldType<decltype(*f0)>>::value;
        REQUIRE(t);

        auto f1 = openNotExisted | lz::limit(1);
        REQUIRE(!f1);

        for (auto i = 0; i < 100; ++i) {
          auto d = lz::gen(noCopy) | lz::limit(1);
          if (d) {
            auto t = std::is_same<NoCopy &,
                                  lz::detail::YieldType<decltype(*d)>>::value;
            REQUIRE(t);
            REQUIRE(((*d).s == "no copy"));
          }
        }
      }
    }
  }

  GIVEN("(a) -> () -> OptionalOrRef<T>") {
    WHEN("have lambdas returned a non-copyable closure variable") {
      THEN("got an const/ordinary refrence by mutable-qualifier if possible") {
        for (auto i = 0; i < 100; ++i) {
          if (i % 2 == 0) {
            auto d = noCopyByConstRef() | lz::limit(1);
            if (d) {
              auto t = std::is_same<const NoCopy &,
                                    lz::detail::YieldType<decltype(*d)>>::value;
              REQUIRE(t);
              REQUIRE(((*d).s == "no copy"));
            }
          } else {
            auto d = noCopyByRef() | lz::limit(1);
            if (d) {
              auto t = std::is_same<NoCopy &,
                                    lz::detail::YieldType<decltype(*d)>>::value;
              REQUIRE(t);
              REQUIRE(((*d).s == "no copy"));
            }
          }
        }
      }
    }

    WHEN("have wrapped these lambdas by gen") {
      THEN("got same results") {
        for (auto i = 0; i < 100; ++i) {
          if (i % 2 == 0) {
            auto d = lz::gen(noCopyByConstRef()) | lz::limit(1);
            if (d) {
              auto t = std::is_same<const NoCopy &,
                                    lz::detail::YieldType<decltype(*d)>>::value;
              REQUIRE(t);
              REQUIRE(((*d).s == "no copy"));
            }
          } else {
            auto d = lz::gen(noCopyByRef()) | lz::limit(1);
            if (d) {
              auto t = std::is_same<NoCopy &,
                                    lz::detail::YieldType<decltype(*d)>>::value;
              REQUIRE(t);
              REQUIRE(((*d).s == "no copy"));
            }
          }
        }
      }
    }

    WHEN("read a file line by line") {
      auto f0 = readall(__FILE__);
      auto f1 = getOneLine(__FILE__) | lz::limit(10000);

      THEN("can get full content by enough limit") {
        REQUIRE(!f0.empty());
        REQUIRE(static_cast<bool>(f1));
        auto i = 0;
        for (auto &line : *f1) {
          REQUIRE(line == f0.substr(i, line.size()));
          i += line.size();
        }
      }
    }

    WHEN("read whole file at the first time") {
      auto f0 = readall(__FILE__);
      auto f1 = getAllLines(__FILE__) | lz::limit(1);

      THEN("got full content at once") {
        REQUIRE(!f0.empty());
        REQUIRE(!!f1);
        auto i = 0;
        for (auto &line : *f1) {
          REQUIRE(line == f0.substr(i, line.size()));
          i += line.size();
        }
      }
    }

    WHEN("have wrapped these file readers by gen") {
      auto f0 = readall(__FILE__);
      auto f1 = lz::gen(getOneLine(__FILE__)) | lz::limit(10000);
      auto f2 = lz::gen(getAllLines(__FILE__)) | lz::limit(1);

      THEN("got same results") {
        REQUIRE(!f0.empty());
        REQUIRE(!!f1);
        REQUIRE(!!f2);

        auto i = 0;
        for (auto &line : *f1) {
          REQUIRE(line == f0.substr(i, line.size()));
          i += line.size();
        }

        i = 0;
        for (auto &line : *f2) {
          REQUIRE(line == f0.substr(i, line.size()));
          i += line.size();
        }
      }
    }
  }
}
