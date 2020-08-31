#define CATCH_CONFIG_MAIN

#include <lz/lz.h>

#include <catch2/catch.hpp>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <limits>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

struct NoCopy {
  static std::size_t created;
  static std::size_t moved;
  static std::size_t moveAssigned;
  static std::size_t destroyed;

  std::string data;

  NoCopy() { NoCopy::created++; }
  ~NoCopy() { NoCopy::destroyed++; }
  NoCopy(NoCopy &&other) : data(std::move(other.data)) { NoCopy::moved++; }
  NoCopy &operator=(NoCopy &&other) {
    data = std::move(other.data);
    NoCopy::moveAssigned++;
    return *this;
  }

  NoCopy(const NoCopy &) = delete;
  NoCopy &operator=(const NoCopy &) = delete;
};

std::size_t NoCopy::created = 0;
std::size_t NoCopy::moved = 0;
std::size_t NoCopy::moveAssigned = 0;
std::size_t NoCopy::destroyed = 0;

static inline void cleanup() {
  NoCopy::created = 0;
  NoCopy::moved = 0;
  NoCopy::moveAssigned = 0;
  NoCopy::destroyed = 0;
}

static NoCopy nc;

static NoCopy rvalue() { return NoCopy{}; }
static NoCopy &lvalue() { return nc; }
static const NoCopy &clvalue() { return nc; }

static lz::Optional<NoCopy> roption() { return NoCopy{}; }
static lz::Optional<NoCopy &> loption() { return std::ref(nc); }
static lz::Optional<const NoCopy &> cloption() { return std::cref(nc); }

static auto rvalueLambda() {
  return [] { return NoCopy{}; };
}

static decltype(auto) lvalueLambda() {
  return [nc = NoCopy{}]() mutable -> NoCopy & { return nc; };
}

static auto clvalueLambda() {
  return [nc = NoCopy{}]() -> const NoCopy & { return nc; };
}

static auto roptionLambda() {
  return []() -> lz::Optional<NoCopy> { return NoCopy{}; };
}

static auto loptionLambda() {
  return [nc = NoCopy{}]() mutable -> lz::Optional<NoCopy &> {
    return std::ref(nc);
  };
}

static auto cloptionLambda() {
  return [nc = NoCopy{}]() -> lz::Optional<const NoCopy &> {
    return std::cref(nc);
  };
}

static auto rvfilter(NoCopy &&a) {
  NoCopy b;
  b.data += ":filtered";
  return b;
}

static NoCopy &lvfilter(NoCopy &a) {
  a.data += ":filtered";
  return a;
}

static auto clvfilter(const NoCopy &a) {
  NoCopy b;
  b.data += ":filtered";
  return b;
}

static auto rvofilter(lz::Optional<NoCopy> &&a) {
  (*a).data += ":filtered";
  return std::move(a);
}

template <typename T, typename V>
static constexpr void TypeAssert() {
  static_assert(std::is_same<T, V>::value, "is not the same");
}

template <typename T, typename V>
static constexpr void TypeAssert(std::false_type) {
  static_assert(!std::is_same<T, V>::value, "is the same");
}

template <typename T, typename Gen,
          lz::detail::EnableIfType<lz::detail::GeneratorBase, Gen> = 0>
static constexpr void TypeAssert(Gen &gen) {
  TypeAssert<T, typename lz::detail::RmRef<Gen>::YieldType>();
}

#define CHECK_ACCESS(lives, cost)                                    \
  do {                                                               \
    auto diff = NoCopy::created + NoCopy::moved - NoCopy::destroyed; \
    REQUIRE(diff == lives);                                          \
    REQUIRE(NoCopy::moved + NoCopy::moveAssigned <= cost);           \
  } while (0)

auto RandomChar = []() -> char {
  static const char charset[] =
      "0123456789"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "abcdefghijklmnopqrstuvwxyz";
  static const size_t maxIndex = (sizeof(charset) - 1);
  return charset[std::rand() % maxIndex];
};

auto RandomString(size_t length) {
  std::string str(length, 0);
  std::generate_n(str.begin(), length, RandomChar);
  return str;
}

auto RandomContent(size_t lines, size_t maxLineLength = 100) {
  std::stringstream ss;
  for (auto i = 0; i < lines; ++i) {
    auto length = std::rand() % maxLineLength;
    std::string str(length, 0);
    std::generate_n(str.begin(), length, RandomChar);
    ss << str;
    if (i < lines - 1) {
      ss << '\n';
    }
  }
  return ss.str();
}

std::vector<std::string> SplitString(const std::string &input,
                                     const std::string &regex) {
  // passing -1 as the submatch index parameter performs splitting
  std::regex re(regex);
  std::sregex_token_iterator first{input.begin(), input.end(), re, -1}, last;
  return {first, last};
}

template <typename Stream, typename Arg,
          lz::detail::EnableIfType<std::ostringstream, Stream> = 0>
Stream &Show(Stream &out, Arg &&arg) {
  out << std::forward<Arg>(arg) << ", ";
  return out;
}

template <typename Stream, typename T,
          lz::detail::EnableIfType<std::ostringstream, Stream> = 0>
Stream &Show(Stream &out, std::initializer_list<T> &&arg) {
  std::ostream_iterator<T> it(out, ", ");
  std::copy(std::begin(arg), std::end(arg), it);
  return out;
}

template <typename... Args>
std::string Show(Args &&... args) {
  std::ostringstream out;
  // Show(out, std::forward<Arg>(arg));
  using expander = int[];
  (void)expander{0, (void(Show(out, std::forward<Args>(args))), 0)...};
  auto s = out.str();
  if (!s.empty()) {
    s.resize(s.size() - 2);
  }
  return s;
}

SCENARIO("examine the YieldType on Generator", "[yieldType]") {
  GIVEN("function pointers") {
#define TA(T, F, V) \
  TypeAssert<       \
      T, typename lz::Generator<decltype(&std::declval<F>()), V>::YieldType>()

    WHEN("passing by value") {
      TA(int, int(int), int);
      TA(int, int(std::string), std::string);
      TA(int, int(std::string), std::string &);
      TA(int, int(std::string), const std::string &);
      TA(int, int(std::string), std::string &&);
      TA(int, int(lz::Optional<std::string>), lz::nullopt_t);
    }

    WHEN("passing by lvalue reference") {
      TA(int, int(NoCopy &), NoCopy);
      TA(int, int(NoCopy &), NoCopy &);
      TA(int, int(NoCopy &), NoCopy &&);
      TA(int, int(const NoCopy &), NoCopy);
      TA(int, int(const NoCopy &), NoCopy &);
      TA(int, int(const NoCopy &), const NoCopy);
      TA(int, int(const NoCopy &), const NoCopy &);
      TA(int, int(const NoCopy &), NoCopy &&);
      TA(int, int(lz::Optional<NoCopy &>), lz::nullopt_t);
    }

    WHEN("passing by rvalue reference") {
      TA(int, int(), void);
      TA(int, int(lz::Optional<NoCopy &&>), lz::nullopt_t);

      THEN("only nullopt is allowed to pass when using Optional") {}
    }

    WHEN("returning a value") {
      TA(NoCopy, NoCopy(), void);
      TA(const NoCopy, const NoCopy(), void);
      TA(NoCopy, lz::Optional<NoCopy>(), void);
      TA(NoCopy, lz::Optional<NoCopy &&>(), void);
      TA(NoCopy, lz::Optional<lz::Optional<NoCopy>>(), void);
      TA(NoCopy, lz::Optional<lz::Optional<NoCopy &&> &&>(), void);
      TA(lz::optional<NoCopy>, lz::Optional<lz::optional<NoCopy>>(), void);
      TA(lz::optionalref<NoCopy>, lz::Optional<lz::optionalref<NoCopy>>(),
         void);

      THEN("could return by either original type or Optional wrapper") {}
    }

    WHEN("returning a lvalue reference") {
      TA(NoCopy &, NoCopy & (), void);
      TA(const NoCopy &, const NoCopy &(), void);
      TA(NoCopy &, lz::Optional<NoCopy &>(), void);
      TA(const NoCopy &, lz::Optional<const NoCopy &>(), void);
      TA(NoCopy &, lz::Optional<lz::Optional<NoCopy &>>(), void);

      TA(std::false_type, lz::Optional<NoCopy> & (), void);
      TA(std::false_type, const lz::Optional<NoCopy> &(), void);
      TA(std::false_type, lz::Optional<lz::Optional<NoCopy &> &>(), void);
      TA(std::false_type, const lz::Optional<lz::Optional<NoCopy &> &> &(),
         void);

      THEN("cannot return lvalue reference of Optional itself") {}
    }

    WHEN("returning a rvalue reference") {
      TA(NoCopy &&, NoCopy && (), void);
      TA(const NoCopy &&, const NoCopy && (), void);

      TA(std::false_type, lz::Optional<NoCopy> && (), void);
      TA(std::false_type, const lz::Optional<NoCopy> && (), void);
      TA(std::false_type, const lz::Optional<lz::Optional<NoCopy &&> &&> && (),
         void);

      THEN("cannot return rvalue reference of Optional itself") {}
    }
  }

#undef TA

  GIVEN("lambdas") {
#define L(R, A) ([](A) { return R{}; })
#define TA(T, F, V)                                                      \
  do {                                                                   \
    auto _x = F;                                                         \
    TypeAssert<T, typename lz::Generator<decltype(_x), V>::YieldType>(); \
  } while (0)

    WHEN("passing by value") {
      TA(int, L(int, int), int);
      TA(int, L(int, std::string), std::string);
      TA(int, L(int, std::string), std::string &);
      TA(int, L(int, std::string), const std::string &);
      TA(int, L(int, std::string), std::string &&);
      TA(int, L(int, lz::Optional<std::string>), lz::nullopt_t);
      TA(int, L(int, auto), int);
    }

    WHEN("passing by lvalue reference") {
      TA(int, L(int, NoCopy &), NoCopy);
      TA(int, L(int, NoCopy &), NoCopy &);
      TA(int, L(int, NoCopy &), NoCopy &&);
      TA(int, L(int, const NoCopy &), NoCopy);
      TA(int, L(int, const NoCopy &), NoCopy &);
      TA(int, L(int, const NoCopy &), const NoCopy);
      TA(int, L(int, const NoCopy &), const NoCopy &);
      TA(int, L(int, const NoCopy &), NoCopy &&);
      TA(int, L(int, lz::Optional<NoCopy &>), lz::nullopt_t);
      TA(int, L(int, auto &), NoCopy);
    }

    WHEN("passing by rvalue reference") {
      TA(int, L(int, void), void);
      TA(int, L(int, lz::Optional<NoCopy &&>), lz::nullopt_t);
      TA(int, L(int, auto &&), NoCopy);

      THEN(
          "either universal reference parameter "
          "or nullopt argument is allowed");
    }
  }
}
#undef L
#undef TA

SCENARIO("try possible cases on overloaded genApply functions", "[genApply]") {
  GIVEN("lambdas") {
#define TA(a, b, c) \
  TypeAssert<a, decltype(lz::detail::genApply(b, lz::generator(c)))>()

    auto Rv = [] { return NoCopy{}; };
    auto LvRef = []() -> NoCopy & { return nc; };
    auto ConstLvRef = []() -> const NoCopy & { return nc; };

    auto RvRefToRv = [](NoCopy &&) { return NoCopy{}; };
    auto RvRefToLvRef = [](NoCopy &&) -> NoCopy & { return nc; };
    auto RvRefToRvRef = [](NoCopy &&) -> decltype(auto) {
      return std::move(nc);
    };

    auto LvRefToRv = [](NoCopy &) { return NoCopy{}; };
    auto LvRefToRvRef = [](NoCopy &nc) -> decltype(auto) {
      return std::move(nc);
    };
    auto LvRefToLvRef = [](NoCopy &nc) -> decltype(auto) { return nc; };

    auto ConstLvRefToConstLvRef = [](const NoCopy &nc) -> decltype(auto) {
      return nc;
    };

    auto ConstRvRefToConstRvRef = [](const NoCopy &nc) -> decltype(auto) {
      return std::move(nc);
    };

    auto RvOption = [] { return lz::Optional<NoCopy>{}; };
    auto LvRefOption = [] { return lz::Optional<NoCopy &>{}; };
    auto ConstLvRefOption = [] { return lz::Optional<const NoCopy &>{}; };

    auto RvOptionToRv = [](lz::Optional<NoCopy>) { return NoCopy{}; };
    auto LvRefOptionToRv = [](lz::Optional<NoCopy &>) { return NoCopy{}; };
    auto ConstLvRefOptionToRv = [](lz::Optional<const NoCopy &>) {
      return NoCopy{};
    };

    auto RvRefToRvOption = [](NoCopy &&) { return lz::Optional<NoCopy>{}; };
    auto RvRefToLvRefOption = [](NoCopy &&) {
      return lz::Optional<NoCopy &>{};
    };
    auto RvRefToConstLvRefOption = [](NoCopy &&) {
      return lz::Optional<const NoCopy &>{};
    };

    auto LvRefToRvOption = [](NoCopy &) { return lz::Optional<NoCopy>{}; };
    auto LvRefToLvRefOption = [](NoCopy &) { return lz::Optional<NoCopy &>{}; };
    auto LvRefToConstLvRefOption = [](NoCopy &) {
      return lz::Optional<const NoCopy &>{};
    };

    auto ConstLvRefToRvOption = [](const NoCopy &) {
      return lz::Optional<NoCopy>{};
    };
    auto ConstLvRefToLvRefOption = [](const NoCopy &) {
      return lz::Optional<NoCopy &>{};
    };
    auto ConstLvRefToConstLvRefOption = [](const NoCopy &) {
      return lz::Optional<const NoCopy &>{};
    };

    WHEN("without Optional") {
      TA(NoCopy, RvRefToRv, Rv);
      TA(NoCopy &, RvRefToLvRef, Rv);
      TA(NoCopy &&, RvRefToRvRef, Rv);

      TA(NoCopy, LvRefToRv, LvRef);
      TA(NoCopy &, LvRefToLvRef, LvRef);
      TA(NoCopy &&, LvRefToRvRef, LvRef);

      TA(const NoCopy &, ConstLvRefToConstLvRef, Rv);
      TA(const NoCopy &, ConstLvRefToConstLvRef, LvRef);
      TA(const NoCopy &, ConstLvRefToConstLvRef, ConstLvRef);

      TA(const NoCopy &&, ConstRvRefToConstRvRef, Rv);
      TA(const NoCopy &&, ConstRvRefToConstRvRef, LvRef);
      TA(const NoCopy &&, ConstRvRefToConstRvRef, ConstLvRef);

      THEN("works like combining two functions directly") {}
    }

    WHEN("with Optional") {
      TA(lz::Optional<NoCopy>, RvOptionToRv, RvOption);
      TA(lz::Optional<NoCopy>, ConstLvRefOptionToRv, LvRefOption);
      TA(lz::Optional<NoCopy>, LvRefToRv, LvRefOption);
      TA(lz::Optional<NoCopy>, LvRefToRv, RvOption);
      TA(lz::Optional<NoCopy>, LvRefToRvRef, RvOption);
      TA(lz::Optional<NoCopy &>, LvRefToLvRef, RvOption);
      TA(lz::Optional<const NoCopy &>, ConstLvRefToConstLvRef, RvOption);

      THEN("the yield type must be an optional one") {}
      AND_THEN("only the lvalue reference argument is allowed") {}
    }
  }
}
#undef TA

SCENARIO("define an entry component", "[entry]") {
  GIVEN("() -> T") {
    WHEN("| limit 0") {
      cleanup();
      auto f = rvalue | lz::limit(0);
      TypeAssert<NoCopy>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = rvalue | lz::limit(1);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = rvalue | lz::limit(100);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 200);
      }
    }
  }

  GIVEN("() -> T &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lvalue | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lvalue | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lvalue | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }
  }

  GIVEN("() -> const T &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = clvalue | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = clvalue | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = clvalue | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }
  }

  GIVEN("() -> Optional<T>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = roption | lz::limit(0);
      TypeAssert<NoCopy>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = roption | lz::limit(1);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = roption | lz::limit(100);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 200);
      }
    }
  }

  GIVEN("() -> Optional<T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = loption | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = loption | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = loption | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }
  }

  GIVEN("() -> Optional<const T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = cloption | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = cloption | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = cloption | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(0, 0);
      }
    }
  }

  GIVEN("() -> () -> T") {
    WHEN("| limit 0") {
      cleanup();
      auto f = rvalueLambda() | lz::limit(0);
      TypeAssert<NoCopy>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = rvalueLambda() | lz::limit(1);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = rvalueLambda() | lz::limit(100);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 200);
      }
    }
  }

  GIVEN("() -> () -> T (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(rvalueLambda()) | lz::limit(0);
      TypeAssert<NoCopy>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(rvalueLambda()) | lz::limit(1);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(rvalueLambda()) | lz::limit(100);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 200);
      }
    }
  }

  GIVEN("() -> () -> T &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lvalueLambda() | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        // there are additional lambda costs depend on number of components
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lvalueLambda() | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lvalueLambda() | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }
  }

  GIVEN("() -> () -> T & (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(lvalueLambda()) | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        // apart from the number of lambdas between pipe(|),
        // the gen wrapper causes a movement as well
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(lvalueLambda()) | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(lvalueLambda()) | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }
  }

  GIVEN("() -> () -> const T &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = clvalueLambda() | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = clvalueLambda() | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = clvalueLambda() | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }
  }

  GIVEN("() -> () -> const T & (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(clvalueLambda()) | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(clvalueLambda()) | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(clvalueLambda()) | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }
  }

  GIVEN("() -> () -> Optional<T>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = roptionLambda() | lz::limit(0);
      TypeAssert<NoCopy>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = roptionLambda() | lz::limit(1);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = roptionLambda() | lz::limit(100);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 200);
      }
    }
  }

  GIVEN("() -> () -> Optional<T> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(roptionLambda()) | lz::limit(0);
      TypeAssert<NoCopy>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(0, 0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(roptionLambda()) | lz::limit(1);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(roptionLambda()) | lz::limit(100);
      TypeAssert<NoCopy>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 200);
      }
    }
  }

  GIVEN("() -> () -> Optional<T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = loptionLambda() | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = loptionLambda() | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = loptionLambda() | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }
  }

  GIVEN("() -> () -> Optional<T &> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(loptionLambda()) | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(loptionLambda()) | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(loptionLambda()) | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }
  }

  GIVEN("() -> () -> Optional<const T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = cloptionLambda() | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = cloptionLambda() | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = cloptionLambda() | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 2);
      }
    }
  }

  GIVEN("() -> () -> Optional<const T &> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(cloptionLambda()) | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(cloptionLambda()) | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(cloptionLambda()) | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        CHECK_ACCESS(1, 3);
      }
    }
  }
}

SCENARIO("define an entry component to read stream into lines", "[readlines]") {
  auto content = RandomContent(std::rand() % 1000 + 10);
  auto lines = SplitString(content, "\n");

  GIVEN("a lambda returned current line of stream") {
    auto getline = [ss = std::istringstream(
                        content)]() mutable -> lz::Optional<std::string> {
      if (ss.good()) {
        std::string line;
        std::getline(ss, line);
        return line;
      }
      return lz::nullopt;
    };

    WHEN("| limit 1") {
      auto line = getline | lz::limit(1);
      THEN("got the first line") {
        REQUIRE(!!line);
        REQUIRE(*line == lines.front());
      }
    }

    WHEN("| limit 10") {
      auto line = getline | lz::limit(10);
      THEN("got the 10th line") {
        REQUIRE(!!line);
        REQUIRE(*line == lines[9]);
      }
    }

    WHEN("| limit MAX") {
      auto line = getline | lz::limit(std::numeric_limits<std::size_t>::max());
      THEN("got the last line") {
        REQUIRE(!!line);
        REQUIRE(*line == lines.back());
      }
    }
  }

  GIVEN("a lambda returned head N lines of stream") {
    auto headlines = [ss = std::istringstream(content),
                      vec = std::vector<std::string>()]() mutable
        -> lz::Optional<std::vector<std::string> &> {
      if (ss.good()) {
        std::string line;
        std::getline(ss, line);
        vec.emplace_back(std::move(line));
        return std::ref(vec);
      }
      return lz::nullopt;
    };

    WHEN("| limit 1") {
      auto head = headlines | lz::limit(1);
      THEN("got the first line") {
        REQUIRE(!!head);
        REQUIRE((*head).size() == 1);
        REQUIRE((*head).front() == lines.front());
      }
    }

    WHEN("| limit 10") {
      auto head = headlines | lz::limit(10);
      THEN("got head 10 lines") {
        REQUIRE(!!head);
        REQUIRE((*head).size() == 10);
        for (auto i = 0; i < 10; ++i) {
          REQUIRE((*head)[i] == lines[i]);
        }
      }
    }

    WHEN("| limit MAX") {
      auto head =
          headlines | lz::limit(std::numeric_limits<std::size_t>::max());
      THEN("got all lines") {
        REQUIRE(!!head);
        REQUIRE((*head).size() == lines.size());
        for (auto i = 0; i < lines.size(); ++i) {
          REQUIRE((*head)[i] == lines[i]);
        }
      }
    }
  }
}

SCENARIO("define a simple middle component", "[simpleMiddle]") {
  GIVEN("(a) -> b") {
    WHEN("previous output is rvalue") {
      cleanup();
      auto previous = lz::gen(rvalue);

      THEN("either this input is rvalue") {
        auto f = previous | rvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":filtered");
        CHECK_ACCESS(1, 3);
      }

      THEN("or this input is const ref") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":filtered");
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("previous output is lvalue") {
      cleanup();
      auto previous = lz::gen(lvalue);

      THEN("either this input is lvalue") {
        auto f = previous | lvfilter | lz::limit(1);
        TypeAssert<NoCopy &>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":filtered");
        CHECK_ACCESS(0, 0);
      }

      THEN("or this input is const ref") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":filtered");
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("previous output is const ref") {
      cleanup();
      auto previous = lz::gen(clvalue);

      THEN("this input has to be const ref as well") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":filtered");
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("previous output is rvalue of option") {
      cleanup();
      auto previous = lz::gen(roption);

      THEN("either this input is const ref") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":filtered");
        CHECK_ACCESS(1, 3);
      }

      THEN("or this input is rvalue of option (with one more movement)") {
        auto f = previous | rvofilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":filtered");
        CHECK_ACCESS(1, 4);
      }
    }
  }
}

// TODO: type conversion
// TODO: go off until got a value
// TODO: unpack to multiple parametered function

SCENARIO("define a complex middle component", "[complexMiddle]") {}
