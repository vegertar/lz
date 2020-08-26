#define CATCH_CONFIG_MAIN

#include <lz/lz.h>

#include <catch2/catch.hpp>
#include <cstdlib>
#include <limits>
#include <regex>
#include <sstream>
#include <string>

struct NoCopy {
  static std::size_t moved;
  static std::size_t moveAssigned;
  static std::size_t destroyed;

  ~NoCopy() { NoCopy::destroyed += 1; }

  NoCopy() = default;
  NoCopy(NoCopy &&) { NoCopy::moved += 1; }
  NoCopy &operator=(NoCopy &&) {
    NoCopy::moveAssigned += 1;
    return *this;
  }

  NoCopy(const NoCopy &) = delete;
  NoCopy &operator=(const NoCopy &) = delete;
};

std::size_t NoCopy::moved = 0;
std::size_t NoCopy::moveAssigned = 0;
std::size_t NoCopy::destroyed = 0;

static inline void cleanup() {
  NoCopy::moved = 0;
  NoCopy::moveAssigned = 0;
  NoCopy::destroyed = 0;
}

static NoCopy nc;

static NoCopy rvalue() { return NoCopy{}; }
static NoCopy &lvalue() { return nc; }
static const NoCopy &clvalue() { return nc; }

static lz::OptionOrRef<NoCopy> roption() { return NoCopy{}; }
static lz::OptionOrRef<NoCopy &> loption() { return std::ref(nc); }
static lz::OptionOrRef<const NoCopy &> cloption() { return std::cref(nc); }

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
  return []() -> lz::OptionOrRef<NoCopy> { return NoCopy{}; };
}

static auto loptionLambda() {
  return [nc = NoCopy{}]() mutable -> lz::OptionOrRef<NoCopy &> {
    return std::ref(nc);
  };
}

static auto cloptionLambda() {
  return [nc = NoCopy{}]() -> lz::OptionOrRef<const NoCopy &> {
    return std::cref(nc);
  };
}

template <typename T>
using Before = typename T::YieldType;

template <typename T>
using After = typename lz::detail::YieldType<T>;

template <typename T, typename V>
static constexpr void TypeAssert() {
  static_assert(std::is_same<T, V>::value, "err");
}

template <typename T1, typename T2 = T1, typename Gen,
          lz::detail::EnableIfType<lz::detail::GeneratorBase, Gen> = 0>
static constexpr void TypeAssert(Gen &gen) {
  TypeAssert<T1, Before<Gen>>();
  TypeAssert<T2, After<decltype(*gen)>>();
}

#define VARGS_(_10, _9, _8, _7, _6, _5, _4, _3, _2, _1, N, ...) N
#define VARGS(...) VARGS_(__VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)

#define CONCAT_(a, b) a##b
#define CONCAT(a, b) CONCAT_(a, b)

#define CHECK_COST_2(limit, lambdaCosts)                                   \
  do {                                                                     \
    CHECK(NoCopy::moved == (limit ? limit + 2 : 0) + lambdaCosts);         \
    CHECK(NoCopy::moveAssigned == (limit ? limit - 1 : 0));                \
    CHECK(NoCopy::destroyed == (limit ? limit * 2 + 1 : 0) + lambdaCosts); \
  } while (0)

#define CHECK_COST_1(a) CHECK_COST_2(a, 0)
#define CHECK_COST(...) CONCAT(CHECK_COST_, VARGS(__VA_ARGS__))(__VA_ARGS__)

#define ACESS_GEN(f)        \
  do {                      \
    decltype(auto) v1 = *f; \
    decltype(auto) v2 = *f; \
    (void)v1;               \
    (void)v2;               \
    (void)*f;               \
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

SCENARIO("define an entry component", "[entry]") {
  GIVEN("() -> a") {
    WHEN("| limit 0") {
      cleanup();
      auto f = rvalue | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = rvalue | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = rvalue | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> a (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(rvalue) | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(rvalue) | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(rvalue) | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> a &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lvalue | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lvalue | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lvalue | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> a & (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(lvalue) | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(lvalue) | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(lvalue) | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> a const &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = clvalue | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = clvalue | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = clvalue | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> a const & (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(clvalue) | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(clvalue) | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(clvalue) | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> OptionalOrRef<T>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = roption | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = roption | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = roption | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> OptionalOrRef<T> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(roption) | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(roption) | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(roption) | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> OptionalOrRef<T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = loption | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = loption | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = loption | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> OptionalOrRef<T &> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(loption) | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(loption) | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(loption) | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> OptionalOrRef<const T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = cloption | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = cloption | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = cloption | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> OptionalOrRef<const T &> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(cloption) | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(cloption) | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(cloption) | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0);
      }
    }
  }

  GIVEN("() -> () -> a") {
    WHEN("| limit 0") {
      cleanup();
      auto f = rvalueLambda() | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = rvalueLambda() | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = rvalueLambda() | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> () -> a (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(rvalueLambda()) | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(rvalueLambda()) | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(rvalueLambda()) | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> () -> a &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lvalueLambda() | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        // there are additional lambda costs depend on number of components
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lvalueLambda() | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lvalueLambda() | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }
  }

  GIVEN("() -> () -> a & (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(lvalueLambda()) | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        // apart from the number of lambdas between pipe(|),
        // the gen wrapper causes a movement as well
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(lvalueLambda()) | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(lvalueLambda()) | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }
  }

  GIVEN("() -> () -> a const &") {
    WHEN("| limit 0") {
      cleanup();
      auto f = clvalueLambda() | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = clvalueLambda() | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = clvalueLambda() | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }
  }

  GIVEN("() -> () -> a const & (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(clvalueLambda()) | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(clvalueLambda()) | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(clvalueLambda()) | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }
  }

  GIVEN("() -> () -> OptionalOrRef<T>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = roptionLambda() | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = roptionLambda() | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = roptionLambda() | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> () -> OptionalOrRef<T> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(roptionLambda()) | lz::limit(0);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(roptionLambda()) | lz::limit(1);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(1);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(roptionLambda()) | lz::limit(100);
      TypeAssert<NoCopy, NoCopy &&>(f);

      THEN("got rvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(100);
      }
    }
  }

  GIVEN("() -> () -> OptionalOrRef<T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = loptionLambda() | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = loptionLambda() | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = loptionLambda() | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }
  }

  GIVEN("() -> () -> OptionalOrRef<T &> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(loptionLambda()) | lz::limit(0);
      TypeAssert<NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(loptionLambda()) | lz::limit(1);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(loptionLambda()) | lz::limit(100);
      TypeAssert<NoCopy &>(f);

      THEN("got lvalue") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }
  }

  GIVEN("() -> () -> OptionalOrRef<const T &>") {
    WHEN("| limit 0") {
      cleanup();
      auto f = cloptionLambda() | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = cloptionLambda() | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = cloptionLambda() | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 2);
      }
    }
  }

  GIVEN("() -> () -> OptionalOrRef<const T &> (wrapped by gen)") {
    WHEN("| limit 0") {
      cleanup();
      auto f = lz::gen(cloptionLambda()) | lz::limit(0);
      TypeAssert<const NoCopy &>(f);

      THEN("got nop") {
        REQUIRE(!f);
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 1") {
      cleanup();
      auto f = lz::gen(cloptionLambda()) | lz::limit(1);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }

    WHEN("| limit 100") {
      cleanup();
      auto f = lz::gen(cloptionLambda()) | lz::limit(100);
      TypeAssert<const NoCopy &>(f);

      THEN("got cref") {
        REQUIRE(!!f);
        ACESS_GEN(f);
        CHECK_COST(0, 3);
      }
    }
  }
}

SCENARIO("define an entry component to read stream into lines", "[readlines]") {
  auto content = RandomContent(std::rand() % 1000 + 10);
  auto lines = SplitString(content, "\n");

  GIVEN("a lambda returned current line of stream") {
    auto getline = [ss = std::istringstream(
                        content)]() mutable -> lz::OptionOrRef<std::string> {
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
        -> lz::OptionOrRef<std::vector<std::string> &> {
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