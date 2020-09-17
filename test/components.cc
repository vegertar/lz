#define CATCH_CONFIG_MAIN

#include <lz/lz.h>

#include <catch2/catch.hpp>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <limits>
#include <list>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

using lz::operator|;

struct NoCopy {
  static std::size_t created;
  static std::size_t moved;
  static std::size_t moveAssigned;
  static std::size_t destroyed;

  std::string data;

  NoCopy(const std::string &s = "") : data(s) { NoCopy::created++; }
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

static NoCopy nc;

static inline void cleanup() {
  nc.data.clear();
  NoCopy::created = 0;
  NoCopy::moved = 0;
  NoCopy::moveAssigned = 0;
  NoCopy::destroyed = 0;
}

static NoCopy rvalue() { return NoCopy{}; }
static NoCopy &lvalue() { return nc; }
static const NoCopy &clvalue() { return nc; }

static lz::Optional<NoCopy> roption() { return NoCopy{}; }
static lz::Optional<NoCopy &> loption() { return std::ref(nc); }
static lz::Optional<const NoCopy &> cloption() { return std::cref(nc); }

static auto rvalueLambda() {
  return [] { return NoCopy{}; };
}

static auto rdataLambda(std::string &&s) {
  return [s = std::move(s)] { return NoCopy{s}; };
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
  b.data += ":rvfilter";
  return b;
}

static NoCopy &lvfilter(NoCopy &a) {
  a.data += ":lvfilter";
  return a;
}

static auto clvfilter(const NoCopy &a) {
  NoCopy b;
  b.data += ":clvfilter";
  return b;
}

static auto rvofilter(lz::Optional<NoCopy> &&a) {
  (*a).data += ":rvofilter";
  return std::move(a);
}

static decltype(auto) nullValue() { return lz::nullopt; }
static decltype(auto) nullFilter(NoCopy &&) { return lz::nullopt; }

template <typename T>
static lz::Optional<T> nullOption() {
  return {};
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
  TypeAssert<T, typename std::decay_t<Gen>::SubmitType>();
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

template <typename T>
struct has_const_iterator {
 private:
  typedef char yes;
  typedef struct {
    char array[2];
  } no;

  template <typename C>
  static yes test(typename C::const_iterator *);
  template <typename C>
  static no test(...);

 public:
  static const bool value = sizeof(test<T>(0)) == sizeof(yes);
  typedef T type;
};

template <typename T>
struct has_begin_end {
  template <typename C>
  static char (
      &f(typename std::enable_if<
          std::is_same<decltype(static_cast<typename C::const_iterator (C::*)()
                                                const>(&C::begin)),
                       typename C::const_iterator (C::*)() const>::value,
          void>::type *))[1];

  template <typename C>
  static char (&f(...))[2];

  template <typename C>
  static char (
      &g(typename std::enable_if<
          std::is_same<decltype(static_cast<typename C::const_iterator (C::*)()
                                                const>(&C::end)),
                       typename C::const_iterator (C::*)() const>::value,
          void>::type *))[1];

  template <typename C>
  static char (&g(...))[2];

  static bool const beg_value = sizeof(f<T>(0)) == 1;
  static bool const end_value = sizeof(g<T>(0)) == 1;
};

template <typename T>
struct is_container
    : std::integral_constant<bool, has_const_iterator<T>::value &&
                                       has_begin_end<T>::beg_value &&
                                       has_begin_end<T>::end_value> {};

template <typename Stream, typename Arg,
          lz::detail::EnableIfType<std::ostringstream, Stream> = 0,
          std::enable_if_t<!is_container<Arg>::value, int> = 0>
Stream &ShowCont(Stream &out, const Arg &arg, const std::string &cont) {
  out << arg << cont;
  return out;
}

template <typename Stream, typename Arg,
          lz::detail::EnableIfType<std::ostringstream, Stream> = 0,
          std::enable_if_t<is_container<Arg>::value, int> = 0>
Stream &ShowCont(Stream &out, const Arg &arg, const std::string &cont) {
  for (auto &item : arg) {
    out << item << cont;
  }
  return out;
}

template <typename Stream, typename T,
          lz::detail::EnableIfType<std::ostringstream, Stream> = 0>
Stream &ShowCont(Stream &out, const std::initializer_list<T> &arg,
                 const std::string &cont) {
  for (auto &item : arg) {
    out << item << cont;
  }
  return out;
}

template <typename... Args>
std::string ShowCont(const std::string &cont, Args &&... args) {
  std::ostringstream out;
  using expander = int[];
  (void)expander{0,
                 (void(ShowCont(out, std::forward<Args>(args), cont)), 0)...};
  auto s = out.str();
  if (!s.empty()) {
    s.resize(s.size() - cont.size());
  }
  return s;
}

SCENARIO("examine the InvokeType on Generator", "[invokeType]") {
  GIVEN("entry generators") {
#define TA(T, F)                                \
  do {                                          \
    auto g = lz::detail::generator(lz::gen(F)); \
    TypeAssert<T, decltype(g)::InvokeType>();   \
  } while (0)

    TA(NoCopy, rvalue);
    TA(NoCopy &, lvalue);
    TA(const NoCopy &, clvalue);

    TA(lz::Optional<NoCopy>, roption);
    TA(lz::Optional<NoCopy &>, loption);
    TA(lz::Optional<const NoCopy &>, cloption);
  }

  GIVEN("2 components") {
#undef TA
#define TA(T, F1, F2)                         \
  do {                                        \
    auto g = lz::gen(F1) | F2;                \
    TypeAssert<T, decltype(g)::InvokeType>(); \
  } while (0)

    TA(NoCopy, rvalue, rvfilter);
    TA(NoCopy &, lvalue, lvfilter);
    TA(NoCopy, clvalue, clvfilter);

    TA(lz::Optional<NoCopy>, roption, clvfilter);
    TA(lz::Optional<NoCopy>, roption, rvofilter);
    TA(lz::Optional<NoCopy>, rvalue, rvofilter);
  }

  GIVEN("3 components") {
    WHEN("a | b | c") {
#undef TA
#define TA(T, F1, F2, F3)                     \
  do {                                        \
    auto g = lz::gen(F1) | F2 | F3;           \
    TypeAssert<T, decltype(g)::InvokeType>(); \
  } while (0)

      TA(NoCopy, rvalue, rvfilter, clvfilter);
      TA(lz::Optional<NoCopy>, rvalue, rvofilter, clvfilter);
      TA(lz::Optional<NoCopy>, roption, clvfilter, clvfilter);
    }

    WHEN("a | (b | c)") {
#undef TA
#define TA(T, F1, F2, F3)                     \
  do {                                        \
    auto g = lz::gen(F1);                     \
    auto m = lz::gen(F2) | F3;                \
    auto h = g | m;                           \
    TypeAssert<T, decltype(h)::InvokeType>(); \
  } while (0)

      TA(NoCopy, rvalue, rvfilter, clvfilter);
      TA(lz::Optional<NoCopy>, rvalue, rvofilter, clvfilter);
      TA(lz::Optional<NoCopy>, roption, clvfilter, clvfilter);
    }
  }

  GIVEN("5 components") {
    auto a = []() { return 0; };
    auto b = [](int x) { return x + 1; };
    auto c = [](int x) { return x * 3; };
    auto d = [](int x) { return x / 2; };
    auto e = [](int x) { return x - 1; };

    WHEN("a | b | c | d | e") {
      int x = lz::gen(a, b, c, d, e);
      REQUIRE(x == 0);
    }

    WHEN("a | (b | c) | (d | e)") {
      int x = lz::gen(a, lz::gen(b, c), lz::gen(d, e));
      REQUIRE(x == 0);
    }

    WHEN("a | (b | c | d) | e") {
      int x = lz::gen(a, lz::gen(b, c, d), e);
      REQUIRE(x == 0);
    }
  }
}

#undef TA

SCENARIO("examine the SubmitType on Generator", "[submitType]") {
  GIVEN("function pointers") {
#define TA(T, F, V)                                                          \
  TypeAssert<T, typename lz::detail::Generator<decltype(&std::declval<F>()), \
                                               V>::SubmitType>()

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
      TA(int, int(), lz::detail::Void);
      TA(int, int(lz::Optional<NoCopy &&>), lz::nullopt_t);

      THEN("only nullopt is allowed to pass when using Optional") {}
    }

    WHEN("returning a value") {
      TA(NoCopy, NoCopy(), lz::detail::Void);
      TA(const NoCopy, const NoCopy(), lz::detail::Void);
      TA(NoCopy, lz::Optional<NoCopy>(), lz::detail::Void);
      TA(NoCopy, lz::Optional<NoCopy &&>(), lz::detail::Void);
      TA(NoCopy, lz::Optional<lz::Optional<NoCopy>>(), lz::detail::Void);
      TA(NoCopy, lz::Optional<lz::Optional<NoCopy &&> &&>(), lz::detail::Void);
      TA(lz::optional<NoCopy>, lz::Optional<lz::optional<NoCopy>>(),
         lz::detail::Void);
      TA(lz::optionalref<NoCopy>, lz::Optional<lz::optionalref<NoCopy>>(),
         lz::detail::Void);

      THEN("could return by either original type or Optional wrapper") {}
    }

    WHEN("returning a lvalue reference") {
      TA(NoCopy &, NoCopy & (), lz::detail::Void);
      TA(const NoCopy &, const NoCopy &(), lz::detail::Void);
      TA(NoCopy &, lz::Optional<NoCopy &>(), lz::detail::Void);
      TA(const NoCopy &, lz::Optional<const NoCopy &>(), lz::detail::Void);
      TA(NoCopy &, lz::Optional<lz::Optional<NoCopy &>>(), lz::detail::Void);

      TA(lz::detail::NeverYieldNonValuedOptionalType, lz::Optional<NoCopy> & (),
         lz::detail::Void);
      TA(lz::detail::NeverYieldNonValuedOptionalType,
         const lz::Optional<NoCopy> &(), lz::detail::Void);
      TA(lz::detail::NeverYieldNonValuedOptionalType,
         lz::Optional<lz::Optional<NoCopy &> &>(), lz::detail::Void);
      TA(lz::detail::NeverYieldNonValuedOptionalType,
         const lz::Optional<lz::Optional<NoCopy &> &> &(), lz::detail::Void);

      THEN("cannot return lvalue reference of Optional itself") {}
    }

    WHEN("returning a rvalue reference") {
      TA(NoCopy &&, NoCopy && (), lz::detail::Void);
      TA(const NoCopy &&, const NoCopy && (), lz::detail::Void);

      TA(lz::detail::NeverYieldNonValuedOptionalType,
         lz::Optional<NoCopy> && (), lz::detail::Void);
      TA(lz::detail::NeverYieldNonValuedOptionalType,
         const lz::Optional<NoCopy> && (), lz::detail::Void);
      TA(lz::detail::NeverYieldNonValuedOptionalType,
         const lz::Optional<lz::Optional<NoCopy &&> &&> && (),
         lz::detail::Void);

      THEN("cannot return rvalue reference of Optional itself") {}
    }
  }

#undef TA

  GIVEN("lambdas") {
#define L(R, A) ([](A) { return R{}; })
#define TA(T, F, V)                                                            \
  do {                                                                         \
    auto _x = F;                                                               \
    TypeAssert<T,                                                              \
               typename lz::detail::Generator<decltype(_x), V>::SubmitType>(); \
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
      TA(int, L(int, void), lz::detail::Void);
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

SCENARIO("try possible entry started cases on overloaded genApply functions",
         "[genApply]") {
  GIVEN("lambdas") {
#define TA(a, b, c) \
  TypeAssert<a, decltype(lz::detail::genApply(b, lz::detail::generator(c)))>()

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

SCENARIO("try possible ways to create middleware") {
  GIVEN("use genApply") {
#define TA(a)                                                              \
  static_assert(std::is_base_of<lz::detail::MiddlewareBase,                \
                                decltype(lz::detail::genApply(a))>::value, \
                "not a Middleware")

#define NTA(a)                                                              \
  static_assert(!std::is_base_of<lz::detail::MiddlewareBase,                \
                                 decltype(lz::detail::genApply(a))>::value, \
                "is a Middleware")

    TA(rvfilter);
    TA(lvfilter);
    TA(RandomString);

    NTA(rvalue);
    NTA(roption);
    NTA(clvalueLambda);
  }

#undef TA
#undef NTA
  GIVEN("use generator") {
#define TA(a)                                                               \
  static_assert(std::is_base_of<lz::detail::MiddlewareBase,                 \
                                decltype(lz::detail::generator(a))>::value, \
                "not a Middleware")

#define NTA(a)                                                               \
  static_assert(!std::is_base_of<lz::detail::MiddlewareBase,                 \
                                 decltype(lz::detail::generator(a))>::value, \
                "is a Middleware")

    TA(rvfilter);
    TA(lvfilter);
    TA(RandomString);

    NTA(rvalue);
    NTA(roption);
    NTA(clvalueLambda);
  }
}
#undef TA
#undef NTA

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

SCENARIO("define a simple middle component", "[middle]") {
  GIVEN("(a) -> b") {
    WHEN("previous output is rvalue") {
      cleanup();
      auto previous = lz::gen(rvalue);

      THEN("either this input is rvalue") {
        auto f = previous | rvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":rvfilter");
        CHECK_ACCESS(1, 3);
      }

      THEN("or this input is const ref") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":clvfilter");
        CHECK_ACCESS(1, 3);
      }
    }

    WHEN("previous output is lvalue") {
      cleanup();
      auto previous = lz::gen(lvalue);

      THEN("either this input is ref") {
        auto f = previous | lvfilter | lz::limit(1);
        TypeAssert<NoCopy &>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":lvfilter");
        CHECK_ACCESS(0, 0);
      }

      THEN("or this input is const ref") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":clvfilter");
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("previous output is const lvalue") {
      cleanup();
      auto previous = lz::gen(clvalue);

      THEN("this input has to be const ref as well") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":clvfilter");
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("previous output is optional of rvalue") {
      cleanup();
      auto previous = lz::gen(roption);

      THEN("either this input is const ref") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":clvfilter");
        CHECK_ACCESS(1, 3);
      }

      THEN("or this input is rvalue of option (with one more movement)") {
        auto f = previous | rvofilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":rvofilter");
        CHECK_ACCESS(1, 4);
      }
    }

    WHEN("previous output is optional of lvalue") {
      cleanup();
      auto previous = lz::gen(loption);

      THEN("either this input is ref") {
        auto f = previous | lvfilter | lz::limit(1);
        TypeAssert<NoCopy &>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":lvfilter");
        CHECK_ACCESS(0, 0);
      }

      THEN("or this input is const ref") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":clvfilter");
        CHECK_ACCESS(1, 2);
      }
    }

    WHEN("previous output is optional of const lvalue") {
      cleanup();
      auto previous = lz::gen(cloption);

      THEN("this input has to be const ref as well") {
        auto f = previous | clvfilter | lz::limit(1);
        TypeAssert<NoCopy>(f);
        REQUIRE(!!f);
        REQUIRE((*f).data == ":clvfilter");
        CHECK_ACCESS(1, 2);
      }
    }
  }
}

template <typename T>
auto Until(T &&status) {
  return
      [status, reached = false](auto &&gen) mutable -> decltype(lz::get(gen)) {
        auto val = lz::get(gen);
        if (reached || !val) {
          return {};
        }

        reached = *val == status;
        return val;
      };
}

template <typename T>
auto Iterate(const std::initializer_list<T> &list) {
  return [begin = list.begin(), end = list.end()]() mutable -> lz::Optional<T> {
    if (begin != end) {
      return *begin++;
    }
    return lz::nullopt;
  };
}

template <typename T>
auto Iterate(T &list) {
  return
      [begin = list.begin(),
       end = list.end()]() mutable -> lz::Optional<typename T::value_type &> {
        if (begin != end) {
          return *begin++;
        }
        return lz::nullopt;
      };
}

template <typename T>
auto AllTheSame() {
  return lz::gen([p = T{}, init = T{}](const T &data) mutable {
    if (p == init) {
      p = data;
      return true;
    }

    return p == data;
  });
};

auto SplitWords(const std::string &s) { return SplitString(s, "(\\W+)"); }

template <typename T>
auto Count(T &&t) {
  return [t = std::forward<T>(t)](const auto &data) {
    return std::count(std::begin(data), std::end(data), t);
  };
}

template <typename M, typename T>
auto MaximumOn(M T::*fn) {
  auto mem = std::mem_fn(fn);
  using R = typename decltype(mem)::result_type;
  return [mem, max = std::numeric_limits<R>::min(),
          p = static_cast<T *>(nullptr)](T &t) mutable -> T & {
    auto x = mem(t);

    if (max < x) {
      max = x;
      p = &t;
    }

    return *p;
  };
}

auto Ints(int start = 0) {
  return [i = start]() mutable { return i++; };
}

auto Show(const std::string &cont) {
  return [cont](auto &&item) mutable {
    return ShowCont(cont, std::forward<decltype(item)>(item));
  };
}

auto CollatzSeq(int x) {
  std::list<int> result;
  while (x > 1) {
    result.push_back(x);
    if (x % 2 == 0) {
      x = x / 2;
    } else {
      x = 3 * x + 1;
    }
  }
  result.push_back(x);
  return result;
}

template <typename T>
auto Vector() {
  return [c = std::vector<T>{}](T &&item) mutable {
    c.emplace_back(std::forward<T>(item));
    return std::ref(c);
  };
}

SCENARIO("define complex middleware components", "[middleware]") {
  GIVEN("The same old song") {
    auto middleware = AllTheSame<std::string>() | Until(false);

    WHEN("all the same") {
      auto f = Iterate({"same old", "same old"}) | middleware;
      REQUIRE(!!f);

      THEN("got true") { REQUIRE(*f); }
    }

    WHEN("not all the same") {
      auto f =
          Iterate({"same old", "same old", "xxx", "same old"}) | middleware;
      REQUIRE(!!f);

      THEN("got false") { REQUIRE(!*f); }
    }
  }

  GIVEN("The I in our team") {
    auto middleware = lz::gen(SplitWords) | lz::gen(Count("I"));

    std::string s0 = "Our team is great. I love everybody I work with.";
    WHEN(s0) {
      auto f = Iterate({s0}) | middleware;
      REQUIRE(!!f);

      THEN("got 2") { REQUIRE(*f == 2); }
    }

    std::string s1 = "Our team is great.";
    WHEN(s1) {
      auto f = Iterate({s1}) | middleware;
      REQUIRE(!!f);

      THEN("got 0") { REQUIRE(*f == 0); }
    }
  }

  GIVEN("The cutest kitty") {
    struct cat {
      double cuteness() const {
        return softness_ * temperature_ * roundness_ * fur_amount_ - size_;
      }
      std::string name_;
      double softness_;
      double temperature_;
      double size_;
      double roundness_;
      double fur_amount_;
    };

    auto middleware = lz::gen(MaximumOn(&cat::cuteness));

    WHEN("have four cats") {
      std::vector<cat> cats = {{"Tigger", 5, 5, 5, 5, 5},
                               {"Simba", 2, 9, 9, 2, 7},
                               {"Muffin", 9, 4, 2, 8, 6},
                               {"Garfield", 6, 5, 7, 9, 5}};
      auto f = Iterate(cats) | middleware;
      REQUIRE(!!f);

      THEN("got Muffin") { REQUIRE((*f).name_ == "Muffin"); }
    }
  }

  GIVEN("Function composition, binding and map creation") {
    auto middleware =
        CollatzSeq |
        lz::gen(Show(" => "));  // | lz::gen(Vector<std::string>());

    WHEN("have 30 ints") {
      auto f = Ints(1) | middleware | lz::limit(30);
      std::vector<std::string> vec;
      std::copy(std::begin(f), std::end(f), std::back_inserter(vec));
      REQUIRE(vec.size() == 30);
      REQUIRE(vec[12] == "13 => 40 => 20 => 10 => 5 => 16 => 8 => 4 => 2 => 1");

      // TODO: create map via pipeline
    }
  }
}

SCENARIO("define once piplines", "[once]") {
  cleanup();
  WHEN("has no limit tailing") {
    NoCopy x = lz::gen(rvalue) | lz::gen(clvfilter);
    REQUIRE(x.data == ":clvfilter");
    CHECK_ACCESS(1, 0);
  }
}

SCENARIO("forward iterator", "[iterator]") {
  WHEN("limit 0") {
    auto f = Ints(0) | lz::limit(0);
    auto begin = std::begin(f);
    auto end = std::end(f);
    THEN("begin equals end") { REQUIRE((begin == end)); }
  }

  WHEN("limit 10") {
    auto f = Ints(0) | lz::limit(10);
    auto begin = std::begin(f);
    auto end = std::end(f);

    THEN("begin not equals end") { REQUIRE((begin != end)); }

    AND_THEN("*begins multitimes equals 0") {
      REQUIRE(*begin == 0);
      REQUIRE(*begin == 0);
      REQUIRE(*begin == 0);
    }

    AND_THEN("begin + 10 equals end") {
      auto start = begin;
      for (auto i = 0; i < 10; ++i) {
        ++begin;
        REQUIRE((start != begin));
      }
      REQUIRE((begin == end));
    }
  }
}

SCENARIO("logical generators", "[logical]") {
  using lz::operator&&;
  using lz::operator||;

#define SA(f)                                                         \
  static_assert(                                                      \
      std::is_base_of<lz::detail::ComponentBase, decltype(f)>::value, \
      "should be a Component")

  GIVEN("a && b") {
    WHEN("a got null") {
      auto f = lz::gen(nullValue) && rvalue;
      SA(f);

      THEN("got null") { REQUIRE(!f); }
    }

    WHEN("b got null") {
      auto f = lz::gen(rvalue) && nullValue;
      SA(f);

      THEN("got null") { REQUIRE(!f); }
    }

    WHEN("both a and b got well") {
      auto f = (rdataLambda("a") && rdataLambda("b")) | lz::limit(1);
      SA(f);

      THEN("got b") {
        REQUIRE(!!f);
        REQUIRE(f->data == "b");
      }
    }
  }

  GIVEN("x | (a && b)") {
    WHEN("x got null") {
      auto f = nullValue | (lz::gen(rvfilter) && clvfilter);
      THEN("got null") { REQUIRE(!f); }
    }

    WHEN("a got null") {
      auto f = rvalue | (lz::gen(nullFilter) && clvfilter);
      THEN("got null") { REQUIRE(!f); }
    }

    WHEN("b got null") {
      auto f = rvalue | (clvfilter && lz::gen(nullFilter));
      THEN("got null") { REQUIRE(!f); }
    }

    WHEN("both a and b got well") {
      auto f = rvalue | (lz::gen(rvfilter) && clvfilter) | lz::limit(1);

      THEN("got b") {
        REQUIRE(!!f);
        REQUIRE(f->data == ":clvfilter");
      }
    }
  }

  GIVEN("a || b") {
    WHEN("a got null") {
      auto f = (lz::gen(nullOption<NoCopy>) || rdataLambda("b")) | lz::limit(1);
      SA(f);

      THEN("got b") {
        REQUIRE(!!f);
        REQUIRE(f->data == "b");
      }
    }

    WHEN("b got null") {
      auto f = (rdataLambda("a") || lz::gen(nullOption<NoCopy>)) | lz::limit(1);
      SA(f);

      THEN("got a") {
        REQUIRE(!!f);
        REQUIRE(f->data == "a");
      }
    }

    WHEN("both a and b got well") {
      auto f = (rdataLambda("a") || rdataLambda("b")) | lz::limit(1);
      SA(f);

      THEN("got a") {
        REQUIRE(!!f);
        REQUIRE(f->data == "a");
      }
    }
  }

  GIVEN("x | (a || b)") {
    WHEN("x got null") {
      auto f = nullValue | (lz::gen(rvfilter) || clvfilter);
      THEN("got null") { REQUIRE(!f); }
    }

    WHEN("a got null") {
      auto f = rvalue | (lz::gen(nullFilter) || clvfilter) | lz::limit(1);
      THEN("got b") {
        REQUIRE(!!f);
        REQUIRE(f->data == ":clvfilter");
      }
    }

    WHEN("b got null") {
      auto f = rvalue | (clvfilter || lz::gen(nullFilter)) | lz::limit(1);
      THEN("got a") {
        REQUIRE(!!f);
        REQUIRE(f->data == ":clvfilter");
      }
    }

    WHEN("both a and b got well") {
      auto f = rvalue | (lz::gen(rvfilter) || clvfilter) | lz::limit(1);

      THEN("got a") {
        REQUIRE(!!f);
        REQUIRE(f->data == ":rvfilter");
      }
    }
  }
}
#undef SA

SCENARIO("scale generators", "[scale]") {
  auto x = lz::gen(lvalue);

  GIVEN("x | (a * 1)") {
    cleanup();
    auto f = x | (lz::gen(lvfilter) * 1) | lz::limit(1);

    THEN("applyed once one loop") {
      REQUIRE(!!f);
      REQUIRE(f->data == ":lvfilter");
    }
  }

  GIVEN("x | (a * 0)") {
    cleanup();
    auto f = x | (lz::gen(lvfilter) * 0) | lz::limit(1);

    THEN("ignored") {
      REQUIRE(!!f);
      REQUIRE(f->data == "");
    }
  }

  GIVEN("x | (a * (-1))") {
    cleanup();
    auto f = x | (lz::gen(lvfilter) * (-1)) | lz::limit(10);

    THEN("applyed only once") {
      REQUIRE(!!f);
      REQUIRE(f->data == ":lvfilter");
    }
  }

  GIVEN("x | (a * 2)") {
    cleanup();
    auto f = x | (lz::gen(lvfilter) * 2) | lz::limit(1);

    THEN("applyed twice one loop") {
      REQUIRE(!!f);
      REQUIRE(f->data == ":lvfilter:lvfilter");
    }
  }

  GIVEN("x | (a * (-2))") {
    cleanup();
    auto f = x | (lz::gen(lvfilter) * (-2)) | lz::limit(10);

    THEN("applyed only twice") {
      REQUIRE(!!f);
      REQUIRE(f->data == ":lvfilter:lvfilter");
    }
  }
}