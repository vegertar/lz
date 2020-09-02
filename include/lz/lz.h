#pragma once

#include <type_traits>

#if __cplusplus >= 201703L
#include <optional>
#else
#include "nonstd/optional.hpp"
#endif

namespace lz {

#if __cplusplus >= 201703
using std::make_optional;
using std::nullopt;
using std::nullopt_t;
using std::optional;
#else
using nonstd::make_optional;
using nonstd::nullopt;
using nonstd::nullopt_t;
using nonstd::optional;
#endif

namespace detail {

struct OptionalBase {};
struct GeneratorBase {};
struct MiddlewareBase {};

template <typename T>
struct RvRefToVal {
  typedef T type;
};

template <typename T>
struct RvRefToVal<T &&> {
  typedef T type;
};

template <typename T>
using RmRvRef = typename RvRefToVal<T>::type;

template <typename T>
using RmRef = typename std::remove_reference<T>::type;

// template <typename T>
// using RmPtr = typename std::remove_pointer<T>::type;

template <typename T1, typename T2>
using EnableIfType =
    std::enable_if_t<std::is_base_of<T1, RmRef<T2>>::value, int>;

template <typename T1, typename T2>
using EnableIfNotType =
    std::enable_if_t<!std::is_base_of<T1, RmRef<T2>>::value, int>;

template <typename T1, typename T2>
using EnableIfSame = std::enable_if_t<std::is_same<T1, RmRef<T2>>::value, int>;

template <typename T1, typename T2>
using EnableIfNotSame =
    std::enable_if_t<!std::is_same<T1, RmRef<T2>>::value, int>;

// template <typename T1, typename T2>
// using EnableIfConv =
//     std::enable_if_t<std::is_convertible<T1, RmRef<T2>>::value, int>;

// template <typename T1, typename T2>
// using EnableIfNotConv =
//     std::enable_if_t<!std::is_convertible<T1, RmRef<T2>>::value, int>;

// template <typename T>
// using EnableIfLvRef = std::enable_if_t<std::is_lvalue_reference<T>::value,
// int>;

// template <typename T>
// using EnableIfNotLvRef =
//     std::enable_if_t<!std::is_lvalue_reference<T>::value, int>;

// template <typename T>
// using EnableIfRvRef = std::enable_if_t<std::is_rvalue_reference<T>::value,
// int>;

// template <typename T>
// using EnableIfNotRvRef =
//     std::enable_if_t<!std::is_rvalue_reference<T>::value, int>;

template <typename T>
using EnableIfFunc = std::enable_if_t<std::is_function<RmRef<T>>::value, int>;

template <typename T>
using EnableIfNotFunc =
    std::enable_if_t<!std::is_function<RmRef<T>>::value, int>;

}  // namespace detail

template <typename T>
using optionalref = optional<std::reference_wrapper<T>>;

template <typename T>
struct Optional : public optional<detail::RmRef<T>>, detail::OptionalBase {
  Optional() = default;
  Optional(nullopt_t) {}
  Optional(const T &t) : optional<detail::RmRef<T>>::optional(t) {}
  Optional(T &&t) : optional<detail::RmRef<T>>::optional(std::move(t)) {}
};

template <typename T>
struct Optional<T &> : public optionalref<T>, detail::OptionalBase {
  Optional() = default;
  Optional(nullopt_t) {}
  Optional(T &t) : optionalref<T>::optional(t) {}
  Optional(std::reference_wrapper<T> t) : optionalref<T>::optional(t) {}

  decltype(auto) operator*() { return optionalref<T>::operator*().get(); }
};

namespace detail {

template <typename T>
struct YieldTypeImpl {
  typedef T type;
};

template <typename T>
struct YieldTypeImpl<Optional<T>> {
  typedef typename YieldTypeImpl<T>::type type;
};

template <typename T>
struct YieldTypeImpl<Optional<T &&>> {
  typedef typename YieldTypeImpl<T>::type type;
};

//
// Should never yield a non-valued Optional type
//

struct NeverYieldNonValuedOptionalType {};

template <typename T>
struct YieldTypeImpl<Optional<T> &> {
  typedef NeverYieldNonValuedOptionalType type;
};

template <typename T>
struct YieldTypeImpl<const Optional<T> &> {
  typedef NeverYieldNonValuedOptionalType type;
};

template <typename T>
struct YieldTypeImpl<Optional<T> &&> {
  typedef NeverYieldNonValuedOptionalType type;
};

template <typename T>
struct YieldTypeImpl<const Optional<T> &&> {
  typedef NeverYieldNonValuedOptionalType type;
};

template <typename T>
using YieldType = typename YieldTypeImpl<T>::type;

template <typename F, typename... A>
using ResultType = decltype(std::declval<F>()(std::declval<A>()...));

}  // namespace detail

template <typename Gen, detail::EnableIfType<detail::GeneratorBase, Gen> = 0>
inline auto next(Gen &gen) {  // Yes, it's lvalue reference, calling from *this
  return Optional<typename detail::RmRef<Gen>::SubmitType>(gen());
}

namespace detail {

template <typename Gen, EnableIfType<GeneratorBase, Gen> = 0>
auto apply(Gen &gen, Optional<typename Gen::SubmitType> &res) {
  while (true) {
    auto val = next(gen);
    if (!val) {
      break;
    }
    res = std::move(val);
  }
}

template <typename Func, typename Mem>
class Generator;

template <typename Func, typename Mem>
class Generator : public GeneratorBase {
 public:
  // Generator is designed to be used as a middleware, so the data
  // must not be rvalue references.
  typedef RmRvRef<Func> FuncType;
  typedef RmRvRef<Mem> MemType;

  // InvokeType is the returning type of operator()
  typedef ResultType<FuncType &, MemType &> InvokeType;

  // SubmitType is the underlying data type never be an Optional
  typedef YieldType<InvokeType> SubmitType;

  template <typename F, typename T>
  Generator(F &&producer, T &&member) noexcept
      : producer(std::forward<F>(producer)), member(std::forward<T>(member)) {}

  decltype(auto) operator()() { return producer(member); }

  // use boolean expression as a way of evaluation & validation
  operator bool() {
    if (!applied) {
      apply(*this, value);
      applied = true;
    }
    return static_cast<bool>(value);
  }

  // forces to retrieve value on lvalue case only
  const auto &operator*() & {
    this->operator bool();
    return *value;
  }

 private:
  FuncType producer;
  MemType member;
  Optional<SubmitType> value;
  bool applied = false;
};

template <typename Func>
class Generator<Func, void> : public GeneratorBase {
 public:
  typedef RmRvRef<Func> FuncType;
  typedef ResultType<FuncType &> InvokeType;
  typedef YieldType<InvokeType> SubmitType;

  template <typename F>
  Generator(F &&producer) noexcept : producer(std::forward<F>(producer)) {}

  decltype(auto) operator()() { return producer(); }

  operator bool() {
    if (!applied) {
      apply(*this, value);
      applied = true;
    }
    return static_cast<bool>(value);
  }

  const auto &operator*() & {
    this->operator bool();
    return *value;
  }

 private:
  FuncType producer;
  Optional<SubmitType> value;
  bool applied = false;
};

template <typename Func, typename Mem>
class Middleware : public MiddlewareBase {
 public:
  typedef RmRvRef<Func> FuncType;
  typedef RmRvRef<Mem> MemType;

  template <typename F, typename T>
  Middleware(F &&producer, T &&member) noexcept
      : producer(std::forward<F>(producer)), member(std::forward<T>(member)) {}

  template <typename T>
  inline auto compose(T &&t) & {
    typedef decltype(member.compose(std::forward<T>(t))) GenType;
    return Generator<FuncType, GenType>(producer,
                                        member.compose(std::forward<T>(t)));
  }

  template <typename T>
  inline auto compose(T &&t) && {
    typedef decltype(member.compose(std::forward<T>(t))) GenType;
    return Generator<FuncType, GenType>(
        std::move(producer), std::move(member).compose(std::forward<T>(t)));
  }

 private:
  FuncType producer;
  MemType member;
};

template <typename Func>
class Middleware<Func, void> : public MiddlewareBase {
 public:
  typedef RmRvRef<Func> FuncType;

  template <typename F>
  Middleware(F &&producer) noexcept : producer(std::forward<F>(producer)) {}

  template <typename T>
  inline auto compose(T &&t) & {
    return Generator<FuncType, T>(producer, std::forward<T>(t));
  }

  template <typename T>
  inline auto compose(T &&t) && {
    return Generator<FuncType, T>(std::move(producer), std::forward<T>(t));
  }

 private:
  FuncType producer;
};

template <typename Func, typename Gen>
using GenYieldTypeImpl =
    Optional<ResultType<RmRef<Func> &, typename RmRef<Gen>::SubmitType &>>;

template <typename Func, typename Gen>
using GenYieldType = Optional<YieldType<GenYieldTypeImpl<Func, Gen>>>;

template <typename Func, typename Gen, EnableIfType<GeneratorBase, Gen> = 0,
          EnableIfType<OptionalBase, typename RmRef<Gen>::InvokeType> = 0>
inline GenYieldType<Func, Gen> genApply(Func &&f, Gen &&g) {
  auto val = g();
  if (!val) {
    return {};
  }

  // The GenYieldType use f(T &) to deduce return type,
  // and the returning code here, force to invoke with forwarded argument,
  // which means either Func's parameter is const T &,
  // or Gen::InvokeType is Optional<T &>.
  typedef typename RmRef<Gen>::SubmitType YieldType;
  return std::forward<Func>(f)(std::forward<YieldType>(*val));
}

template <typename Func, typename Gen, EnableIfType<GeneratorBase, Gen> = 0,
          EnableIfNotType<OptionalBase, typename RmRef<Gen>::InvokeType> = 0>
inline decltype(auto) genApply(Func &&f, Gen &&g) {
  return std::forward<Func>(f)(g());
}

// Since a valid component never be returning void, so use F() to deduce
// if given F is Entry or Middleware.
template <typename Func, EnableIfNotSame<void, ResultType<Func>> = 0>
inline decltype(auto) genApply(Func &&f) {
  return std::forward<Func>(f)();  // this is an Entry component
}

template <typename Func, typename... Param>
inline auto genApply(Func &&, Param &&...) {
  return MiddlewareBase{};
}

template <typename F, EnableIfNotType<MiddlewareBase, ResultType<F>> = 0>
inline auto generator(F &&f) {
  return Generator<F, void>(std::forward<F>(f));
}

template <typename F, EnableIfType<MiddlewareBase, ResultType<F>> = 0>
inline auto generator(F &&f) {
  return Middleware<F, void>{std::forward<F>(f)};
}

template <typename F, typename T, EnableIfNotType<MiddlewareBase, F> = 0>
inline auto generator(F &&f, T &&t) {
  static_assert(std::is_base_of<GeneratorBase, RmRef<T>>::value,
                "require a Generator");
  return Generator<F, T>(std::forward<F>(f), std::forward<T>(t));
}

template <typename F, typename T, EnableIfType<MiddlewareBase, F> = 0>
inline auto generator(F &&f, T &&t) {
  static_assert(std::is_base_of<GeneratorBase, RmRef<T>>::value,
                "require a Generator");
  return std::forward<F>(f).compose(std::forward<T>(t));
}

}  // namespace detail

// gen gives the target function an Optional wrapper if possible.
template <typename Func, detail::EnableIfNotFunc<Func> = 0>
inline auto gen(Func &&f) noexcept {
  return
      [f = std::forward<Func>(f)](auto &&... param) mutable -> decltype(auto) {
        return detail::genApply(std::forward<Func>(f),
                                std::forward<decltype(param)>(param)...);
      };
}

template <typename Func, detail::EnableIfFunc<Func> = 0>
inline auto gen(Func &&f) noexcept {
  return gen(&f);
}

// limit causes pipe stream evaluating at most N times.
inline auto limit(std::size_t n) noexcept {
  return [count = 0, n](auto &&gen) mutable -> decltype(next(gen)) {
    if (count++ < n) {
      return gen();
    }
    return {};
  };
}

namespace detail {}  // namespace detail

template <typename In, typename Out,
          detail::EnableIfType<detail::GeneratorBase, In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return detail::generator(std::forward<Out>(out), std::forward<In>(in));
}

template <typename In, typename Out,
          detail::EnableIfNotType<detail::GeneratorBase, In> = 0,
          detail::EnableIfType<detail::MiddlewareBase, In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return detail::Middleware<Out, In>(std::forward<Out>(out),
                                     std::forward<In>(in));
}
// TODO: if Out is Middleware

template <typename In, typename Out,
          detail::EnableIfNotType<detail::GeneratorBase, In> = 0,
          detail::EnableIfNotType<detail::MiddlewareBase, In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return operator|(detail::generator(std::forward<In>(in)),
                   std::forward<Out>(out));
}

template <typename Out, typename InR, typename... InA>
inline auto operator|(InR (*in)(InA...), Out &&out) noexcept {
  return operator|(gen(in), std::forward<Out>(out));
}

template <typename In, typename OutR, typename... OutA>
inline auto operator|(In &&in, OutR (*out)(OutA...)) noexcept {
  return operator|(std::forward<In>(in), gen(out));
}

}  // namespace lz
