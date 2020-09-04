#pragma once

#include <iterator>
#include <type_traits>
#include <utility>

#if __cplusplus >= 201703L
#include <optional>
#else
#include "nonstd/optional.hpp"
#endif

#define LZ_VERSION "0.0.1"

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
struct ComponentBase {};
struct GeneratorBase : ComponentBase {};
struct MiddlewareBase : ComponentBase {};

template <typename T>
struct RvRefToVal {
  using type = T;
};

template <typename T>
struct RvRefToVal<T &&> {
  using type = T;
};

template <typename T>
using RmRvRef = typename RvRefToVal<T>::type;

template <typename T>
using RmRef = typename std::remove_reference<T>::type;

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

template <typename T>
struct Optional<std::reference_wrapper<T>> : public Optional<T &> {
  Optional() = default;
  Optional(nullopt_t) {}
  Optional(T &t) : Optional<T &>(t) {}
  Optional(std::reference_wrapper<T> t) : Optional<T &>(t) {}

  decltype(auto) operator*() { return Optional<T &>::operator*(); }
};

namespace detail {

template <typename T>
struct YieldTypeImpl {
  using type = T;
};

template <typename T>
struct YieldTypeImpl<Optional<T>> {
  using type = typename YieldTypeImpl<T>::type;
};

template <typename T>
struct YieldTypeImpl<Optional<T &&>> {
  using type = typename YieldTypeImpl<T>::type;
};

//
// Should never yield a non-valued Optional type
//

struct NeverYieldNonValuedOptionalType {};

template <typename T>
struct YieldTypeImpl<Optional<T> &> {
  using type = NeverYieldNonValuedOptionalType;
};

template <typename T>
struct YieldTypeImpl<const Optional<T> &> {
  using type = NeverYieldNonValuedOptionalType;
};

template <typename T>
struct YieldTypeImpl<Optional<T> &&> {
  using type = NeverYieldNonValuedOptionalType;
};

template <typename T>
struct YieldTypeImpl<const Optional<T> &&> {
  using type = NeverYieldNonValuedOptionalType;
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

template <bool, typename = void>
struct UseIf {
  using type = void;
};

template <typename T>
struct UseIf<true, T> {
  using type = T;
};

template <typename T>
using UseVoidIfOptional =
    typename UseIf<!std::is_base_of<OptionalBase, RmRef<T>>::value, T>::type;

template <typename Gen>
class Iterator : public std::iterator<std::forward_iterator_tag,
                                      typename detail::RmRef<Gen>::SubmitType> {
 public:
  Iterator() : ended(true) {}
  explicit Iterator(Gen *g) : gen(g) {
    if (gen) {
      step();
    }
  }

  decltype(auto) operator*() { return *(gen->value); }

  auto operator==(const Iterator &other) {
    if (ended == other.ended) {
      return ended || (gen == other.gen);
    }
    return false;
  }
  auto operator!=(const Iterator &other) { return !operator==(other); }

  Iterator &operator++() {
    step();
    return *this;
  }

 private:
  void step() {
    if (ended) {
      return;
    }

    gen->value = next(*gen);
    if (!(gen->value)) {
      ended = true;
    } else {
      ++n;
    }
  }

  Gen *gen = nullptr;
  std::size_t n = 0;
  bool ended = false;
};

using Void = void;

template <typename Func, typename Mem>
class Generator;

template <typename Func, typename Mem>
class Generator : public GeneratorBase {
 public:
  // Generator is designed to be used as a middleware, so the data
  // must not be rvalue references.
  using FuncType = RmRvRef<Func>;
  using MemType = RmRvRef<Mem>;

  // InvokeType is the returning type of operator()
  using InvokeType = ResultType<FuncType &, MemType &>;

  // SubmitType is the underlying data type never be an Optional
  using SubmitType = YieldType<InvokeType>;

  // Ordinary forward iterator
  using iterator = Iterator<Generator>;

  template <typename F, typename M>
  struct Rebind {
    using type = Generator<F, M>;
  };

  template <typename F, typename T>
  Generator(F &&producer, T &&member) noexcept
      : producer(std::forward<F>(producer)), member(std::forward<T>(member)) {}

  decltype(auto) operator()() { return producer(member); }

  operator UseVoidIfOptional<InvokeType>() && { return operator()(); }

  // use boolean cast under lvalue scenes as a way of evaluation & validation
  operator bool() & {
    if (!applied) {
      apply(*this, value);
      applied = true;
    }
    return static_cast<bool>(value);
  }

  // forces to retrieve value on lvalue case only
  const auto &operator*() & { return *value; }

  auto begin() { return iterator{this}; }
  auto end() { return iterator{}; }

 private:
  FuncType producer;
  MemType member;
  Optional<SubmitType> value;
  bool applied = false;

  friend iterator;
};

template <typename Func>
class Generator<Func, Void> : public GeneratorBase {
 public:
  using FuncType = RmRvRef<Func>;
  using InvokeType = ResultType<FuncType &>;
  using SubmitType = YieldType<InvokeType>;
  using iterator = Iterator<Generator>;

  template <typename F, typename M>
  struct Rebind {
    using type = Generator<F, M>;
  };

  template <typename F>
  Generator(F &&producer) noexcept : producer(std::forward<F>(producer)) {}

  decltype(auto) operator()() { return producer(); }

  operator UseVoidIfOptional<InvokeType>() && { return operator()(); }

  operator bool() & {
    if (!applied) {
      apply(*this, value);
      applied = true;
    }
    return static_cast<bool>(value);
  }

  const auto &operator*() & { return *value; }

  auto begin() { return iterator{this}; }
  auto end() { return iterator{}; }

 private:
  FuncType producer;
  Optional<SubmitType> value;
  bool applied = false;

  friend iterator;
};

template <typename Func, typename Mem>
class Middleware : public MiddlewareBase {
 public:
  using FuncType = RmRvRef<Func>;
  using MemType = RmRvRef<Mem>;

  template <typename F, typename M>
  struct Rebind {
    using type = Middleware<F, M>;
  };

  template <typename F, typename T>
  Middleware(F &&producer, T &&member) noexcept
      : producer(std::forward<F>(producer)), member(std::forward<T>(member)) {}

  template <typename T>
  inline auto compose(T &&t) & {
    using MemType = decltype(member.compose(std::forward<T>(t)));
    using R = typename T::Rebind<FuncType, MemType>::type;
    return R(producer, member.compose(std::forward<T>(t)));
  }

  template <typename T>
  inline auto compose(T &&t) && {
    using MemType = decltype(member.compose(std::forward<T>(t)));
    using R = typename T::Rebind<FuncType, MemType>::type;
    return R(std::move(producer),
             std::move(member).compose(std::forward<T>(t)));
  }

 private:
  FuncType producer;
  MemType member;
};

template <typename Func>
class Middleware<Func, Void> : public MiddlewareBase {
 public:
  using FuncType = RmRvRef<Func>;

  template <typename F, typename M>
  struct Rebind {
    using type = Middleware<F, M>;
  };

  template <typename F>
  Middleware(F &&producer) noexcept : producer(std::forward<F>(producer)) {}

  template <typename T>
  inline auto compose(T &&t) & {
    using R = typename T::Rebind<FuncType, T>::type;
    return R(producer, std::forward<T>(t));
  }

  template <typename T>
  inline auto compose(T &&t) && {
    using R = typename T::Rebind<FuncType, T>::type;
    return R(std::move(producer), std::forward<T>(t));
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

  // Since val is lvalue and declare(*val) is lvalue-reference,
  // if we directly perfect forwarding *val, which is a reference
  // of a local variable, to f then might be causing danger,
  // so use SubmitType to forward.
  using YieldType = typename RmRef<Gen>::SubmitType;

  // The GenYieldType use f(T &) to deduce return type,
  // and the code here, force to invoke with forwarded argument,
  // which means to avoid one additional movement,
  // either Func's parameter is const T &,
  // or Gen::SubmitType is T &.
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
  return Generator<F, Void>(std::forward<F>(f));
}

template <typename F, EnableIfType<MiddlewareBase, ResultType<F>> = 0>
inline auto generator(F &&f) {
  return Middleware<F, Void>{std::forward<F>(f)};
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
// But if you wanna handle Generator yourself, e.g. a lambda defined as
//
//   [](auto &&gen) -> decltype(lz::next(gen)) {
//     auto val = lz::next(gen);
//     if (!val) { return {}; }
//
//     decltype(auto) data = *val;
//     ...
//   }
//
// DON't wrap it with lz::gen, which doesn't work, just piping(|) directly.
template <typename Func, detail::EnableIfNotFunc<Func> = 0,
          detail::EnableIfNotType<detail::ComponentBase, Func> = 0>
inline auto gen(Func &&f) noexcept {
  return
      [f = std::forward<Func>(f)](auto &&... param) mutable -> decltype(auto) {
        return detail::genApply(std::forward<Func>(f),
                                std::forward<decltype(param)>(param)...);
      };
}

template <typename Func, detail::EnableIfNotFunc<Func> = 0,
          detail::EnableIfType<detail::ComponentBase, Func> = 0>
inline auto gen(Func &&f) noexcept {
  return std::forward<Func>(f);
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

template <typename In, typename Out,
          detail::EnableIfType<detail::GeneratorBase, In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return detail::generator(std::forward<Out>(out), std::forward<In>(in));
}

template <typename In, typename Out,
          detail::EnableIfNotType<detail::GeneratorBase, In> = 0,
          detail::EnableIfType<detail::MiddlewareBase, In> = 0,
          detail::EnableIfNotType<detail::MiddlewareBase, Out> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return detail::Middleware<Out, In>(std::forward<Out>(out),
                                     std::forward<In>(in));
}

template <typename In, typename Out,
          detail::EnableIfNotType<detail::GeneratorBase, In> = 0,
          detail::EnableIfType<detail::MiddlewareBase, In> = 0,
          detail::EnableIfType<detail::MiddlewareBase, Out> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return std::forward<Out>(out).compose(std::forward<In>(in));
}

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

template <typename Func, typename... Args>
inline auto gen(Func &&f, Args &&... args) {
  return operator|(gen(std::forward<Func>(f)),
                   gen(std::forward<Args>(args)...));
}

}  // namespace lz
