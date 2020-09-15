#pragma once

#include <iterator>
#include <type_traits>
#include <utility>

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
using RmRef = typename std::remove_reference_t<T>;

template <typename T>
using RmCvRef = typename std::remove_cv_t<std::remove_reference_t<T>>;

template <typename T1, typename T2>
using EnableIfType =
    std::enable_if_t<std::is_base_of<T1, std::decay_t<T2>>::value, int>;

template <typename T1, typename T2>
using EnableIfNotType =
    std::enable_if_t<!std::is_base_of<T1, std::decay_t<T2>>::value, int>;

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

template <typename T>
using EnableIfCom = EnableIfType<ComponentBase, T>;

template <typename T>
using EnableIfNotCom = EnableIfNotType<ComponentBase, T>;

template <typename T>
using EnableIfGen = EnableIfType<GeneratorBase, T>;

template <typename T>
using EnableIfNotGen = EnableIfNotType<GeneratorBase, T>;

template <typename T>
using EnableIfMid = EnableIfType<MiddlewareBase, T>;

template <typename T>
using EnableIfNotMid = EnableIfNotType<MiddlewareBase, T>;

template <typename T>
using EnableIfOpt = EnableIfType<OptionalBase, T>;

template <typename T>
using EnableIfNotOpt = EnableIfNotType<OptionalBase, T>;

template <typename T>
using EnableIfNull = EnableIfType<nullopt_t, T>;

template <typename T>
using EnableIfNotNull = EnableIfNotType<nullopt_t, T>;

}  // namespace detail

template <typename T>
using optionalref = optional<std::reference_wrapper<T>>;

template <typename T>
struct Optional : optional<detail::RmCvRef<T>>, detail::OptionalBase {
  Optional() noexcept = default;

  template <typename U, detail::EnableIfNull<U> = 0>
  Optional(const Optional<U> &) {}

  template <typename U, detail::EnableIfNull<U> = 0>
  Optional(const U &) noexcept {}

  template <typename U, detail::EnableIfNotNull<U> = 0>
  Optional(const U &t) noexcept : optional<detail::RmCvRef<T>>::optional(t) {}

  Optional(T &&t) noexcept
      : optional<detail::RmCvRef<T>>::optional(std::move(t)) {}
};

template <typename T>
struct Optional<T &> : optionalref<T>, detail::OptionalBase {
  Optional() noexcept = default;
  Optional(nullopt_t) noexcept {}
  Optional(T &t) noexcept : optionalref<T>::optional(t) {}
  Optional(std::reference_wrapper<T> t) noexcept
      : optionalref<T>::optional(t) {}

  decltype(auto) operator*() noexcept {
    return optionalref<T>::operator*().get();
  }
};

template <typename T>
struct Optional<std::reference_wrapper<T>> : Optional<T &> {
  Optional() noexcept = default;
  Optional(nullopt_t) noexcept {}
  Optional(T &t) noexcept : Optional<T &>(t) {}
  Optional(std::reference_wrapper<T> t) noexcept : Optional<T &>(t) {}

  decltype(auto) operator*() noexcept { return Optional<T &>::operator*(); }
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

template <typename Gen, detail::EnableIfGen<Gen> = 0>
inline auto get(Gen &&gen) noexcept {
  return Optional<typename std::decay_t<Gen>::SubmitType>(
      std::forward<Gen>(gen)());
}

namespace detail {

template <typename Gen, EnableIfGen<Gen> = 0>
auto apply(Gen &gen, Optional<typename Gen::SubmitType> &res) noexcept {
  while (true) {
    auto val = get(gen);
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
    typename UseIf<!std::is_base_of<OptionalBase, std::decay_t<T>>::value,
                   T>::type;

template <typename Gen>
class Iterator : public std::iterator<std::forward_iterator_tag,
                                      typename std::decay_t<Gen>::SubmitType> {
 public:
  Iterator() noexcept : ended(true) {}
  explicit Iterator(Gen *g) noexcept : gen(g) {
    if (gen) {
      step();
    }
  }

  decltype(auto) operator*() noexcept { return *(gen->value); }

  decltype(auto) operator++() noexcept {
    step();
    return *this;
  }

  auto operator==(const Iterator &other) noexcept {
    if (ended == other.ended) {
      return ended || (gen == other.gen && n == other.n);
    }
    return false;
  }
  auto operator!=(const Iterator &other) noexcept { return !operator==(other); }

 private:
  void step() noexcept {
    if (ended) {
      return;
    }

    gen->value = get(*gen);
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

  decltype(auto) operator()() noexcept { return producer(member); }

  operator UseVoidIfOptional<InvokeType>() &&noexcept { return operator()(); }

  // use boolean cast under lvalue scenes as a way of evaluation & validation
  operator bool() &noexcept {
    if (!applied) {
      apply(*this, value);
      applied = true;
    }
    return static_cast<bool>(value);
  }

  // forces to retrieve value on lvalue case only
  const auto &operator*() &noexcept { return *value; }
  const auto &operator->() &noexcept { return value; }

  auto begin() noexcept { return iterator{this}; }
  auto end() noexcept { return iterator{}; }

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

  template <typename F, EnableIfNotSame<Generator, F> = 0>
  Generator(F &&producer) noexcept : producer(std::forward<F>(producer)) {}

  decltype(auto) operator()() noexcept { return producer(); }

  operator UseVoidIfOptional<InvokeType>() &&noexcept { return operator()(); }

  operator bool() &noexcept {
    if (!applied) {
      apply(*this, value);
      applied = true;
    }
    return static_cast<bool>(value);
  }

  const auto &operator*() &noexcept { return *value; }
  const auto &operator->() &noexcept { return value; }

  auto begin() noexcept { return iterator{this}; }
  auto end() noexcept { return iterator{}; }

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
  inline auto compose(T &&t) &noexcept {
    using MemType = decltype(member.compose(std::forward<T>(t)));
    using R =
        typename std::decay_t<T>::template Rebind<FuncType, MemType>::type;
    return R(producer, member.compose(std::forward<T>(t)));
  }

  template <typename T>
  inline auto compose(T &&t) &&noexcept {
    using MemType = decltype(member.compose(std::forward<T>(t)));
    using R =
        typename std::decay_t<T>::template Rebind<FuncType, MemType>::type;
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

  template <typename F, EnableIfNotSame<Middleware, F> = 0>
  Middleware(F &&producer) noexcept : producer(std::forward<F>(producer)) {}

  template <typename T>
  inline auto compose(T &&t) &noexcept {
    using R = typename std::decay_t<T>::template Rebind<FuncType, T>::type;
    return R(producer, std::forward<T>(t));
  }

  template <typename T>
  inline auto compose(T &&t) &&noexcept {
    using R = typename std::decay_t<T>::template Rebind<FuncType, T>::type;
    return R(std::move(producer), std::forward<T>(t));
  }

 private:
  FuncType producer;
};

template <typename Func, typename Gen>
using GenInvokeType =
    ResultType<RmRef<Func> &, typename std::decay_t<Gen>::SubmitType &>;

template <typename Func, typename Gen, typename = void>
struct GenYieldTypeImpl {
  using type = Optional<YieldType<Optional<GenInvokeType<Func, Gen>>>>;
};

template <typename Func, typename Gen>
struct GenYieldTypeImpl<
    Func, Gen,
    std::enable_if_t<std::is_base_of<
        nullopt_t, typename std::decay_t<Gen>::SubmitType>::value>> {
  using type = Optional<nullopt_t>;
};

template <typename Func, typename Gen>
using GenYieldType = typename GenYieldTypeImpl<Func, Gen>::type;

template <typename Func, typename Gen, EnableIfGen<Gen> = 0,
          EnableIfOpt<typename std::decay_t<Gen>::InvokeType> = 0>
inline GenYieldType<Func, Gen> genApply(Func &&f, Gen &&g) noexcept {
  auto val = std::forward<Gen>(g)();
  if (!val) {
    return {};
  }

  // Since val is lvalue and declare(*val) is lvalue-reference,
  // if we directly perfect forwarding *val, which is a reference
  // of a local variable, to f then might be causing danger,
  // so use SubmitType to forward.
  using YieldType = typename std::decay_t<Gen>::SubmitType;

  // The GenYieldType use f(T &) to deduce return type,
  // and the code here, force to invoke with forwarded argument,
  // which means to avoid one additional movement,
  // either Func's parameter is const T &,
  // or Gen::SubmitType is T &.
  return std::forward<Func>(f)(std::forward<YieldType>(*val));
}

template <typename Func, typename Gen, EnableIfGen<Gen> = 0,
          EnableIfNotOpt<typename std::decay_t<Gen>::InvokeType> = 0,
          EnableIfNotNull<typename std::decay_t<Gen>::InvokeType> = 0>
inline decltype(auto) genApply(Func &&f, Gen &&g) noexcept {
  return std::forward<Func>(f)(std::forward<Gen>(g)());
}

template <typename Func, typename Gen, EnableIfGen<Gen> = 0,
          EnableIfNull<typename std::decay_t<Gen>::InvokeType> = 0>
inline decltype(auto) genApply(Func &&, Gen &&g) noexcept {
  return std::forward<Gen>(g)();
}

// Since a valid component never be returning void, so use F() to deduce
// if given F is Entry or Middleware.
template <typename Func, EnableIfNotSame<void, ResultType<Func>> = 0>
inline decltype(auto) genApply(Func &&f) noexcept {
  return std::forward<Func>(f)();  // this is an Entry component
}

template <typename Func, typename... Param>
inline auto genApply(Func &&, Param &&...) noexcept {
  return MiddlewareBase{};
}

template <typename Left, typename Right, EnableIfGen<Left> = 0,
          EnableIfGen<Right> = 0>
inline auto andApply(Left &&l, Right &&r) noexcept -> decltype(get(r)) {
  if (get(std::forward<Left>(l))) {
    return get(std::forward<Right>(r));
  }
  return {};
}

template <typename Left, typename Right, typename Gen, EnableIfMid<Left> = 0,
          EnableIfMid<Right> = 0, EnableIfGen<Gen> = 0>
inline auto andApply(Left &&l, Right &&r, Gen &&g) noexcept
    -> decltype(get(r.compose(g))) {
  if (get(std::forward<Left>(l).compose(g))) {
    return get(std::forward<Right>(r).compose(std::forward<Gen>(g)));
  }
  return {};
}

template <typename Left, typename Right, typename... Param>
inline auto andApply(Left &&l, Right &&r, Param &&...) noexcept {
  return MiddlewareBase{};
}

template <typename Left, typename Right>
inline auto genAnd(Left &&l, Right &&r) noexcept {
  using L = std::decay_t<Left>;
  using R = std::decay_t<Right>;

  static_assert((std::is_base_of<GeneratorBase, L>::value &&
                 std::is_base_of<GeneratorBase, R>::value) ||
                    (std::is_base_of<MiddlewareBase, L>::value &&
                     std::is_base_of<MiddlewareBase, R>::value),
                "should both be Generator or Middleware");

  return [l = std::forward<Left>(l), r = std::forward<Right>(r)](
             auto &&... param) mutable noexcept -> decltype(auto) {
    return andApply(std::forward<Left>(l), std::forward<Right>(r),
                    std::forward<decltype(param)>(param)...);
  };
}

template <typename L, typename R, typename = void>
struct OrYieldTypeImpl;

template <typename L, typename R>
struct OrYieldTypeImpl<
    L, R,
    std::enable_if_t<
        std::is_same<L, R>::value ||
        std::is_base_of<nullopt_t, std::decay_t<YieldType<R>>>::value>> {
  using type = L;
};

template <typename L, typename R>
struct OrYieldTypeImpl<
    L, R,
    std::enable_if_t<
        !std::is_same<L, R>::value &&
        std::is_base_of<nullopt_t, std::decay_t<YieldType<L>>>::value>> {
  using type = R;
};

template <typename L, typename R>
using OrYieldType = typename OrYieldTypeImpl<L, R>::type;

template <typename Left, typename Right, EnableIfGen<Left> = 0,
          EnableIfGen<Right> = 0>
inline auto orApply(Left &&l, Right &&r) noexcept
    -> OrYieldType<decltype(get(l)), decltype(get(r))> {
  auto val = get(std::forward<Left>(l));
  if (val) {
    return val;
  }

  return get(std::forward<Right>(r));
}

template <typename Left, typename Right, typename Gen, EnableIfMid<Left> = 0,
          EnableIfMid<Right> = 0, EnableIfGen<Gen> = 0>
inline auto orApply(Left &&l, Right &&r, Gen &&g) noexcept
    -> OrYieldType<decltype(get(l.compose(g))), decltype(get(r.compose(g)))> {
  auto val = get(std::forward<Left>(l).compose(g));
  if (val) {
    return val;
  }

  return get(std::forward<Right>(r).compose(std::forward<Gen>(g)));
}

template <typename Left, typename Right, typename... Param>
inline auto orApply(Left &&l, Right &&r, Param &&...) noexcept {
  return MiddlewareBase{};
}

template <typename Left, typename Right>
inline auto genOr(Left &&l, Right &&r) noexcept {
  using L = std::decay_t<Left>;
  using R = std::decay_t<Right>;

  static_assert((std::is_base_of<GeneratorBase, L>::value &&
                 std::is_base_of<GeneratorBase, R>::value) ||
                    (std::is_base_of<MiddlewareBase, L>::value &&
                     std::is_base_of<MiddlewareBase, R>::value),
                "should both be Generator or Middleware");

  return [l = std::forward<Left>(l), r = std::forward<Right>(r)](
             auto &&... param) mutable noexcept -> decltype(auto) {
    return orApply(std::forward<Left>(l), std::forward<Right>(r),
                   std::forward<decltype(param)>(param)...);
  };
}

template <typename F, EnableIfNotMid<ResultType<F>> = 0>
inline auto generator(F &&f) noexcept {
  return Generator<F, Void>(std::forward<F>(f));
}

template <typename F, EnableIfMid<ResultType<F>> = 0>
inline auto generator(F &&f) noexcept {
  return Middleware<F, Void>{std::forward<F>(f)};
}

template <typename F, typename T, EnableIfNotMid<F> = 0>
inline auto generator(F &&f, T &&t) noexcept {
  static_assert(std::is_base_of<GeneratorBase, std::decay_t<T>>::value,
                "requires a Generator");
  return Generator<F, T>(std::forward<F>(f), std::forward<T>(t));
}

template <typename F, typename T, EnableIfMid<F> = 0>
inline auto generator(F &&f, T &&t) noexcept {
  static_assert(std::is_base_of<GeneratorBase, std::decay_t<T>>::value,
                "requires a Generator");
  return std::forward<F>(f).compose(std::forward<T>(t));
}

template <typename F, typename... T>
inline auto generator(F &&f, T &&...) noexcept {
  return Middleware<F, Void>{std::forward<F>(f)};
}

}  // namespace detail

// gen gives the target function an Optional wrapper if possible.
// But if you wanna handle Generator yourself, e.g. a lambda defined as
//
//   [](auto &&gen) -> decltype(lz::get(gen)) {
//     auto val = lz::get(gen);
//     if (!val) { return {}; }
//
//     decltype(auto) data = *val;
//     ...
//   }
//
// DON't wrap it with lz::gen, which doesn't work, just piping(|) directly.
template <typename Func, detail::EnableIfNotFunc<Func> = 0,
          detail::EnableIfNotCom<Func> = 0>
inline auto gen(Func &&f) noexcept {
  return [f = std::forward<Func>(f)](
             auto &&... param) mutable noexcept -> decltype(auto) {
    return detail::genApply(std::forward<Func>(f),
                            std::forward<decltype(param)>(param)...);
  };
}

template <typename Func, detail::EnableIfNotFunc<Func> = 0,
          detail::EnableIfCom<Func> = 0>
inline auto gen(Func &&f) noexcept {
  return std::forward<Func>(f);
}

template <typename Func, detail::EnableIfFunc<Func> = 0>
inline auto gen(Func &&f) noexcept {
  return gen(&f);
}

// limit causes pipe stream evaluating at most N times.
inline auto limit(std::size_t n) noexcept {
  return [count = 0, n](auto &&gen) mutable noexcept -> decltype(get(gen)) {
    if (count++ < n) {
      return gen();
    }
    return {};
  };
}

template <typename In, typename Out, detail::EnableIfGen<In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return detail::generator(std::forward<Out>(out), std::forward<In>(in));
}

template <typename In, typename Out, detail::EnableIfMid<In> = 0,
          detail::EnableIfNotMid<Out> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return detail::Middleware<Out, In>(std::forward<Out>(out),
                                     std::forward<In>(in));
}

template <typename In, typename Out, detail::EnableIfMid<In> = 0,
          detail::EnableIfMid<Out> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return std::forward<Out>(out).compose(std::forward<In>(in));
}

template <typename In, typename Out, detail::EnableIfNotCom<In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return operator|(detail::generator(std::forward<In>(in)),
                   std::forward<Out>(out));
}

template <typename Out, typename InR, typename... InA>
inline auto operator|(InR (*in)(InA...), Out &&out) noexcept {
  return operator|(gen(in), std::forward<Out>(out));
}

template <typename In, typename RightR, typename... RightA>
inline auto operator|(In &&in, RightR (*out)(RightA...)) noexcept {
  return operator|(std::forward<In>(in), gen(out));
}

template <typename Left, typename Right, detail::EnableIfCom<Left> = 0,
          detail::EnableIfCom<Right> = 0>
inline auto operator&&(Left &&left, Right &&right) noexcept {
  return detail::generator(
      detail::genAnd(std::forward<Left>(left), std::forward<Right>(right)));
}

template <typename Left, typename Right, detail::EnableIfCom<Left> = 0,
          detail::EnableIfNotCom<Right> = 0>
inline auto operator&&(Left &&left, Right &&right) noexcept {
  return operator&&(std::forward<Left>(left),
                    detail::generator(std::forward<Right>(right)));
}

template <typename Left, typename Right, detail::EnableIfNotCom<Left> = 0>
inline auto operator&&(Left &&left, Right &&right) noexcept {
  return operator&&(detail::generator(std::forward<Left>(left)),
                    std::forward<Right>(right));
}

template <typename Right, typename LeftR, typename... LeftA>
inline auto operator&&(LeftR (*left)(LeftA...), Right &&right) noexcept {
  return operator&&(gen(left), std::forward<Right>(right));
}

template <typename Left, typename RightR, typename... RightA>
inline auto operator&&(Left &&left, RightR (*right)(RightA...)) noexcept {
  return operator&&(std::forward<Left>(left), gen(right));
}

template <typename Left, typename Right, detail::EnableIfCom<Left> = 0,
          detail::EnableIfCom<Right> = 0>
inline auto operator||(Left &&left, Right &&right) noexcept {
  return detail::generator(
      detail::genOr(std::forward<Left>(left), std::forward<Right>(right)));
}

template <typename Left, typename Right, detail::EnableIfCom<Left> = 0,
          detail::EnableIfNotCom<Right> = 0>
inline auto operator||(Left &&left, Right &&right) noexcept {
  return operator||(std::forward<Left>(left),
                    detail::generator(std::forward<Right>(right)));
}

template <typename Left, typename Right, detail::EnableIfNotCom<Left> = 0>
inline auto operator||(Left &&left, Right &&right) noexcept {
  return operator||(detail::generator(std::forward<Left>(left)),
                    std::forward<Right>(right));
}

template <typename Right, typename LeftR, typename... LeftA>
inline auto operator||(LeftR (*left)(LeftA...), Right &&right) noexcept {
  return operator||(gen(left), std::forward<Right>(right));
}

template <typename Left, typename RightR, typename... RightA>
inline auto operator||(Left &&left, RightR (*right)(RightA...)) noexcept {
  return operator||(std::forward<Left>(left), gen(right));
}

template <typename T>
inline auto operator+(T &t) noexcept {
  return t;
}

template <typename T>
inline auto operator-(T &t) noexcept {
  return std::move(t);
}

template <typename Func, typename... Args>
inline auto gen(Func &&f, Args &&... args) noexcept {
  return operator|(gen(std::forward<Func>(f)),
                   gen(std::forward<Args>(args)...));
}

}  // namespace lz
