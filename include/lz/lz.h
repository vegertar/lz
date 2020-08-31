#pragma once

#include <cstddef>
#include <functional>
#include <tuple>
#include <type_traits>
#include <utility>

#include "nonstd/optional.hpp"

namespace lz {

namespace detail {

struct GeneratorBase {};

struct OptionalBase {};

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

// template <typename T>
// using EnableIfFunc = std::enable_if_t<std::is_function<RmRef<T>>::value,
// int>;

// template <typename T>
// using EnableIfNotFunc =
//     std::enable_if_t<!std::is_function<RmRef<T>>::value, int>;

// // unpack tuple into arguments to function
// template <class F, size_t... Is>
// constexpr auto index_apply_impl(F &&f, std::index_sequence<Is...>) {
//   return f(std::integral_constant<size_t, Is>{}...);
// }

// template <size_t N, class F>
// constexpr auto index_apply(F &&f) {
//   return index_apply_impl(f, std::make_index_sequence<N>{});
// }

// template <class F, class Tuple>
// constexpr auto apply(F &&f, Tuple &&t) {
//   return index_apply<std::tuple_size<RmRef<Tuple>>::value>([&](auto... Is) {
//     return f(std::forward<decltype(std::get<Is>(t))>(std::get<Is>(t))...);
//   });
// }

// template <class F, class Tuple>
// constexpr auto applyWithoutForward(F &&f, Tuple &&t) {
//   return index_apply<std::tuple_size<RmRef<Tuple>>::value>(
//       [&](auto... Is) { return f(std::get<Is>(t)...); });
// }

}  // namespace detail

using nonstd::make_optional;
using nonstd::nullopt;
using nonstd::nullopt_t;
using nonstd::optional;

template <typename T>
using optionalref = optional<std::reference_wrapper<T>>;

template <typename T>
struct Optional : public optional<detail::RmRef<T>>, detail::OptionalBase {
  Optional() = default;
  Optional(nullopt_t t) : optional<detail::RmRef<T>>::optional(t) {}
  Optional(const T &t) : optional<detail::RmRef<T>>::optional(t) {}
  Optional(T &&t) : optional<detail::RmRef<T>>::optional(std::move(t)) {}
};

template <typename T>
struct Optional<T &> : public optionalref<T>, detail::OptionalBase {
  Optional() = default;
  Optional(nullopt_t t) : optionalref<T>::optional(t) {}
  Optional(T &t) : optionalref<T>::optional(t) {}
  Optional(std::reference_wrapper<T> t) : optionalref<T>::optional(t) {}

  decltype(auto) operator*() { return optionalref<T>::operator*().get(); }
};

// template <typename T>
// struct Optional<T &&> : public optional<T>, detail::OptionalBase {
//   Optional() = default;

//   template <typename V>
//   Optional(V &&v) : optional<T>::optional(std::forward<V>(v)) {}
// };

// template <typename T>
// struct Optional<Optional<T>> : public optional<T>, detail::OptionalBase {
//   Optional() = default;

//   template <typename V>
//   Optional(V &&v) : optional<T>::optional(std::forward<V>(v)) {}
// };

// template <typename T>
// struct Optional<Optional<T &>> : public optionalref<T>, detail::OptionalBase
// {
//   Optional() = default;

//   template <typename V>
//   Optional(V &&v) : optionalref<T>::optional(std::forward<V>(v)) {}

//   decltype(auto) operator*() { return optionalref<T>::operator*().get(); }
// };

// template <typename T>
// struct Optional<Optional<T &&>> : public optional<T>, detail::OptionalBase {
//   Optional() = default;

//   template <typename V>
//   Optional(V &&v) : optional<T>::optional(std::forward<V>(v)) {}
// };

namespace detail {

// template <typename T>
// struct FunctionTraitsImpl;

// template <typename ReturnType, typename... Args>
// struct FunctionTraitsImpl<ReturnType(Args...)> {
//   // Arity is the number of arguments.
//   enum { Arity = sizeof...(Args) };

//   typedef ReturnType ResultType;

//   template <std::size_t i>
//   struct Arg {
//     typedef typename std::tuple_element<i, std::tuple<Args...>>::type Type;
//     // the i-th argument is equivalent to the i-th tuple element of a
//     // tuple composed of those arguments.
//   };
// };

// template <typename ClassType, typename ReturnType, typename... Args>
// struct FunctionTraitsImpl<ReturnType (ClassType::*)(Args...) const> {
//   // Arity is the number of arguments.
//   enum { Arity = sizeof...(Args) };

//   typedef ReturnType ResultType;

//   template <std::size_t i>
//   struct Arg {
//     typedef typename std::tuple_element<i, std::tuple<Args...>>::type Type;
//     // the i-th argument is equivalent to the i-th tuple element of a tuple
//     // composed of those arguments.
//   };
// };

// template <typename T, bool>
// struct FunctionTraits;

// template <typename T>
// struct FunctionTraits<T, true>
//     : public FunctionTraitsImpl<decltype(&T::operator())> {};

// template <typename T>
// struct FunctionTraits<T, false> : public FunctionTraitsImpl<RmPtr<RmRef<T>>>
// {};

// template <typename T, std::size_t i>
// using ArgType =
//     typename FunctionTraits<T, std::is_class<T>::value>::template
//     Arg<i>::Type;

// template <typename T>
// using ResultType =
//     typename FunctionTraits<T, std::is_class<T>::value>::ResultType;

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

template <typename T>
struct YieldTypeImpl<Optional<T> &> {
  typedef std::false_type type;
};

template <typename T>
struct YieldTypeImpl<const Optional<T> &> {
  typedef std::false_type type;
};

template <typename T>
struct YieldTypeImpl<Optional<T> &&> {
  typedef std::false_type type;
};

template <typename T>
struct YieldTypeImpl<const Optional<T> &&> {
  typedef std::false_type type;
};

template <typename T>
using YieldType = typename YieldTypeImpl<T>::type;

template <typename F, typename... A>
using ResultType = decltype(std::declval<F>()(std::declval<A>()...));

}  // namespace detail

// template <typename T>
// inline auto wrap(T &&val) {
//   return Optional<T>(std::forward<T>(val));
// }

// template <typename T>
// inline decltype(auto) wrap(Optional<T> &val) {
//   return val;
// }

// template <typename T>
// inline decltype(auto) wrap(const Optional<T> &val) {
//   return val;
// }

// template <typename T>
// inline auto wrap(Optional<T> &&val) {
//   return std::move(val);
// }

// template <typename T>
// inline auto wrap(const Optional<T> &&val) {
//   return std::move(val);
// }

// template <typename Gen, detail::EnableIfType<detail::GeneratorBase, Gen> = 0>
// inline decltype(auto) next(Gen &&gen) {
//   return wrap(std::forward<Gen>(gen)());
// }

template <typename Gen, detail::EnableIfType<detail::GeneratorBase, Gen> = 0>
auto apply(Gen &gen, lz::Optional<typename Gen::YieldType> &res) {
  while (true) {
    detail::RmRef<decltype(res)> val = gen();
    if (!val) {
      break;
    }
    res = std::move(val);
  }
}

template <typename Func, typename Mem>
class Generator;

template <typename Func, typename Mem>
class Generator : public detail::GeneratorBase {
 public:
  // Generator is designed to be used as a middleware, so the data
  // must not be rvalue references.
  typedef detail::RmRvRef<Func> FuncType;
  typedef detail::RmRvRef<Mem> MemType;

  // InvokeType is the returning type of operator()
  typedef detail::ResultType<FuncType &, MemType &> InvokeType;

  // YieldType is the underlying data type never be an Optional
  typedef detail::YieldType<InvokeType> YieldType;

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
  Optional<YieldType> value;
  bool applied = false;
};

template <typename Func>
class Generator<Func, void> : public detail::GeneratorBase {
 public:
  typedef detail::RmRvRef<Func> FuncType;
  typedef detail::ResultType<FuncType &> InvokeType;
  typedef detail::YieldType<InvokeType> YieldType;

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
  Optional<YieldType> value;
  bool applied = false;
};

// template <typename Func>
// auto unpack(Func &&func) {
//   return [funcIn = std::forward<Func>(func)](auto &&tup) {
//     return detail::apply(funcIn, tup);
//   };
// }

template <typename F>
inline auto generator(F &&f) {
  return Generator<F, void>(std::forward<F>(f));
}

template <typename F, typename T>
inline auto generator(F &&f, T &&t) {
  static_assert(std::is_base_of<detail::GeneratorBase, detail::RmRef<T>>::value,
                "require a Generator");
  return Generator<F, T>(std::forward<F>(f), std::forward<T>(t));
}

namespace detail {

template <typename Func, typename Gen>
using GenYieldTypeImpl =
    Optional<ResultType<RmRef<Func> &, typename RmRef<Gen>::YieldType &>>;

template <typename Func, typename Gen>
using GenYieldType = Optional<YieldType<GenYieldTypeImpl<Func, Gen>>>;

template <typename Func, typename Gen, EnableIfType<GeneratorBase, Gen> = 0,
          EnableIfType<OptionalBase, typename RmRef<Gen>::InvokeType> = 0>
inline GenYieldType<Func, Gen> genApply(Func &&f, Gen &&g) {
  auto val = g();
  if (!val) {
    return nullopt;
  }

  typedef typename RmRef<Gen>::YieldType YieldType;
  return std::forward<Func>(f)(std::forward<YieldType>(*val));
}

template <typename Func, typename Gen, EnableIfType<GeneratorBase, Gen> = 0,
          EnableIfNotType<OptionalBase, typename RmRef<Gen>::InvokeType> = 0>
inline decltype(auto) genApply(Func &&f, Gen &&g) {
  return std::forward<Func>(f)(g());
}

template <typename Func>
inline decltype(auto) genApply(Func &&f) {
  return std::forward<Func>(f)();
}

}  // namespace detail

// gen gives the target function an Optional wrapper if possible.
template <typename Func>
inline auto gen(Func &&f) noexcept {
  return
      [f = std::forward<Func>(f)](auto &&... param) mutable -> decltype(auto) {
        return detail::genApply(std::forward<Func>(f),
                                std::forward<decltype(param)>(param)...);
      };
}

// gen gives the target function an Optional wrapper if possible.
template <typename R, class... A>
inline auto gen(R (*f)(A...)) noexcept {
  return [f](auto &&... param) mutable -> decltype(auto) {
    return detail::genApply(f, std::forward<decltype(param)>(param)...);
  };
}

// limit causes pipe stream evaluating at most N times.
inline auto limit(std::size_t n) noexcept {
  return [count = 0, n](auto &&gen) mutable -> decltype(auto) {
    typedef typename detail::RmRef<decltype(gen)>::YieldType YieldType;
    return count++ < n ? gen() : Optional<YieldType>(nullopt);
  };
}

template <typename In, typename Out,
          detail::EnableIfType<detail::GeneratorBase, In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return generator(std::forward<Out>(out), std::forward<In>(in));
}

template <typename In, typename Out,
          detail::EnableIfNotType<detail::GeneratorBase, In> = 0>
inline auto operator|(In &&in, Out &&out) noexcept {
  return generator(std::forward<Out>(out), generator(std::forward<In>(in)));
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
