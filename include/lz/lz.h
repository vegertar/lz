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

struct OptionOrRefBase {};

template <typename T>
using RmRef = typename std::remove_reference<T>::type;

template <typename T>
struct StdRefToRef {
  typedef T type;
};

template <typename T>
struct StdRefToRef<std::reference_wrapper<T>> {
  typedef T &type;
};

template <typename T>
struct StdRefToRef<std::reference_wrapper<T> &&> {
  typedef T &type;
};

template <typename T>
RmRef<T> &&wrapIfRef(T &&v) {
  return std::move(v);
}

template <typename T>
typename std::reference_wrapper<T> wrapIfRef(T &v) {
  return std::ref(v);
}

template <typename T1, typename T2>
using EnableIfType =
    typename std::enable_if<std::is_base_of<T1, RmRef<T2>>::value, int>::type;

template <typename T1, typename T2>
using EnableIfNotType =
    typename std::enable_if<!std::is_base_of<T1, RmRef<T2>>::value, int>::type;

template <typename T>
using EnableIfYieldRef =
    typename std::enable_if<std::is_reference<typename T::YieldType>::value,
                            int>::type;

template <typename T>
using EnableIfYieldVal =
    typename std::enable_if<!std::is_reference<typename T::YieldType>::value,
                            int>::type;

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
using nonstd::optional;

template <typename T>
using optionalref = optional<std::reference_wrapper<T>>;

template <typename T>
struct OptionOrRef : public optional<T>, detail::OptionOrRefBase {
  OptionOrRef() = default;

  template <typename V>
  OptionOrRef(V &&v) : optional<T>::optional(std::forward<V>(v)) {}
};

template <typename T>
struct OptionOrRef<T &> : public optionalref<T>, detail::OptionOrRefBase {
  OptionOrRef() = default;

  template <typename V>
  OptionOrRef(V &&v) : optionalref<T>::optional(std::forward<V>(v)) {}

  decltype(auto) operator*() { return optionalref<T>::operator*().get(); }
};

template <typename T>
struct OptionOrRef<T &&> : public optional<T>, detail::OptionOrRefBase {
  OptionOrRef() = default;

  template <typename V>
  OptionOrRef(V &&v) : optional<T>::optional(std::forward<V>(v)) {}
};

template <typename T>
struct OptionOrRef<OptionOrRef<T>> : public optional<T>,
                                     detail::OptionOrRefBase {
  OptionOrRef() = default;

  template <typename V>
  OptionOrRef(V &&v) : optional<T>::optional(std::forward<V>(v)) {}
};

template <typename T>
struct OptionOrRef<OptionOrRef<T &>> : public optionalref<T>,
                                       detail::OptionOrRefBase {
  OptionOrRef() = default;

  template <typename V>
  OptionOrRef(V &&v) : optionalref<T>::optional(std::forward<V>(v)) {}

  decltype(auto) operator*() { return optionalref<T>::operator*().get(); }
};

template <typename T>
struct OptionOrRef<OptionOrRef<T &&>> : public optional<T>,
                                        detail::OptionOrRefBase {
  OptionOrRef() = default;

  template <typename V>
  OptionOrRef(V &&v) : optional<T>::optional(std::forward<V>(v)) {}
};

namespace detail {

//
// lambda trains, See also: https://stackoverflow.com/a/7943765
//
// For generic types, directly use the result of the signature of its
// 'operator()' template <typename T> struct FunctionTraits
//     : public FunctionTraits<decltype(&T::operator())> {};

// // We specialize for pointers to member function
// template <typename ClassType, typename ReturnType, typename... Args>
// struct FunctionTraits<ReturnType(ClassType::*)(Args...) const> {
//     // Arity is the number of arguments.
//     enum { Arity = sizeof...(Args) };

//     typedef ReturnType ResultType;

//     template <std::size_t i>
//     struct Arg {
//         typedef typename std::tuple_element<i, std::tuple<Args...>>::type
//         Type;
//         // the i-th argument is equivalent to the i-th tuple element of a
//         tuple
//         // composed of those arguments.
//     };
// };

// template <typename T, std::size_t i>
// using ArgType = typename FunctionTraits<T>::Arg<i>::Type;

// template <typename T>
// using ResultType = typename FunctionTraits<T>::ResultType;

template <typename T>
struct YieldTypeImpl {
  typedef T type;
};

template <typename T>
struct YieldTypeImpl<optional<T>> {
  typedef T type;
};

template <typename T>
struct YieldTypeImpl<optionalref<T>> {
  typedef T &type;
};

template <typename T>
struct YieldTypeImpl<OptionOrRef<T>> {
  typedef T type;
};

template <typename T>
struct YieldTypeImpl<OptionOrRef<T &>> {
  typedef T &type;
};

template <typename T>
struct YieldTypeImpl<OptionOrRef<T &&>> {
  typedef T type;
};

// template <typename T>
// struct YieldTypeImpl<OptionOrRef<T> &&> {
//   typedef T type;
// };

template <typename T>
using YieldType = typename YieldTypeImpl<T>::type;

}  // namespace detail

template <typename Gen, detail::EnableIfType<detail::GeneratorBase, Gen> = 0>
decltype(auto) apply(Gen &&gen) {
  typedef typename detail::RmRef<Gen>::YieldType YieldType;
  OptionOrRef<YieldType> res;
  while (true) {
    // N movements
    decltype(auto) val = gen();
    if (!val) {
      break;
    }
    // 1 movement + (N - 1) move-assignments
    res = std::forward<decltype(val)>(val);
  }
  // 1 movement
  return res;
}

template <typename Member, typename ProducerFunc>
class Generator;

template <typename Member, typename ProducerFunc>
class Generator : public detail::GeneratorBase {
 public:
  typedef typename detail::YieldType<decltype(
      std::declval<ProducerFunc &>()(std::declval<Member &>()))>
      YieldType;
  typedef detail::RmRef<YieldType> NoRefYieldType;

  Generator(Member &&member, ProducerFunc &&producer)
      : member(std::forward<Member>(member)),
        producer(std::forward<ProducerFunc>(producer)) {}

  decltype(auto) operator()() { return producer(member); }

  operator bool() {
    if (!applied) {
      value = apply(std::forward<decltype(*this)>(*this));
      applied = true;
    }
    return value.has_value();
  }

  decltype(auto) operator*() {
    // YieldType &operator*() {
    this->operator bool();
    return std::forward<YieldType>(*value);
    // return *value;
  }

 private:
  Member member;
  ProducerFunc producer;
  OptionOrRef<YieldType> value;
  bool applied = false;
};

template <typename ProducerFunc>
class Generator<void, ProducerFunc> : public detail::GeneratorBase {
 public:
  typedef typename detail::YieldType<decltype(std::declval<ProducerFunc &>()())>
      YieldType;
  typedef detail::RmRef<YieldType> NoRefYieldType;

  Generator(ProducerFunc &&producer)
      : producer(std::forward<ProducerFunc>(producer)) {}

  decltype(auto) operator()() { return producer(); }

  operator bool() {
    if (!applied) {
      value = apply(std::forward<decltype(*this)>(*this));
      applied = true;
    }
    return value.has_value();
  }

  decltype(auto) operator*() {
    // YieldType &operator*() {
    this->operator bool();
    return std::forward<YieldType>(*value);
    // return *value;
  }

 private:
  ProducerFunc producer;
  OptionOrRef<YieldType> value;
  bool applied = false;
};

// template <typename Func>
// auto unpack(Func &&func) {
//   return [funcIn = std::forward<Func>(func)](auto &&tup) {
//     return detail::apply(funcIn, tup);
//   };
// }

// template <typename Gen, detail::EnableIfType<detail::GeneratorBase, Gen> = 0,
//           detail::EnableIfYieldRef<Gen> = 0>
// decltype(auto) apply(Gen &&gen) {
//   typedef typename Gen::YieldType YieldType;
//   OptionOrRef<YieldType> res;
//   while (true) {
//     decltype(auto) val = gen();
//     if (!val) {
//       break;
//     }

//     // if (!res) {
//     res = std::forward<decltype(val)>(val);
//     // }
//   }
//   return res;
// }

// template <typename ProducerFuncIn>
// auto generator(ProducerFuncIn producer) {
//   typedef typename detail::StdRefToRef<ProducerFuncIn>::type ProducerFunc;
//   return Generator<void,
//   ProducerFunc>(std::forward<ProducerFuncIn>(producer));
// }

// template <typename MemberIn, typename ProducerFuncIn>
// auto generator(MemberIn member, ProducerFuncIn producer) {
//   typedef typename detail::StdRefToRef<MemberIn>::type Member;
//   typedef typename detail::StdRefToRef<ProducerFuncIn>::type ProducerFunc;
//   return Generator<Member, ProducerFunc>(
//       std::forward<MemberIn>(member),
//       std::forward<ProducerFuncIn>(producer));
// }

template <typename ProducerFunc>
inline auto generator(ProducerFunc &&producer) {
  return Generator<void, ProducerFunc>(std::forward<ProducerFunc>(producer));
}

template <typename Member, typename ProducerFunc>
inline auto generator(Member &&member, ProducerFunc &&producer) {
  return Generator<Member, ProducerFunc>(std::forward<Member>(member),
                                         std::forward<ProducerFunc>(producer));
}

template <typename In, typename Out,
          detail::EnableIfType<detail::GeneratorBase, In> = 0>
inline auto operator|(In &&in, Out &&out) {
  return generator(std::forward<In>(in), std::forward<Out>(out));
}

template <typename In, typename Out,
          detail::EnableIfNotType<detail::GeneratorBase, In> = 0>
inline auto operator|(In &&in, Out &&out) {
  return generator(generator(std::forward<In>(in)), std::forward<Out>(out));
}

template <typename In, typename Default,
          detail::EnableIfNotType<detail::GeneratorBase, In> = 0>
inline decltype(auto) operator||(In &&in, Default &&dfault) {
  return generator(std::forward<In>(in)) || std::forward<Default>(dfault);
}

template <typename Gen, typename Default,
          detail::EnableIfType<detail::GeneratorBase, Gen> = 0>
inline decltype(auto) operator||(Gen &&gen, Default &&dfault) {
  typedef typename Gen::YieldType YieldType;
  decltype(auto) res = apply(std::forward<Gen>(gen));
  if (!res) {
    return std::forward<Default>(dfault);
  }
  return std::forward<YieldType>(*res);
}

//
// Built-in Components
//

namespace detail {

template <typename Func>
using Gen0YieldType = decltype(std::declval<Func &>()());

template <typename Func, typename Gen>
using Gen1YieldType = decltype(std::declval<Func &>()(
    std::declval<typename detail::RmRef<Gen>::YieldType &>()));

template <typename Func, detail::EnableIfType<detail::OptionOrRefBase,
                                              Gen0YieldType<Func>> = 0>
inline decltype(auto) genApply(Func &&f) {
  return f();
}

template <typename Func, detail::EnableIfNotType<detail::OptionOrRefBase,
                                                 Gen0YieldType<Func>> = 0>
inline OptionOrRef<Gen0YieldType<Func>> genApply(Func &&f) {
  return f();
}

template <
    typename Func, typename Gen,
    detail::EnableIfType<detail::OptionOrRefBase, Gen1YieldType<Func, Gen>> = 0,
    detail::EnableIfType<detail::GeneratorBase, Gen> = 0>
inline decltype(auto) genApply(Func &&f, Gen &&g) {
  decltype(auto) val = g();
  if (!val) {
    return nullopt;
  }
  return f(std::forward<decltype(*val)>(*val));
}

template <typename Func, typename Gen,
          detail::EnableIfNotType<detail::OptionOrRefBase,
                                  Gen1YieldType<Func, Gen>> = 0,
          detail::EnableIfType<detail::GeneratorBase, Gen> = 0>
inline OptionOrRef<Gen1YieldType<Func, Gen>> genApply(Func &&f, Gen &&g) {
  decltype(auto) val = g();
  if (!val) {
    return nullopt;
  }
  return f(std::forward<decltype(*val)>(*val));
}

}  // namespace detail

template <typename Func>
inline decltype(auto) gen(Func &&f) {
  return
      [f1 = std::forward<Func>(f)](auto &&... param) mutable -> decltype(auto) {
        return detail::genApply(std::forward<Func>(f1),
                                std::forward<decltype(param)>(param)...);
      };
}

template <typename R, class... A>
inline decltype(auto) gen(R (*f)(A...)) {
  return [f](auto &&... param) mutable -> decltype(auto) {
    return detail::genApply(f, std::forward<decltype(param)>(param)...);
  };
}

inline decltype(auto) limit(std::size_t n) {
  return [count = 0, n](auto &&gen) mutable {
    typedef typename detail::RmRef<decltype(gen)>::YieldType YieldType;
    return count++ < n ? gen() : OptionOrRef<YieldType>(nullopt);
  };
}

}  // namespace lz
