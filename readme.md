# LZ (Lazy Code)

```C++
auto helloworld() { return "hello world\n"; }
auto show = lz::gen(printf);

int main() { return (helloworld | show); }
```

Heavily inspired by [SaadAttieh/lazyCode](https://github.com/SaadAttieh/lazyCode), the idea was awesome, but original interfaces taste awful, I mean, which were [really hard](https://github.com/SaadAttieh/lazyCode/blob/10b0a323d836230a536d305da1709737da9d186b/include/lazyCode/basicCollectors.h#L122) to customize a new Generator. So I've pruned almost all codes (actuall, only CMakeLists.txts are remained :) ), and implement these concepts:

- **Component**: similar to path component, so do functions between | , but I call the entry(first) component a _Generator_, others are _Middleware_.
- **No algorithm/container**: loads of excellent librarys, e.g. fplus, have existed years, so I just wanna implement an intuitive way to use present functions.
- **Infinity**: the default execution of pipeline will run forever until one component causes an `nullopt`. _Yes, the example above is an exceptional case runs only once._
- **Branch** <s>, **Parallel, and Backend**</s>: && and || for branch,<s>/ for parallel, & for backend/async, </s>these operators might be cool, but it seems like a little bit of complicated.

If you please and interested, see [test/components.cc](https://github.com/vegertar/lz/blob/master/test/components.cc) for more details.

## Build

Only tested on Archlinux and Ubuntu 20.04, for clang 10+ and gcc 9+ with std=c++14.

Although `lz` is a single-header library, there is still a dependent library, [optional-lite](https://github.com/martinmoene/optional-lite), which provides `nonstd::optional` that were first brought in C++17.

And if you wanna run tests, `Catch2` is required as well.

Please run `git submodule update --init` to fetch dependencies. Then build via ordinary CMake way:

```shell
mkdir build && cd build && cmake .. && make -j 8
```

## How it works

Intuitively, for arbitrary sequence of functions, `f1, f2, ..., fn`, and the corresponding calling chain `fn(...f2(f1()))`, the simplest way to write the expression from left to right, is what define a macro, the C-style magic, e.g. the 3 parameters version:

```
#define X(a1, a2, a3) a3(a2(a1()))
```

But we're playing **modern** C++++++...

Binary `Operator Overloading` cannot directly play with both literal function pointers, so `lz` accept lambda with such signature, I call it `lz-frendly` function:

- necessarily, only one parameter of type `lz::Generator` in lvalue reference
- optionally, returning an Optional value, i.e. `lz::Optional<T>`

If rewrite the `show` (global variable appeared in example), a complete lz-friendly function could be

```C++
auto show2 = ([](auto &&gen) -> lz::Optional<int> {
               auto x = lz::get(gen);
               if (!x) {
                 return {};
               }
               return printf(">>> %s\n", *x);
             }) |
             lz::limit(2);

int main() {
  auto g = lz::gen(helloworld) | show2;
  if (g) {
    printf("%d\n", *g);
  }
}

```

The results were

```shell
>>> hello world

>>> hello world

17
```

The parameter `gen` is an instance of `lz::Generator`, indicates the previous function in calling chain. Now we just need to know that there is a helper function `lz::get` could retrieve the function result, an optional wrapper similar to `std::optional<T>`, in this case, is `lz::Optional<const char *>{"hello world\n"}`. So before using `*x` to get the actual value, it's generally safety to check if the optional data `x` is available.

And the tailing `lz::limit(2)` controlling the loop runs 2 times.

Finally, the expression `lz::gen(helloworld)` converts an ordinary function `helloworld` to lz-friendly lambda.

I've simplified the whole processes, but the kernel idea is as simple as I said.

Template magic did them all.

## Interface

There are only 3 APIs in `lz` header file:

- **lz::get(gen)**: retrieve the previous function result in an Optional wrapper.
- **lz::limit(n)**: limits the loop running at most `n` times.
- **lz::gen(func)**: converts ordinary functions to lz-friendly lambda, supported function signatures are:

  - **() -> T**: entry component has no parameter
  - **(T) -> U**: middleware accepts the parameter of previous function result

  > _ordinary lambdas in above signatures are accepted as well_

An entry component started pipeline expression returns a `lz::Generator`, e.g. the result of `helloworld | show2`, since `lz::Generator` implements both `std::begin` and `std::end`, so range-based for loop is also compatible:

```C++
for (auto n : helloworld | show2) {
  printf("got %d\n", n);
}


>>> hello world

got 17
>>> hello world

got 17
```

Surely STL iterator_traits is supported as well.

## Licence

MIT
