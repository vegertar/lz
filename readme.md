# LZ (Lazy Code)

```C++
auto helloworld() { return "hello world\n"; }
auto show = lz::gen(printf);

int main() { return (helloworld | show); }
```

 Heavily inspired by [SaadAttieh/lazyCode](https://github.com/SaadAttieh/lazyCode), the idea was awesome, but original interfaces taste awful, I mean, which were [really hard](https://github.com/SaadAttieh/lazyCode/blob/10b0a323d836230a536d305da1709737da9d186b/include/lazyCode/basicCollectors.h#L122) to customize a new Generator. So I've pruned almost all codes (actuall, only CMakeLists.txts are remained :) ), and implement these concepts:
 
 - **Component**: similar to path component, so do functions between | , but I call the entry(first) component a *Generator*, others are *Middleware*.
 - **No algorithm/container**: loads of excellent librarys, e.g. fplus, have existed years, so I just wanna implement an intuitive way to use present functions.
 - **Infinity**: the default execution of pipeline will run forever until one component causes an `nullopt`. *Yes, the example above is an exceptional case runs only once.*
 - <s>**Branch and Backend**: && and || for branch, & for backend/async, </s>these operators might be cool, but too complicated to write right now.

If you please and interested, see [test/components.cc](https://github.com/vegertar/lz/blob/master/test/components.cc) for more details.

## Others

Only tested on Archlinux and Ubuntu 20.04, for clang 10+ and gcc 9+ with std=c++14.

There is only one dependent library, [optional-lite](https://github.com/martinmoene/optional-lite), which provides `nonstd::optional` that were first bringed in C++17. 

Please run `git submodule update --init` to fetch dependencies.