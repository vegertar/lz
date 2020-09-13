#include <lz/lz.h>
#include <stdio.h>

auto helloworld() { return "hello world\n"; }
auto show = lz::gen(printf);

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

  for (auto n : helloworld | show2) {
    printf("got %d\n", n);
  }

  return (helloworld | show);
}