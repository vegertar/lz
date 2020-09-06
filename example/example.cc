#include <lz/lz.h>
#include <stdio.h>

auto helloworld() { return "hello world\n"; }
auto show = lz::gen(printf);

int main() { return (helloworld | show); }