const $claraType = Symbol('$claraType')

interface $ClaraObject {
    [$claraType]: Symbol
}

interface Foo extends $ClaraObject {
  x: number
  y: number
}

const Foo$Type = Symbol('Foo')

const Foo$Methods = {
  area: (a: Foo) => a.x * a.y
}

const foo1: Foo = {
  [$claraType]: Foo$Type,
  x: 1,
  y: 2,
}
