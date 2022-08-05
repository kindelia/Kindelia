
/*
== Client ==

kindelia test file.kdl

kindelia serialize code.kdl > code.hex.txt

kindelia deserialize stmt.hex.txt
kindelia deserialize <<< a67bd36d75da

kindelia [--pvt-file] ?
kindelia [--pvt-pass] ?

kindelia sign stmt.hex.txt
kindelia sign <<< a67bd36d75da

kindelia post stmt.hex.txt
kindelia sign <<< a67bd36d75da

kindelia completion zsh >> .zshrc

== Remote ==

kindelia get fn Count code
kindelia get fn Count state
kindelia get fn Count slots

kindelia get ns Foo.Bar owner
kindelia get ns Foo.Bar list

kindelia get bk 0xc7da4b76b4d7a64b7 | kindelia deserialize
kindelia get bk 751
kindelia get bk 2756

kindelia get ct Pair code
kindelia get ct Pair arity

kindelia get tick
kindelia get mana
kindelia get space

kindelia get fn-count
kindelia get ns-count
kindelia get ct-count

kindelia run  [--host ""] code.hex.txt
kindelia post [--host ""] code.hex.txt

== Node ==

kindelia node start --mine --local --log-events --nice-ui?
kindelia node clean [-f]       // asks confirmation

*/
