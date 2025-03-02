{1, 2, 3}
# <- punctuation.bracket
#       ^ punctuation.bracket
#^  ^  ^ number
# ^  ^ punctuation.delimiter

{:t}
# <- punctuation.bracket
#  ^ punctuation.bracket
#^^ string.special.symbol

{ :t }
# <- punctuation.bracket
#    ^ punctuation.bracket

{a: :a, b: "b"}
# <-            punctuation.bracket
#             ^ punctuation.bracket
# ^   ^  ^      punctuation.delimiter
#^      ^       property

def foo(*args, **kwargs);end
#       ^      ^^             operator

foo 1, 2 .. { 3 }
#           ^   ^ punctuation.bracket

foo a: 1, b: 2, c: 3
#   ^     ^     ^     property
#    ^     ^     ^    punctuation.delimiter
