a : {Int, Int} = {1, 2}
#   ^        ^ punctuation.bracket
#       ^ punctuation.delimiter
#    ^^^  ^^^ type

a : { Char } = { 'c' }
#   ^      ^   ^     ^ punctuation.bracket

def foo : ->{ 'a'=>'b' }; ->{ nil } end
#           ^          ^    ^     ^punctuation.bracket

def foo : ->{Int32}; ->{ {5,} } end
#           ^     ^    ^ ^  ^ ^punctuation.bracket
def foo_spacing : -> { Int32 }; ->{ {5,} } end
#                    ^       ^    ^ ^  ^ ^ punctuation.bracket

f = -> : -> { a: String; ->{} }
#           ^              ^^ ^ punctuation.bracket

foo : A | B | {c: C}
#              ^      property
