//
# <- punctuation.bracket
#^ punctuation.bracket

/asdf/im
# <- punctuation.bracket
#^^^^ string.regexp
#    ^ punctuation.bracket
#     ^^ character.special

/#{1}(\a)\basdfasdf\d\#{2}\1/
# <- punctuation.bracket
#                           ^ punctuation.bracket
#^^ ^ punctuation.special
#  ^ number
#    ^^^^^^^^^^^^^^^^^^^^^^^ string.regexp

 %r()
#^^^^ punctuation.bracket
  %r| \| |im
# ^^^    ^ punctuation.bracket
#    ^^^^ string.regexp
#         ^^ character.special

 %r<#{1}a\#{2})>
#^^^           ^punctuation.bracket
#   ^^ ^ punctuation.special
#     ^ number
#       ^^^^^^ string.regexp
