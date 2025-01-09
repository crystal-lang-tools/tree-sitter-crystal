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

def foo(*args);end

foo 1, 2 .. { 3 }
#           ^   ^ punctuation.bracket
