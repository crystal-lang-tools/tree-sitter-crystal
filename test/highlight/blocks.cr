@annotations.try &.values.flatten
#            ^^^                  function.call
#                ^                operator

option_parser.invalid_option(1, 2, &old_invalid_option)
#             ^^^^^^^^^^^^^^                            function.call
#                                  ^                    operator

foo(&->a)
#<-       function.call
#^^       function.call
#   ^^^   operator

foo do |asdf|
#   ^^        keyword
#      ^    ^ punctuation.bracket
#       ^^^^  variable.parameter
  qux
end

foo { |asdf| qux }
#   ^ ^    ^     ^  punctuation.bracket
#      ^^^^         variable.parameter

 ->(a) { qux }
#^^             operator
#  ^ ^ ^     ^  punctuation.bracket
#   ^           variable.parameter
