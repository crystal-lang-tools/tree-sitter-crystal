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
