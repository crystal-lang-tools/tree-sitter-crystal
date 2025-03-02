def foo(bar : String, *a : Bool, **nums : Int32, &blk : ->)
#   ^^^                                                     function.method
#       ^^^            ^           ^^^^           ^^^       variable.parameter
rescue ex : StandardError
#      ^^ variable.parameter
end

def foo(bar, *a, **nums, &blk)
#   ^^^                         function.method
#       ^^^   ^    ^^^^   ^^^   variable.parameter
rescue ex
#      ^^ variable.parameter
end

abstract def foo()
#            ^^^    function.method

macro foo(args)
#     ^^^       function.method
#         ^^^^  variable.parameter
end

fun foo = __foo(a : Int32)
#   ^^^   ^^^^^             function
#               ^           variable.parameter
end
