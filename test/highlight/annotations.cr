@[Deprecated("Use `#global?` instead")]
# <-                                    punctuation.bracket
#^          ^                        ^^ punctuation.bracket

@[Primitive(:interpreter_libm_ceil_f32)]
# <-                                      punctuation.bracket
#^         ^                          ^^  punctuation.bracket

@[MyAnnotation(key: "value", value: 123)]
# <-                                      punctuation.bracket
#^            ^                        ^^ punctuation.bracket
#              ^^^           ^^^^^        property
#                 ^        ^      ^       punctuation.delimiter
