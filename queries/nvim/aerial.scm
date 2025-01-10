(module_def
  name: (constant) @name
  (#set! "kind" "Module")) @symbol

(class_def
  name: (constant) @name
  (#set! "kind" "Class")) @symbol

(struct_def
  name: (constant) @name
  (#set! "kind" "Struct")) @symbol

(enum_def
  name: (constant) @name
  (#set! "kind" "Enum")) @symbol

(method_def
  name: (identifier) @name
  (#set! "kind" "Method")) @symbol

(const_assign
  lhs: (constant) @name
  (#set! "kind" "Constant")) @symbol
