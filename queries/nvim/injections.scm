((comment) @injection.content
  (#set! injection.language "comment"))

(heredoc_body
  (heredoc_content) @injection.content
  (heredoc_end) @injection.language
  (#downcase! @injection.language))

(macro_def
  body: (expressions) @injection.content
  (#set! injection.language "crystal")
  (#set! injection.include-children))

(macro_begin
  body: (expressions) @injection.content
  (#set! injection.language "crystal")
  (#set! injection.include-children))
