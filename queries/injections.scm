((comment) @injection.content
  (#set! injection.language "comment"))

(heredoc_body
  (literal_content) @injection.content
  (heredoc_end) @injection.language
  (#downcase! @injection.language))
