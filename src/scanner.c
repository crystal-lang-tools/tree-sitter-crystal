#include "tree_sitter/alloc.h"
#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wctype.h>

// Functions for converting between UTF-8 and UTF-32
#include "unicode.c"

/*
 * Token types
 */

enum Token {
    LINE_BREAK,
    LINE_CONTINUATION,

    START_OF_BRACE_BLOCK,
    START_OF_HASH_OR_TUPLE,
    START_OF_NAMED_TUPLE,
    START_OF_TUPLE_TYPE,
    START_OF_NAMED_TUPLE_TYPE,

    START_OF_INDEX_OPERATOR,

    END_OF_WITH_EXPRESSSION,

    UNARY_PLUS,
    UNARY_MINUS,
    BINARY_PLUS,
    BINARY_MINUS,

    UNARY_WRAPPING_PLUS,
    UNARY_WRAPPING_MINUS,
    BINARY_WRAPPING_PLUS,
    BINARY_WRAPPING_MINUS,

    POINTER_STAR,
    UNARY_STAR,
    BINARY_STAR,

    UNARY_DOUBLE_STAR,
    BINARY_DOUBLE_STAR,

    BLOCK_AMPERSAND,
    BINARY_AMPERSAND,

    BEGINLESS_RANGE_OPERATOR,

    REGEX_START,
    BINARY_SLASH,
    BINARY_DOUBLE_SLASH,

    REGULAR_IF_KEYWORD,
    MODIFIER_IF_KEYWORD,

    REGULAR_UNLESS_KEYWORD,
    MODIFIER_UNLESS_KEYWORD,

    REGULAR_RESCUE_KEYWORD,
    MODIFIER_RESCUE_KEYWORD,

    REGULAR_ENSURE_KEYWORD,
    MODIFIER_ENSURE_KEYWORD,

    MODULO_OPERATOR,

    START_OF_SYMBOL,
    UNQUOTED_SYMBOL_CONTENT,

    ABSTRACT_KEYWORD,
    ALIAS_KEYWORD,
    ALIGNOF_KEYWORD,
    ANNOTATION_KEYWORD,
    ASM_KEYWORD,
    BEGIN_KEYWORD,
    BREAK_KEYWORD,
    CASE_KEYWORD,
    CLASS_KEYWORD,
    DEF_KEYWORD,
    DO_KEYWORD,
    ELSE_KEYWORD,
    ELSIF_KEYWORD,
    END_KEYWORD,
    ENUM_KEYWORD,
    EXTEND_KEYWORD,
    FALSE_KEYWORD,
    FUN_KEYWORD,
    IN_KEYWORD,
    INCLUDE_KEYWORD,
    INSTANCE_ALIGNOF_KEYWORD,
    INSTANCE_SIZEOF_KEYWORD,
    LIB_KEYWORD,
    MACRO_KEYWORD,
    MODULE_KEYWORD,
    NEXT_KEYWORD,
    NIL_KEYWORD,
    OFFSETOF_KEYWORD,
    OUT_KEYWORD,
    POINTEROF_KEYWORD,
    PRIVATE_KEYWORD,
    PROTECTED_KEYWORD,
    REQUIRE_KEYWORD,
    RETURN_KEYWORD,
    SELECT_KEYWORD,
    SIZEOF_KEYWORD,
    STRUCT_KEYWORD,
    TRUE_KEYWORD,
    TYPE_KEYWORD,
    TYPEOF_KEYWORD,
    UNION_KEYWORD,
    UNTIL_KEYWORD,
    WHEN_KEYWORD,
    WHILE_KEYWORD,
    WITH_KEYWORD,
    YIELD_KEYWORD,

    TYPE_FIELD_COLON,

    STRING_LITERAL_START,
    DELIMITED_STRING_CONTENTS,
    STRING_LITERAL_END,

    COMMAND_LITERAL_START,
    COMMAND_LITERAL_END,

    STRING_PERCENT_LITERAL_START,
    COMMAND_PERCENT_LITERAL_START,
    STRING_ARRAY_PERCENT_LITERAL_START,
    SYMBOL_ARRAY_PERCENT_LITERAL_START,
    REGEX_PERCENT_LITERAL_START,
    PERCENT_LITERAL_END,

    DELIMITED_ARRAY_ELEMENT_START,
    DELIMITED_ARRAY_ELEMENT_END,

    HEREDOC_START,
    HEREDOC_BODY_START,
    HEREDOC_CONTENT,
    HEREDOC_END,

    REGEX_MODIFIER,

    MACRO_START,
    MACRO_DELIMITER_END,
    MACRO_DELIMITER_ELSE,
    MACRO_DELIMITER_ELSIF,
    MACRO_CONTENT,
    MACRO_CONTENT_NESTING,

    // Never returned
    START_OF_PARENLESS_ARGS,
    END_OF_RANGE,
    START_OF_MACRO_VAR_EXPS,

    // Only used when error recovery mode is active
    ERROR_RECOVERY,

    NONE,
};
typedef enum Token Token;

/*
 * Helpful macros
 */

// NOTE(margret): This is temporarily disabled as calling lexer->log
// causes a seg fault with WASM builds. Enable locally when needed.
#define DEBUG(...) // lexer->log(lexer, "[LOG] " __VA_ARGS__);

/*
 * State types
 */

enum LiteralTypeEnum {
    STRING,
    STRING_NO_ESCAPE,
    COMMAND,
    STRING_ARRAY,
    SYMBOL_ARRAY,
    REGEX,
};
typedef uint8_t LiteralType;

struct PercentLiteral {
    // We compare these chars with int32_t codepoints, but all valid delimiters
    // in Crystal are in the ASCII range, so we won't overflow.
    uint8_t opening_char;
    uint8_t closing_char;
    uint8_t nesting_level;
    LiteralType type;
};
typedef struct PercentLiteral PercentLiteral;

struct Heredoc {
    bool allow_escapes;
    bool started;
    // Heredoc identifier encoded with UTF-8
    Array(uint8_t) identifier;
};
typedef struct Heredoc Heredoc;

struct MacroState {
    // Set to true if the macro scan is currently in a comment (the rest of the line after #).
    // Defaults to false.
    bool in_comment;
    // Tracks if the regular (not modifier) versions of `if` and `unless` may
    // be scanned in the macro text. (Also affects `while` and `until`, because
    // they used to be modifier keywords too.)
    bool non_modifier_keyword_can_begin;

    // TODO: heredocs?
};
typedef struct MacroState MacroState;

#define MAX_LITERAL_COUNT 16
#define MAX_HEREDOC_COUNT 16
// The maximum number of bytes that can be stored in the state, across all heredocs
#define HEREDOC_BUFFER_SIZE 512
// We only use a single byte to store the size
#define MAX_HEREDOC_WORD_SIZE 255

struct State {
    bool has_leading_whitespace;
    bool previous_line_continued;

    MacroState macro_state;

    // It's possible to have nested delimited literals, like
    //   %(#{%(foo)})
    // We can handle up to MAX_LITERAL_COUNT levels of nesting.
    Array(PercentLiteral) literals;

    Array(Heredoc) heredocs;
};
typedef struct State State;

/*
 * State-related macros and functions
 */

#define HAS_ACTIVE_LITERAL(state) \
    (state->literals.size > 0)

#define ACTIVE_LITERAL(state) \
    array_back(&state->literals)

#define PUSH_LITERAL(state, literal) \
    array_push(&state->literals, literal)

#define POP_LITERAL(state) \
    array_pop(&state->literals)

// Return true if any heredoc is started
static bool has_active_heredoc(State *state) {
    for (uint8_t i = 0; i < state->heredocs.size; i++) {
        if (array_get(&state->heredocs, i)->started) {
            return true;
        }
    }
    return false;
}

// Return true if the first heredoc on the queue is unstarted
static bool has_unstarted_heredoc(State *state) {
    if (state->heredocs.size == 0) {
        return false;
    }

    return !array_front(&state->heredocs)->started;
}

// Return the number of bytes currently stored across all heredocs
static size_t heredoc_current_buffer_size(State *state) {
    size_t bytes = 0;
    for (uint8_t i = 0; i < state->heredocs.size; i++) {
        bytes += array_get(&state->heredocs, i)->identifier.size;
    }
    return bytes;
}

// Pop the active heredoc off the queue
static void pop_heredoc(State *state) {
    assert(state->heredocs.size > 0);

    Heredoc *popped = array_front(&state->heredocs);
    assert(popped->started);

    array_delete(&popped->identifier);
    array_erase(&state->heredocs, 0);
}

// Return true if state has room to track another heredoc
static bool has_room_for_heredoc(State *state, Heredoc heredoc) {
    if (state->heredocs.size >= MAX_HEREDOC_COUNT) {
        return false;
    }

    size_t current_bytes = heredoc_current_buffer_size(state);
    return (current_bytes + heredoc.identifier.size) <= HEREDOC_BUFFER_SIZE;
}

// Push a heredoc onto the end of the state queue. If there's already an active
// heredoc, the new heredoc must be nested, so it is added to the queue before
// the active heredoc.
static void push_heredoc(State *state, Heredoc heredoc) {
    assert(state->heredocs.size < MAX_HEREDOC_COUNT);

    if (has_active_heredoc(state)) {
        // This must be a nested heredoc, so insert it before the currently-active heredoc
        size_t index;
        for (index = 0; index < state->heredocs.size; index++) {
            if (array_get(&state->heredocs, index)->started) {
                break;
            }
        }
        assert(index < state->heredocs.size);

        array_insert(&state->heredocs, index, heredoc);
    } else {
        array_push(&state->heredocs, heredoc);
    }
}

enum LookaheadResult {
    LOOKAHEAD_UNKNOWN = 0,
    LOOKAHEAD_TYPE,
    LOOKAHEAD_NAMED_TUPLE,
};
typedef enum LookaheadResult LookaheadResult;

enum ScanResult {
    // Keep scanning to match a different external token
    SR_CONTINUE,
    // Stop scanning and return content
    SR_STOP,
    // Stop scanning and don't return content (we expect non-external tokens to match)
    SR_STOP_NO_CONTENT,
};
typedef enum ScanResult ScanResult;

// Reset the macro state to its defaults
static void reset_macro_state(State *state) {
    state->macro_state.in_comment = false;
    state->macro_state.non_modifier_keyword_can_begin = true;
}

// Skip one character, which will not be included in the token emitted by the scanner.
// WARNING: this will set the _start_ of the token range. Don't use this after mark_end!
static void lex_skip(State *state, TSLexer *lexer) {
    state->has_leading_whitespace = true;
    lexer->advance(lexer, true);
}

// NOTE: apparently this can't be called `advance` because it conflicts with a
// symbol in glibc
static void lex_advance(TSLexer *lexer) {
    lexer->advance(lexer, false);
}

static bool next_char_is_identifier(TSLexer *lexer) {
    int32_t lookahead = lexer->lookahead;

    return iswalnum(lookahead)
        || lookahead == '_'
        || lookahead == '?'
        || lookahead == '!'
        || lookahead >= 0xa0;
}

static bool is_ident_part(int32_t codepoint) {
    // identifier token characters are in the range [0-9A-Za-z_\u{00a0}-\u{10ffff}]
    // (except for the first and last character)
    return ('0' <= codepoint && codepoint <= '9')
        || ('A' <= codepoint && codepoint <= 'Z')
        || ('a' <= codepoint && codepoint <= 'z')
        || (codepoint == '_')
        || (0x00a0 <= codepoint && codepoint <= 0x10ffffff);
}

static bool next_is_colon_space(TSLexer *lexer) {
    lexer->mark_end(lexer);
    while (iswspace(lexer->lookahead)) {
        lex_advance(lexer);
    }
    if (lexer->lookahead != ':') {
        return false;
    }
    lex_advance(lexer);
    return iswspace(lexer->lookahead);
}

// Usually scan_whitespace will handle starting heredocs, but it won't be called if a heredoc is
// already active. This function is called before heredoc contents or whitespace is scanned, so it
// can handle the start of a nested heredoc.
static bool check_for_heredoc_start(State *state, TSLexer *lexer, const bool *valid_symbols) {
    // Note: calling get_column(lexer) at EOF seems to cause loops, so make sure EOF is checked first
    if (valid_symbols[HEREDOC_BODY_START]
        && has_unstarted_heredoc(state)
        && !state->previous_line_continued
        && !lexer->eof(lexer)
        && lexer->get_column(lexer) == 0) {

        assert(state->heredocs.size > 0);
        assert(!array_front(&state->heredocs)->started);

        array_front(&state->heredocs)->started = true;

        lexer->result_symbol = HEREDOC_BODY_START;
        return true;
    }
    return false;
}

static bool scan_whitespace(State *state, TSLexer *lexer, const bool *valid_symbols) {
    bool crossed_newline = false;

    for (;;) {
        switch (lexer->lookahead) {
            case ' ':
            case '\t':
            case '\r':
                lex_skip(state, lexer);
                break;

            case '\n':
                if (valid_symbols[HEREDOC_BODY_START] && has_unstarted_heredoc(state)) {
                    assert(state->heredocs.size > 0);
                    Heredoc *heredoc = array_front(&state->heredocs);
                    assert(!heredoc->started);

                    heredoc->started = true;
                    // HEREDOC_BODY_START is a zero-width token. Use skip instead
                    // of advance because we don't want to include the newline.
                    lex_skip(state, lexer);
                    lexer->result_symbol = HEREDOC_BODY_START;
                    return true;
                } else if (valid_symbols[LINE_BREAK] && !crossed_newline) {
                    lex_advance(lexer);
                    lexer->mark_end(lexer);
                    crossed_newline = true;
                    state->has_leading_whitespace = true;
                } else {
                    lex_skip(state, lexer);
                }
                break;

            case '\v':
            case '\f':
                // In regular code, these characters are not allowed. But they
                // may be used in between strings in a %w array.
                if (HAS_ACTIVE_LITERAL(state)) {
                    lex_skip(state, lexer);
                    break;
                }
                return false;

            default:
                if (crossed_newline) {
                    if (lexer->lookahead == '.') {
                        // Check if this is the continuation of a method call,
                        // or the start of a beginless range literal.
                        lex_advance(lexer);
                        if (lexer->lookahead == '.') {
                            lexer->result_symbol = LINE_BREAK;
                        }
                    } else if (lexer->lookahead == '#') {
                        // Comments don't interrupt line continuations
                    } else if (lexer->lookahead == ':' && valid_symbols[TYPE_FIELD_COLON]) {
                        // Check for a type field separator that comes after a newline, e.g.
                        //   ( params )
                        //   : ReturnType
                        lex_advance(lexer);
                        if (iswspace(lexer->lookahead)) {
                            lex_advance(lexer);
                            lexer->mark_end(lexer);
                            lexer->result_symbol = TYPE_FIELD_COLON;
                        } else {
                            lexer->result_symbol = LINE_BREAK;
                        }
                    } else {
                        lexer->result_symbol = LINE_BREAK;
                    }
                }
                return true;
        }
    }
}

// Returns true if a string content token is found
static ScanResult scan_string_contents(State *state, TSLexer *lexer, const bool *valid_symbols) {
    bool found_content = false;
    LiteralType active_type;

    // This may be overridden later.
    lexer->result_symbol = DELIMITED_STRING_CONTENTS;

    for (;;) {
        if (lexer->eof(lexer)) {
            DEBUG("reached EOF");
            if (found_content) {
                return SR_STOP;
            } else {
                return SR_STOP_NO_CONTENT;
            }
        }

        active_type = ACTIVE_LITERAL(state)->type;

        switch (lexer->lookahead) {
            case '\\':
                switch (active_type) {
                    case STRING:
                    case COMMAND:
                        if (found_content) {
                            return SR_STOP;
                        } else {
                            // do the regular check for LINE_CONTINUATION
                            return SR_CONTINUE;
                        }
                    case REGEX:
                        // No special regex escapes
                        lex_advance(lexer);
                        break;
                    case STRING_NO_ESCAPE:
                        break;
                    case STRING_ARRAY:
                    case SYMBOL_ARRAY:
                        // %w and %i allow only '\<whitespace>' or the closing
                        // delimiter as an escape sequence, so we have to look
                        // ahead one character.
                        lexer->mark_end(lexer);
                        lex_advance(lexer);
                        if (iswspace(lexer->lookahead) || ACTIVE_LITERAL(state)->closing_char == lexer->lookahead) {
                            if (found_content) {
                                return SR_STOP;
                            } else {
                                return SR_STOP_NO_CONTENT;
                            }
                        }

                        // The backslash must be part of the word contents.
                        found_content = true;
                        lexer->mark_end(lexer);
                        continue;
                }
                break;
            case '#':
                if (active_type == STRING_NO_ESCAPE || active_type == STRING_ARRAY || active_type == SYMBOL_ARRAY) {
                    // These types don't allow interpolation
                    break;
                }

                lexer->mark_end(lexer);
                lex_advance(lexer);
                if (lexer->lookahead == '{') {
                    if (found_content) {
                        return SR_STOP;
                    } else {
                        return SR_STOP_NO_CONTENT;
                    }
                }

                found_content = true;
                lexer->mark_end(lexer);
                continue;
            case ' ':
            case '\t':
            case '\n':
            case '\r':
            case '\v':
            case '\f':
                if (active_type == STRING_ARRAY || active_type == SYMBOL_ARRAY) {
                    assert(found_content || valid_symbols[DELIMITED_ARRAY_ELEMENT_END]);

                    if (found_content) {
                        // We've already found string contents, return that.
                        return SR_STOP;
                    } else if (valid_symbols[DELIMITED_ARRAY_ELEMENT_END]) {
                        // We've reached the end of an array word.
                        lexer->result_symbol = DELIMITED_ARRAY_ELEMENT_END;
                        return SR_STOP;
                    }
                }
                break;
            case '"':
            case '|':
            case '`':
                // These delimiters can't nest
                if (ACTIVE_LITERAL(state)->closing_char == lexer->lookahead) {
                    if (found_content) {
                        // We've already found string contents, return that.
                        return SR_STOP;
                    } else if (valid_symbols[DELIMITED_ARRAY_ELEMENT_END]) {
                        // We've reached the end of an array word.
                        lexer->result_symbol = DELIMITED_ARRAY_ELEMENT_END;
                        return SR_STOP;
                    } else {
                        // The check for PERCENT_LITERAL_END comes later.
                        return SR_CONTINUE;
                    }
                }
                break;
            case '(':
            case '[':
            case '{':
            case '<':
                if (ACTIVE_LITERAL(state)->opening_char == lexer->lookahead) {
                    ACTIVE_LITERAL(state)->nesting_level++;
                }
                break;
            case ')':
            case ']':
            case '}':
            case '>':
                if (ACTIVE_LITERAL(state)->closing_char == lexer->lookahead) {
                    if (ACTIVE_LITERAL(state)->nesting_level == 0) {
                        if (found_content) {
                            // We've already found string contents, return that.
                            return SR_STOP;
                        } else if (valid_symbols[DELIMITED_ARRAY_ELEMENT_END]) {
                            // We've reached the end of an array word.
                            lexer->result_symbol = DELIMITED_ARRAY_ELEMENT_END;
                            return SR_STOP;
                        } else {
                            // The check for PERCENT_LITERAL_END comes later.
                            return SR_CONTINUE;
                        }
                    }

                    ACTIVE_LITERAL(state)->nesting_level--;
                }
                break;
        }

        lex_advance(lexer);
        lexer->mark_end(lexer);
        found_content = true;
    }
}

// Scan for heredoc contents, and return true if the body or end tag of the
// current heredoc is matched. This function will scan across multiple lines,
// so one heredoc_content node will contain as many characters as possible.
static bool scan_heredoc_contents(State *state, TSLexer *lexer, const bool *valid_symbols) {
    if (valid_symbols[ERROR_RECOVERY] && !has_active_heredoc(state)) {
        return false;
    }

    assert(state->heredocs.size > 0);
    assert(has_active_heredoc(state));

    bool found_content = false;

    Heredoc *active_heredoc = array_front(&state->heredocs);
    bool heredoc_pending_start;

    if (active_heredoc->started) {
        heredoc_pending_start = false;
    } else {
        // The first heredoc in the queue isn't started, which means it's a
        // pending nested heredoc that will begin on the next line.
        heredoc_pending_start = true;
        for (uint8_t i = 1; i < state->heredocs.size; i++) {
            if (array_get(&state->heredocs, i)->started) {
                active_heredoc = array_get(&state->heredocs, i);
                break;
            }
        }
    }

    for (;;) {
    start_of_line:
        if (found_content && heredoc_pending_start) {
            // We matched the remaining heredoc_content on a previous line after
            // the start of a nested heredoc. Now we return that content, and
            // the next call to the scanner will trigger check_for_heredoc_start.
            return true;
        }

        if (valid_symbols[HEREDOC_END] && !lexer->eof(lexer) && lexer->get_column(lexer) == 0) {
            if (found_content) {
                lexer->mark_end(lexer);
                while (lexer->lookahead == '\t' || lexer->lookahead == ' ') {
                    lex_advance(lexer);
                }

            } else {
                while (lexer->lookahead == '\t' || lexer->lookahead == ' ') {
                    lex_skip(state, lexer);
                }
                lexer->mark_end(lexer);
            }

            size_t byte_size = active_heredoc->identifier.size;
            size_t matched_codepoint_count;

            // Load all the expected codepoints at once. Theoretically this is less efficient than
            // checking one codepoint at a time, but the actual performance impact is minimal.
            int32_t codepoints[MAX_HEREDOC_WORD_SIZE];
            size_t codepoint_count = utf8_to_codepoints(codepoints, active_heredoc->identifier.contents, byte_size);

            for (matched_codepoint_count = 0; matched_codepoint_count < codepoint_count; matched_codepoint_count++) {
                int32_t expected_codepoint = codepoints[matched_codepoint_count];

                if (lexer->lookahead == expected_codepoint) {
                    lex_advance(lexer);
                } else {
                    break;
                }
            }

            bool end_of_line = (lexer->lookahead == '\n' || lexer->lookahead == '\r' || lexer->eof(lexer));

            if ((matched_codepoint_count == codepoint_count) && end_of_line) {
                if (found_content) {
                    // We already scanned content on a previous line, which must be
                    // returned. The next call to the scanner will match this heredoc word
                    // again, and return it.
                    return true;
                } else {
                    pop_heredoc(state);

                    lexer->mark_end(lexer);
                    lexer->result_symbol = HEREDOC_END;
                    return true;
                }
            }

            if (matched_codepoint_count > 0) {
                // lex_advance was called at least once while scanning for the heredoc
                // word, make sure those characters are counted as content.
                found_content = true;
                lexer->mark_end(lexer);
            }
        }

        // We found either a partial or no match for the heredoc identifier, so scan for string contents
        lexer->result_symbol = HEREDOC_CONTENT;

        for (;;) {
            if (lexer->eof(lexer)) {
                DEBUG("reached EOF");
                return found_content;
            }

            switch (lexer->lookahead) {
                case '\\':
                    if (active_heredoc->allow_escapes) {
                        return found_content;
                    }
                    break;

                case '#':
                    if (!active_heredoc->allow_escapes) {
                        break;
                    }

                    lexer->mark_end(lexer);
                    lex_advance(lexer);

                    if (lexer->lookahead == '{') {
                        return found_content;
                    }

                    found_content = true;
                    lexer->mark_end(lexer);
                    continue;

                case '\r':
                    lex_advance(lexer);
                    lexer->mark_end(lexer);
                    found_content = true;

                    if (lexer->lookahead != '\n') {
                        continue;
                    }
                    // fall through
                case '\n':
                    lex_advance(lexer);
                    lexer->mark_end(lexer);
                    found_content = true;
                    goto start_of_line;
            }

            lex_advance(lexer);
            lexer->mark_end(lexer);
            found_content = true;
        }
    }
}

// Check if a given keyword matches at the current location.
//
// Returns true if the keyword matches exactly.
// Returns false if:
// - a different identifier is matched (shorter or longer)
// - the keyword ends in /[:?!]/
//
// Will consume the entire identifier even if there's only a partial match.
static bool match_macro_keyword(TSLexer *lexer, const char keyword[]) {
    size_t keyword_size = strlen(keyword);
    bool found_match = true;

    for (size_t i = 0; i < keyword_size; i++) {
        if (lexer->lookahead != (int32_t)keyword[i]) {
            found_match = false;
            break;
        }
        lex_advance(lexer);
    }

    if (lexer->lookahead == ':') {
        // Looks like a tuple keyword
        return false;
    }
    if (!found_match || next_char_is_identifier(lexer)) {
        // consume the rest of the identifier, so e.g. `beginbegin` doesn't get split in the middle
        // and then match on the next loop
        while (is_ident_part(lexer->lookahead)) {
            lex_advance(lexer);
        }
        if (next_char_is_identifier(lexer)) {
            lex_advance(lexer);
        }
        return false;
    }

    return true;
}

// Scan for macro literal content, which is treated as text instead of being parsed normally.
// This function is modeled after Crystal::Lexer#next_macro_token. To simplify the implementation
// here, we use grammar rules to model the nesting of different keywords.
//
// When scanning directly inside a `macro` definition, the MACRO_CONTENT_NESTING symbol is valid,
// and nesting keywords are treated as a boundary. When an `end` is reached, the scanner stops
// scanning. We rely on the grammar rules to decide if `end` terminates the macro, or just reduces
// the nesting level.
//
// When scanning inside other macro expressions like `{%begin%}`/`{%end%}`, the MACRO_CONTENT
// symbol is valid. We don't care about matching `end` or other keywords.
//
// This function may end up scanning some characters multiple times. First, it will scan until it
// finds a nesting keyword like `begin`. It will return SR_STOP if any macro literal content has
// been consumed so far, so the overall scan has a result of MACRO_CONTENT_NESTING. Then the
// external scanner is triggered again, starting exactly at the beginning of the keyword. The
// keyword is matched again, and this function returns SR_STOP_NO_CONTENT. The overall scan will
// not return a token on the second scan, so the grammar rule for the `begin`
// keyword matches instead.
static ScanResult scan_macro_contents(State *state, TSLexer *lexer, const bool *valid_symbols) {
    // Set to true if any content has been scanned with advance. This signals
    // the overall scan will return MACRO_CONTENT or MACRO_CONTENT_NESTING.
    bool found_content = false;
    // Set to true if the scan is looking for nesting keywords.
    bool nesting = false;
    // Set to true if a nesting keyword may begin at this point in the scan.
    bool keyword_can_begin = true;

    // NOTE: See also the comments for MacroState.in_comment and
    // MacroState.non_modifier_keyword_can_begin

    lexer->result_symbol = MACRO_CONTENT;

    if (valid_symbols[MACRO_CONTENT_NESTING]) {
        assert(!valid_symbols[MACRO_CONTENT] || valid_symbols[ERROR_RECOVERY]);
        nesting = true;
        lexer->result_symbol = MACRO_CONTENT_NESTING;
    }

#define RETURN_NESTING_CONTENT                                            \
    if (nesting && !state->macro_state.in_comment && keyword_can_begin) { \
        if (found_content) {                                              \
            return SR_STOP;                                               \
        } else {                                                          \
            return SR_STOP_NO_CONTENT;                                    \
        }                                                                 \
    }

#define RETURN_CONTENT             \
    if (found_content) {           \
        return SR_STOP;            \
    } else {                       \
        return SR_STOP_NO_CONTENT; \
    }

    for (;;) {
        if (lexer->eof(lexer)) {
            DEBUG("reached EOF");
            RETURN_CONTENT;
        }

        // keywords that decrease nesting:
        // end

        // keywords that don't change nesting:
        // abstract def

        // keywords that increase nesting:
        // abstract class
        // abstract struct
        // annotation
        // begin
        // case
        // class
        // do
        // def
        // enum
        // fun
        // lib
        // macro
        // module
        // select
        // struct
        // union

        // keywords that increase nesting only at the beginning of line
        // if
        // unless
        // until
        // while

        switch (lexer->lookahead) {
            case '{':
                lexer->mark_end(lexer);

                // In a state like
                //   %macro_var{foo}
                //             ^
                // let the grammar handle the rest of the macro var expressions
                if (valid_symbols[START_OF_MACRO_VAR_EXPS] && !found_content) {
                    return SR_STOP_NO_CONTENT;
                }

                lex_advance(lexer);

                if (lexer->lookahead == '{' || lexer->lookahead == '%') {
                    // This is the start of a macro expression. After the macro expression ends,
                    // if/unless is a modifier.
                    state->macro_state.non_modifier_keyword_can_begin = false;
                    if (found_content) {
                        return SR_STOP;
                    }

                    lex_advance(lexer);
                    lexer->mark_end(lexer);

                    // Check if next token is a special macro keyword. If we detect end/else/elsif,
                    // return the corresponding MACRO_DELIMITER_* token for the initial `{%`.

                    while (iswspace(lexer->lookahead)) {
                        lex_advance(lexer);
                    }

                    if (lexer->lookahead == 'e') {
                        lex_advance(lexer);

                        if (lexer->lookahead == 'n') {
                            if (match_macro_keyword(lexer, "nd")) {
                                lexer->result_symbol = MACRO_DELIMITER_END;
                                return SR_STOP;
                            }
                        } else if (lexer->lookahead == 'l') {
                            lex_advance(lexer);
                            if (lexer->lookahead == 's') {
                                lex_advance(lexer);
                                if (lexer->lookahead == 'e') {
                                    if (match_macro_keyword(lexer, "e")) {
                                        lexer->result_symbol = MACRO_DELIMITER_ELSE;
                                        return SR_STOP;
                                    }
                                } else if (lexer->lookahead == 'i') {
                                    if (match_macro_keyword(lexer, "if")) {
                                        lexer->result_symbol = MACRO_DELIMITER_ELSIF;
                                        return SR_STOP;
                                    }
                                }
                            }
                        }
                    }
                    return SR_STOP_NO_CONTENT;
                }

                // This is the start of a tuple, brace block, etc.
                found_content = true;
                keyword_can_begin = true;
                state->macro_state.non_modifier_keyword_can_begin = true;
                lexer->mark_end(lexer);
                continue;

            case '}':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // This might be the end of a block, hash/tuple, or macro expression. In any case,
                // if/unless is a modifier after this point.
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == '}') {
                    RETURN_CONTENT;
                }

                // This is the end for a tuple, brace block, etc. Non-modifier keywords are valid.
                found_content = true;
                keyword_can_begin = true;
                lexer->mark_end(lexer);
                continue;

            case '%':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // This might be the end of a macro expression, or a macro variable, or a modulo
                // operator. In any case, if/unless is a modifier after this point.
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == '}') {
                    RETURN_CONTENT;
                } else if (lexer->lookahead == 'i'
                    || lexer->lookahead == 'q'
                    || lexer->lookahead == 'Q'
                    || lexer->lookahead == 'r'
                    || lexer->lookahead == 'w'
                    || lexer->lookahead == 'x') {
                    lex_advance(lexer);

                    if (lexer->lookahead == '('
                        || lexer->lookahead == '<'
                        || lexer->lookahead == '['
                        || lexer->lookahead == '{'
                        || lexer->lookahead == '|') {
                        // TODO eventually we'll return here to mark this as a delimiter
                        // For now, just continue
                        lex_advance(lexer);
                    }
                } else if (is_ident_part(lexer->lookahead)) {
                    // This is a macro var
                    RETURN_CONTENT;
                }

                // Keywords are not valid here: `%begin` is a macro variable
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case '"':
                // Whether we are delegating to string rules or not, if/unless is a
                // modifier after this point.
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (valid_symbols[STRING_LITERAL_START]) {
                    // Delegate to string rules
                    if (found_content) {
                        return SR_STOP;
                    } else {
                        return SR_CONTINUE;
                    }
                }

                lex_advance(lexer);
                lexer->mark_end(lexer);

                // Probably inside a string in a non-nesting context, so keywords aren't valid
                found_content = true;
                keyword_can_begin = false;
                continue;

            case '#':
                lex_advance(lexer);
                lexer->mark_end(lexer);

                // Mark the rest of the line as a comment, where nesting keywords don't apply
                state->macro_state.in_comment = true;
                found_content = true;
                keyword_can_begin = false;
                state->macro_state.non_modifier_keyword_can_begin = false;
                continue;

            case 'a':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == 'b') {
                    if (match_macro_keyword(lexer, "bstract")) {
                        if (iswspace(lexer->lookahead)) {
                            lex_advance(lexer);

                            switch (lexer->lookahead) {
                                case 'c':
                                    if (match_macro_keyword(lexer, "class")) { RETURN_NESTING_CONTENT; }
                                    break;
                                case 's':
                                    if (match_macro_keyword(lexer, "struct")) { RETURN_NESTING_CONTENT; }
                                    break;
                                case 'd':
                                    // fully consume "abstract def", which doesn't increase the nesting level
                                    match_macro_keyword(lexer, "def");
                            }
                        }
                    }
                } else if (lexer->lookahead == 'n') {
                    if (match_macro_keyword(lexer, "nnotation")) { RETURN_NESTING_CONTENT; }
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'b':
                lexer->mark_end(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (match_macro_keyword(lexer, "begin")) { RETURN_NESTING_CONTENT; }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'c':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == 'l') {
                    if (match_macro_keyword(lexer, "lass")) { RETURN_NESTING_CONTENT; }
                } else if (lexer->lookahead == 'a') {
                    if (match_macro_keyword(lexer, "ase")) { RETURN_NESTING_CONTENT; }
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'd':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == 'e') {
                    if (match_macro_keyword(lexer, "ef")) { RETURN_NESTING_CONTENT; }
                } else if (lexer->lookahead == 'o') {
                    if (match_macro_keyword(lexer, "o")) { RETURN_NESTING_CONTENT; }
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'e':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == 'n') {
                    lex_advance(lexer);

                    if (lexer->lookahead == 'd') {
                        if (match_macro_keyword(lexer, "d")) { RETURN_NESTING_CONTENT; }
                    } else if (lexer->lookahead == 'u') {
                        if (match_macro_keyword(lexer, "um")) { RETURN_NESTING_CONTENT; }
                    }
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'f':
                lexer->mark_end(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (match_macro_keyword(lexer, "fun")) { RETURN_NESTING_CONTENT; }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'i':
                lexer->mark_end(lexer);

                if (state->macro_state.non_modifier_keyword_can_begin) {
                    if (match_macro_keyword(lexer, "if")) {
                        if (nesting && !state->macro_state.in_comment && keyword_can_begin) {
                            if (found_content) {
                                // Don't set non_modifier_keyword_can_begin yet, the scan is going
                                // to re-enter at this point.
                                return SR_STOP;
                            } else {
                                state->macro_state.non_modifier_keyword_can_begin = false;
                                return SR_STOP_NO_CONTENT;
                            }
                        }
                    } else {
                        // Whatever identifier this is, if/unless is a modifier after this point
                        state->macro_state.non_modifier_keyword_can_begin = false;
                    }
                } else {
                    lex_advance(lexer);
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'l':
                lexer->mark_end(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (match_macro_keyword(lexer, "lib")) { RETURN_NESTING_CONTENT; }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'm':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == 'a') {
                    if (match_macro_keyword(lexer, "acro")) { RETURN_NESTING_CONTENT; }
                } else if (lexer->lookahead == 'o') {
                    if (match_macro_keyword(lexer, "odule")) { RETURN_NESTING_CONTENT; }
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 's':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                // Whatever identifier this is, if/unless is a modifier after this point
                state->macro_state.non_modifier_keyword_can_begin = false;

                if (lexer->lookahead == 'e') {
                    if (match_macro_keyword(lexer, "elect")) { RETURN_NESTING_CONTENT; }
                } else if (lexer->lookahead == 't') {
                    if (match_macro_keyword(lexer, "truct")) { RETURN_NESTING_CONTENT; }
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'u':
                lexer->mark_end(lexer);
                lex_advance(lexer);

                if (lexer->lookahead == 'n') {
                    lex_advance(lexer);

                    if (lexer->lookahead == 'i') {
                        // Whatever identifier this is, if/unless is a modifier after this point
                        state->macro_state.non_modifier_keyword_can_begin = false;
                        if (match_macro_keyword(lexer, "ion")) { RETURN_NESTING_CONTENT; }
                    } else if (lexer->lookahead == 'l') {
                        if (state->macro_state.non_modifier_keyword_can_begin && match_macro_keyword(lexer, "less")) {
                            if (nesting && !state->macro_state.in_comment && keyword_can_begin) {
                                if (found_content) {
                                    // Don't set non_modifier_keyword_can_begin yet, the scan is going
                                    // to re-enter at this point.
                                    return SR_STOP;
                                } else {
                                    state->macro_state.non_modifier_keyword_can_begin = false;
                                    return SR_STOP_NO_CONTENT;
                                }
                            }
                        }
                    } else if (lexer->lookahead == 't') {
                        if (state->macro_state.non_modifier_keyword_can_begin && match_macro_keyword(lexer, "til")) {
                            if (nesting && !state->macro_state.in_comment && keyword_can_begin) {
                                if (found_content) {
                                    // Don't set non_modifier_keyword_can_begin yet, the scan is going
                                    // to re-enter at this point.
                                    return SR_STOP;
                                } else {
                                    state->macro_state.non_modifier_keyword_can_begin = false;
                                    return SR_STOP_NO_CONTENT;
                                }
                            }
                        }
                    } else {
                        // Whatever identifier this is, if/unless is a modifier after this point
                        state->macro_state.non_modifier_keyword_can_begin = false;
                    }
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case 'w':
                lexer->mark_end(lexer);

                if (state->macro_state.non_modifier_keyword_can_begin) {
                    if (match_macro_keyword(lexer, "while")) {
                        if (nesting && !state->macro_state.in_comment && keyword_can_begin) {
                            if (found_content) {
                                // Don't set non_modifier_keyword_can_begin yet, the scan is going
                                // to re-enter at this point.
                                return SR_STOP;
                            } else {
                                state->macro_state.non_modifier_keyword_can_begin = false;
                                return SR_STOP_NO_CONTENT;
                            }
                        }
                    } else {
                        // Whatever identifier this is, if/unless is a modifier after this point
                        state->macro_state.non_modifier_keyword_can_begin = false;
                    }
                } else {
                    lex_advance(lexer);
                }

                // Keywords are not valid immediately after an identifier
                found_content = true;
                keyword_can_begin = false;
                lexer->mark_end(lexer);
                continue;

            case '\n':
                lex_advance(lexer);
                lexer->mark_end(lexer);
                found_content = true;
                // We've reached the end of the line, no more comment
                state->macro_state.in_comment = false;
                // Keywords are always valid on a new line
                keyword_can_begin = true;
                state->macro_state.non_modifier_keyword_can_begin = true;
                continue;

            case ' ':
            case '\t':
            case '\f':
            case '\v':
            case '\r':
                // These whitespace characters may be followed by a keyword
                lex_advance(lexer);
                lexer->mark_end(lexer);
                found_content = true;
                keyword_can_begin = true;
                continue;

            case '(':
            case '[':
                // These nesting characters may be followed by a keyword
                lex_advance(lexer);
                lexer->mark_end(lexer);
                found_content = true;
                keyword_can_begin = true;
                continue;

            case '=':
                lex_advance(lexer);
                lexer->mark_end(lexer);
                found_content = true;
                keyword_can_begin = false;

                if (iswspace(lexer->lookahead)) {
                    // After `= `, if/unless is treated as a regular keyword, not modifier
                    state->macro_state.non_modifier_keyword_can_begin = true;
                }
                continue;
        }

        lex_advance(lexer);
        lexer->mark_end(lexer);
        found_content = true;
        // We've scanned something that's not whitespace or an opening character. Most keywords
        // are not allowed until we reach whitespace, and if/unless is treated as a modifier
        // for the rest of the line.
        keyword_can_begin = false;
        state->macro_state.non_modifier_keyword_can_begin = false;
    }
}

static ScanResult scan_symbol_content(TSLexer *lexer) {
    int32_t lookahead = lexer->lookahead;

    if (('A' <= lookahead && lookahead <= 'Z')
        || ('a' <= lookahead && lookahead <= 'z')
        || (lookahead == '_')
        || (0x00a0 <= lookahead && lookahead <= 0x10ffffff)) {

        lexer->result_symbol = UNQUOTED_SYMBOL_CONTENT;
        lex_advance(lexer);

        while (is_ident_part(lexer->lookahead)) {
            lex_advance(lexer);
        }

        switch (lexer->lookahead) {
            case '?':
                // Symbols like `:foo?` always include the trailing character
                lex_advance(lexer);
                return SR_STOP;

            case '!':
            case '=':
                // Symbols like `:foo!` or `:bar=` include the trailing character,
                // only if the next char isn't also `=`
                lexer->mark_end(lexer);
                lex_advance(lexer);

                if (lexer->lookahead != '=') {
                    lexer->mark_end(lexer);
                }
                return SR_STOP;

            default:
                return SR_STOP;
        }
    }

    return SR_CONTINUE;
}

static bool scan_regex_modifier(State *state, TSLexer *lexer) {
    if (!state->has_leading_whitespace) {
        bool found_modifier = false;

        for (;;) {
            switch (lexer->lookahead) {
                case 'i':
                case 'm':
                case 'x':
                    found_modifier = true;
                    lex_advance(lexer);
                    continue;
            }

            if (found_modifier) {
                lexer->result_symbol = REGEX_MODIFIER;
                return true;
            } else {
                break;
            }
        }
    }

    return false;
}

// Advance while the next character is a space or tab
static void advance_space(TSLexer *lexer) {
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
        lex_advance(lexer);
    }
}

// Advance while the next character is a space, tab, or newline
static void advance_space_and_newline(TSLexer *lexer) {
    while (lexer->lookahead == ' '
        || lexer->lookahead == '\t'
        || lexer->lookahead == '\r'
        || lexer->lookahead == '\n') {
        lex_advance(lexer);
    }
}

static void consume_const(TSLexer *lexer) {
    if ('A' <= lexer->lookahead && lexer->lookahead <= 'Z') {
        lex_advance(lexer);

        while (is_ident_part(lexer->lookahead)) {
            lex_advance(lexer);
        }
    }
}

static void consume_string_literal(TSLexer *lexer) {
    bool can_escape = true, can_nest;
    int32_t opening_char = 0, closing_char, nesting_level = 0;

    if (lexer->lookahead == '"') {
        opening_char = '"';
        closing_char = '"';
        can_nest = false;
    } else if (lexer->lookahead == '%') {
        lex_advance(lexer);

        if (lexer->lookahead == 'q') {
            can_escape = false;
            lex_advance(lexer);
        } else if (lexer->lookahead == 'Q') {
            lex_advance(lexer);
        }

        switch (lexer->lookahead) {
            case '{':
                opening_char = '{';
                closing_char = '}';
                can_nest = true;
                break;
            case '(':
                opening_char = '(';
                closing_char = ')';
                can_nest = true;
                break;
            case '[':
                opening_char = '[';
                closing_char = ']';
                can_nest = true;
                break;
            case '<':
                opening_char = '<';
                closing_char = '>';
                can_nest = true;
                break;
            case '|':
                opening_char = '|';
                closing_char = '|';
                can_nest = false;
                break;
        }
    }

    if (!opening_char) {
        // this isn't a string
        return;
    }

    // advance past opening char
    lex_advance(lexer);

    for (;;) {

        if (lexer->eof(lexer)) {
            return;
        }

        if (lexer->lookahead == '\\' && can_escape) {
            // consume the backslash and next character
            lex_advance(lexer);
            lex_advance(lexer);
            continue;
        }

        if (lexer->lookahead == closing_char) {
            lex_advance(lexer);

            if (nesting_level == 0) {
                // reached the end of the literal
                return;
            }

            assert(nesting_level > 0);
            nesting_level--;
            continue;
        }

        if (lexer->lookahead == opening_char && can_nest) {
            lex_advance(lexer);
            nesting_level++;
            continue;
        }

        lex_advance(lexer);
    }
}

// Check if there is a type suffix (e.g. `?` or `.class`) or a delimiter like `|`
static LookaheadResult lookahead_delimiter_or_type_suffix(State *state, TSLexer *lexer) {
    if (lexer->eof(lexer)) { return true; }

    switch (lexer->lookahead) {
        case '.':
            lex_advance(lexer);
            advance_space_and_newline(lexer);
            if (lexer->lookahead != 'c') { return false; }
            lex_advance(lexer);
            if (lexer->lookahead != 'l') { return false; }
            lex_advance(lexer);
            if (lexer->lookahead != 'a') { return false; }
            lex_advance(lexer);
            if (lexer->lookahead != 's') { return false; }
            lex_advance(lexer);
            if (lexer->lookahead != 's') { return false; }
            lex_advance(lexer);
            if (is_ident_part(lexer->lookahead)) {
                // the keyword doesn't end after `class`
                return LOOKAHEAD_UNKNOWN;
            } else {
                // .class type
                return LOOKAHEAD_TYPE;
            }

        case '?':
        case '*':
            lex_advance(lexer);
            return lookahead_delimiter_or_type_suffix(state, lexer);

        case '-':
            lex_advance(lexer);
            if (lexer->lookahead == '>') {
                // Const -> is considered a type suffix
                return LOOKAHEAD_TYPE;
            }
            return LOOKAHEAD_UNKNOWN;

        case '=':
            lex_advance(lexer);
            switch (lexer->lookahead) {
                case '>':
                    // Const => is considered a type suffix
                    return LOOKAHEAD_TYPE;
                case '=':
                case '~':
                    // other operators
                    return LOOKAHEAD_UNKNOWN;
                default:
                    // Const = is considered a type suffix
                    return LOOKAHEAD_TYPE;
            }

        case '|':
        case ',':
        case ';':
        case '\n':
        case '(':
        case ')':
        case '[':
        case ']':
            // other type delimiters
            return LOOKAHEAD_TYPE;

        default:
            return LOOKAHEAD_UNKNOWN;
    }
}

// Check if there is an identifier followed by `:` indicating the start of a named tuple item
static LookaheadResult lookahead_start_of_named_tuple_entry(TSLexer *lexer, bool started) {
    if (started
        || ('a' <= lexer->lookahead && lexer->lookahead <= 'z')
        || ('A' <= lexer->lookahead && lexer->lookahead <= 'Z')
        || (lexer->lookahead == '_')
        || (0x00a0 <= lexer->lookahead && lexer->lookahead <= 0x10ffffff)) {

        while (('0' <= lexer->lookahead && lexer->lookahead <= '9')
            || ('A' <= lexer->lookahead && lexer->lookahead <= 'Z')
            || ('a' <= lexer->lookahead && lexer->lookahead <= 'z')
            || (lexer->lookahead == '_')
            || (0x00a0 <= lexer->lookahead && lexer->lookahead <= 0x10ffffff)) {
            lex_advance(lexer);
        }

        if ((lexer->lookahead == '!') || (lexer->lookahead == '?')) {
            lex_advance(lexer);
        }

        if (lexer->lookahead == ':') {
            lex_advance(lexer);
            if (lexer->lookahead == ':') {
                return LOOKAHEAD_UNKNOWN;
            }

            return LOOKAHEAD_NAMED_TUPLE;
        }
    }

    if (lexer->lookahead == '"' || lexer->lookahead == '%') {
        consume_string_literal(lexer);

        if (lexer->lookahead == ':') {
            lex_advance(lexer);
            if (lexer->lookahead == ':') {
                return LOOKAHEAD_UNKNOWN;
            }

            return LOOKAHEAD_NAMED_TUPLE;
        }
    }

    return LOOKAHEAD_UNKNOWN;
}

// Check to see if the next token is part of the type grammar or not. Based on
// https://github.com/crystal-lang/crystal/blob/cd2b7d6490301e092cecc22dfbc91d0f9553ba20/src/compiler/crystal/syntax/parser.cr#L5195
// As the compiler code notes, these conditions are not completely accurate in determining what
// could or could not be a type.
static LookaheadResult lookahead_start_of_type(State *state, TSLexer *lexer) {

    advance_space(lexer);

    if (lexer->eof(lexer)) {
        DEBUG("reached EOF");
        return LOOKAHEAD_UNKNOWN;
    }

    while (lexer->lookahead == '{' || lexer->lookahead == '(') {
        lex_advance(lexer);
        advance_space_and_newline(lexer);
    }

    // Check for identifier
    if (lexer->lookahead == 't') {
        lex_advance(lexer);
        if (lexer->lookahead == 'y') {
            lex_advance(lexer);
            if (lexer->lookahead == 'p') {
                lex_advance(lexer);
                if (lexer->lookahead == 'e') {
                    lex_advance(lexer);
                    if (lexer->lookahead == 'o') {
                        lex_advance(lexer);
                        if (lexer->lookahead == 'f') {
                            lex_advance(lexer);

                            if (lexer->lookahead == ':') {
                                // named tuple start
                                return LOOKAHEAD_NAMED_TUPLE;
                            }

                            if (is_ident_part(lexer->lookahead)
                                || (lexer->lookahead == '!')
                                || (lexer->lookahead == '?')) {
                                // identifier continues beyond `typeof`
                                return lookahead_start_of_named_tuple_entry(lexer, true);
                            }

                            return LOOKAHEAD_TYPE;
                        }
                    }
                }
            }
        }
    } else if (lexer->lookahead == 's') {
        lex_advance(lexer);
        if (lexer->lookahead == 'e') {
            lex_advance(lexer);
            if (lexer->lookahead == 'l') {
                lex_advance(lexer);
                if (lexer->lookahead == 'f') {
                    lex_advance(lexer);

                    if (lexer->lookahead == ':') {
                        // named tuple start
                        return LOOKAHEAD_NAMED_TUPLE;
                    }

                    if (is_ident_part(lexer->lookahead)
                        || (lexer->lookahead == '!')) {
                        // identifier continues beyond `self`
                        return lookahead_start_of_named_tuple_entry(lexer, true);
                    }

                    advance_space(lexer);
                    return lookahead_delimiter_or_type_suffix(state, lexer);
                }
            }
        }
    } else if (('a' <= lexer->lookahead && lexer->lookahead <= 'z')
        || (0x00a0 <= lexer->lookahead && lexer->lookahead <= 0x10ffffff)) {
        // other identifiers are not part of the type grammar
        return lookahead_start_of_named_tuple_entry(lexer, false);
    }

    // Check for constant
    while ('A' <= lexer->lookahead && lexer->lookahead <= 'Z') {
        consume_const(lexer);

        if (lexer->lookahead == ':') {
            lex_advance(lexer);

            if (lexer->lookahead == ':') {
                lex_advance(lexer);
                advance_space_and_newline(lexer);
                // continue consuming const segments
            } else {
                // named tuple start
                return LOOKAHEAD_NAMED_TUPLE;
            }
        } else {
            advance_space(lexer);
            return lookahead_delimiter_or_type_suffix(state, lexer);
        }
    }

    switch (lexer->lookahead) {
        case '_':
            lex_advance(lexer);
            if (!iswalnum(lexer->lookahead)) {
                // This is just a plain underscore
                return LOOKAHEAD_TYPE;
            }
            break;
        case '-':
            lex_advance(lexer);
            if (lexer->lookahead == '>') {
                // proc type
                return LOOKAHEAD_TYPE;
            }
            break;
        case '*':
            lex_advance(lexer);
            advance_space_and_newline(lexer);
            if (lexer->lookahead == '*') {
                // double splat is not a valid type operator
                return LOOKAHEAD_UNKNOWN;
            } else {
                if (lookahead_start_of_type(state, lexer) == LOOKAHEAD_TYPE) {
                    return LOOKAHEAD_TYPE;
                } else {
                    // If lookahead_start_of_type returns LOOKAHEAD_NAMED_TUPLE
                    // at this point, it's not valid, because named tuple tags
                    // can't start with '*'
                    return LOOKAHEAD_UNKNOWN;
                }
            }
            break;
        case '"':
        case '%':
            return lookahead_start_of_named_tuple_entry(lexer, false);
    }

    DEBUG("Not the start of a type");
    return LOOKAHEAD_UNKNOWN;
}

static bool inner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    State *state = (State *)payload;
    state->has_leading_whitespace = false;

    if (check_for_heredoc_start(state, lexer, valid_symbols)) {
        return true;
    }

    // The previous_line_continued flag only matters for check_for_heredoc_start,
    // so it can now be cleared.
    if (state->previous_line_continued) {
        state->previous_line_continued = false;
    }

    if (valid_symbols[DELIMITED_STRING_CONTENTS] && HAS_ACTIVE_LITERAL(state)) {
        switch (scan_string_contents(state, lexer, valid_symbols)) {
            case SR_STOP:
                return true;
            case SR_STOP_NO_CONTENT:
                return false;
            case SR_CONTINUE:
                break;
        }
    }

    if (valid_symbols[HEREDOC_CONTENT] && state->heredocs.size > 0 && scan_heredoc_contents(state, lexer, valid_symbols)) {
        return true;
    }

    if (valid_symbols[MACRO_START] && !valid_symbols[ERROR_RECOVERY]) {
        reset_macro_state(state);
        lexer->result_symbol = MACRO_START;
        return true;
    }

    if ((valid_symbols[MACRO_CONTENT] || valid_symbols[MACRO_CONTENT_NESTING]) && !valid_symbols[ERROR_RECOVERY]) {
        switch (scan_macro_contents(state, lexer, valid_symbols)) {
            case SR_STOP:
                return true;
            case SR_STOP_NO_CONTENT:
                return false;
            case SR_CONTINUE:
                break;
        }
    }

    if (valid_symbols[UNQUOTED_SYMBOL_CONTENT] && !valid_symbols[ERROR_RECOVERY]) {
        switch (scan_symbol_content(lexer)) {
            case SR_STOP:
                return true;
            case SR_STOP_NO_CONTENT:
                return false;
            case SR_CONTINUE:
                break;
        }
    }

    lexer->result_symbol = NONE;

    if (!scan_whitespace(state, lexer, valid_symbols)) {
        return false;
    }

    if (lexer->result_symbol != NONE) {
        return true;
    }

    if (valid_symbols[PERCENT_LITERAL_END] && HAS_ACTIVE_LITERAL(state)) {
        if (lexer->lookahead == ACTIVE_LITERAL(state)->closing_char) {
            lex_advance(lexer);
            (void)POP_LITERAL(state);
            lexer->result_symbol = PERCENT_LITERAL_END;
            return true;
        }
    }

    if (valid_symbols[STRING_LITERAL_END] && HAS_ACTIVE_LITERAL(state)) {
        if (lexer->lookahead == ACTIVE_LITERAL(state)->closing_char) {
            lex_advance(lexer);
            (void)POP_LITERAL(state);
            lexer->result_symbol = STRING_LITERAL_END;
            return true;
        }
    }

    if (valid_symbols[COMMAND_LITERAL_END] && HAS_ACTIVE_LITERAL(state)) {
        if (lexer->lookahead == ACTIVE_LITERAL(state)->closing_char) {
            lex_advance(lexer);
            (void)POP_LITERAL(state);
            lexer->result_symbol = COMMAND_LITERAL_END;
            return true;
        }
    }

    if (valid_symbols[DELIMITED_ARRAY_ELEMENT_START] && HAS_ACTIVE_LITERAL(state)) {
        lexer->result_symbol = DELIMITED_ARRAY_ELEMENT_START;
        return true;
    }

    if (valid_symbols[REGEX_MODIFIER] && scan_regex_modifier(state, lexer)) {
        return true;
    }

    switch (lexer->lookahead) {
        case '{':
            lex_advance(lexer);

            // Start of a macro expression
            if (lexer->lookahead == '{' || lexer->lookahead == '%') {
                return false;
            }

            // We expect these symbols to always be valid or not valid together
            assert(valid_symbols[START_OF_TUPLE_TYPE] == valid_symbols[START_OF_NAMED_TUPLE_TYPE]);

#define BRACE_BLOCK (valid_symbols[START_OF_BRACE_BLOCK])
#define BRACE_EXPR  (valid_symbols[START_OF_HASH_OR_TUPLE] || valid_symbols[START_OF_NAMED_TUPLE])
#define BRACE_TYPE  (valid_symbols[START_OF_TUPLE_TYPE] || valid_symbols[START_OF_NAMED_TUPLE_TYPE])

            if (BRACE_BLOCK || BRACE_EXPR || BRACE_TYPE) {
                if (BRACE_BLOCK && BRACE_EXPR && BRACE_TYPE) {
                    if (valid_symbols[ERROR_RECOVERY]) {
                        return false;
                    } else {
                        // Shouldn't reach here
                        assert(!(
                            valid_symbols[START_OF_BRACE_BLOCK]
                            && (valid_symbols[START_OF_HASH_OR_TUPLE] || valid_symbols[START_OF_NAMED_TUPLE])
                            && (valid_symbols[START_OF_TUPLE_TYPE] || valid_symbols[START_OF_NAMED_TUPLE_TYPE])));
                        return false;
                    }
                } else if (BRACE_BLOCK && BRACE_EXPR) {
                    // In Crystal, the '{' token may be used as the start of a block,
                    // or the start of another literal like a tuple. The language
                    // resolves this potential ambiguity by requiring that the first
                    // non-block argument to a method invoked without parentheses may
                    // not start with a '{'. In other words, if you want to pass a
                    // tuple as the first argument, the method call _must_ use
                    // parentheses.
                    //
                    // This means, if we see a '{' and we're in a context where a block
                    // could be valid, it must be the start of a block.

                    if (valid_symbols[START_OF_PARENLESS_ARGS]) {
                        lexer->result_symbol = START_OF_BRACE_BLOCK;
                        return true;
                    }

                    // Another edge case here is after a range operator:
                    //   foo 1, 2, 3 .. { 4 }
                    // Crystal always considers this as a hash or tuple, not a block
                    if (valid_symbols[END_OF_RANGE]) {
                        // We don't want to consume while looking ahead
                        lexer->mark_end(lexer);
                        advance_space_and_newline(lexer);

                        // After a range operator, these symbols should only be valid together
                        assert(valid_symbols[START_OF_NAMED_TUPLE] && valid_symbols[START_OF_HASH_OR_TUPLE]);

                        switch (lookahead_start_of_named_tuple_entry(lexer, false)) {
                            case LOOKAHEAD_NAMED_TUPLE:
                                lexer->result_symbol = START_OF_NAMED_TUPLE;
                                return true;

                            default:
                                lexer->result_symbol = START_OF_HASH_OR_TUPLE;
                                return true;
                        }
                    }

                    // This is the start of an array-like or hash-like constructor:
                    //   MyArray { 1, 2, 3 }
                    // Named tuples are not accepted here.
                    if (valid_symbols[START_OF_HASH_OR_TUPLE] && !valid_symbols[START_OF_NAMED_TUPLE]) {
                        lexer->result_symbol = START_OF_HASH_OR_TUPLE;
                        return true;
                    }

                    // Is there anywhere else '{' could represent either a block or a hash/tuple?
                    assert(valid_symbols[END_OF_RANGE] || valid_symbols[START_OF_PARENLESS_ARGS]);
                    return false;

                } else if (BRACE_BLOCK && BRACE_TYPE) {
                    // We don't want to consume while looking ahead
                    lexer->mark_end(lexer);

                    // Use type lookahead to resolve conflict between start of block and start of tuple type
                    // For example, as a tuple type:
                    //   -> : -> {->} {
                    //     # ...
                    //   }
                    // Or as a block:
                    //   -> : -> { ; Proc(Nil).new{} }

                    switch (lookahead_start_of_type(state, lexer)) {
                        case LOOKAHEAD_TYPE:
                            assert(valid_symbols[START_OF_TUPLE_TYPE]);
                            lexer->result_symbol = START_OF_TUPLE_TYPE;
                            return true;

                        case LOOKAHEAD_NAMED_TUPLE:
                            // When the Crystal parser is trying to resolve whether a token is part
                            // of a type or not, anything that looks like the start of a named
                            // tuple is assumed _not_ to be a type.
                            lexer->result_symbol = START_OF_BRACE_BLOCK;
                            return true;

                        default:
                            lexer->result_symbol = START_OF_BRACE_BLOCK;
                            return true;
                    }

                } else if (BRACE_EXPR && BRACE_TYPE) {
                    // We don't want to consume while looking ahead
                    lexer->mark_end(lexer);

                    // Use type lookahead to resolve conflict between start of hash/tuple and start of tuple type
                    // For example, as a tuple type:
                    //   def foo : ->{Char,Char}; ->{ {'a','b'} } end
                    // Or as a hash:
                    //   def foo : ->{'a'=>'b'}; ->{ nil } end

                    // When distinguishing between type and expression, these symbols should only be valid together
                    assert(valid_symbols[START_OF_NAMED_TUPLE] && valid_symbols[START_OF_HASH_OR_TUPLE]);

                    switch (lookahead_start_of_type(state, lexer)) {
                        case LOOKAHEAD_TYPE:
                            assert(valid_symbols[START_OF_TUPLE_TYPE]);
                            lexer->result_symbol = START_OF_TUPLE_TYPE;
                            return true;

                        case LOOKAHEAD_NAMED_TUPLE:
                            // When the Crystal parser is trying to resolve whether a token is part
                            // of a type or not, anything that looks like the start of a named
                            // tuple is assumed _not_ to be a type.
                            assert(valid_symbols[START_OF_NAMED_TUPLE]);
                            lexer->result_symbol = START_OF_NAMED_TUPLE;
                            return true;

                        default:
                            assert(valid_symbols[START_OF_HASH_OR_TUPLE]);
                            lexer->result_symbol = START_OF_HASH_OR_TUPLE;
                            return true;
                    }

                } else if (BRACE_EXPR) {
                    // We don't want to consume while looking ahead
                    lexer->mark_end(lexer);
                    advance_space_and_newline(lexer);

                    if (valid_symbols[START_OF_HASH_OR_TUPLE] && !valid_symbols[START_OF_NAMED_TUPLE]) {
                        // This is possible with "array-like" or "hash-like" syntax:
                        // Const { a => 1 }
                        lexer->result_symbol = START_OF_HASH_OR_TUPLE;
                        return true;
                    } else if (valid_symbols[START_OF_NAMED_TUPLE] && !valid_symbols[START_OF_HASH_OR_TUPLE]) {
                        // NOTE(keidax): I'm not sure if this can ever occur?
                        lexer->result_symbol = START_OF_NAMED_TUPLE;
                        return true;
                    }

                    switch (lookahead_start_of_named_tuple_entry(lexer, false)) {
                        case LOOKAHEAD_NAMED_TUPLE:
                            lexer->result_symbol = START_OF_NAMED_TUPLE;
                            return true;

                        default:
                            lexer->result_symbol = START_OF_HASH_OR_TUPLE;
                            return true;
                    }

                } else if (BRACE_TYPE) {
                    // We don't want to consume while looking ahead
                    lexer->mark_end(lexer);
                    advance_space_and_newline(lexer);

                    switch (lookahead_start_of_named_tuple_entry(lexer, false)) {
                        case LOOKAHEAD_NAMED_TUPLE:
                            assert(valid_symbols[START_OF_NAMED_TUPLE_TYPE]);
                            lexer->result_symbol = START_OF_NAMED_TUPLE_TYPE;
                            return true;

                        default:
                            assert(valid_symbols[START_OF_TUPLE_TYPE]);
                            lexer->result_symbol = START_OF_TUPLE_TYPE;
                            return true;
                    }

                } else if (BRACE_BLOCK) {
                    lexer->result_symbol = START_OF_BRACE_BLOCK;
                    return true;
                } else {
                    assert(!"This should never be reached");
                }
            }

            break;
        case '[':
            if (valid_symbols[START_OF_INDEX_OPERATOR]) {
                // If there's ambiguity whether '[' is the start of an index
                // access or an array literal, we assume it's an array if
                // there's leading whitespace and we're at the start of a
                // potential method call:
                //   foo [1]
                // If there's no leading whitespace, or we know this isn't the
                // first parameter of a method call, then it must be an index
                // operator:
                //   puts({42} [0])
                if (state->has_leading_whitespace && valid_symbols[START_OF_PARENLESS_ARGS]) {
                    return false;
                } else {
                    lex_advance(lexer);
                    lexer->result_symbol = START_OF_INDEX_OPERATOR;
                    return true;
                }
            }
            break;

        case '<':
            if (valid_symbols[HEREDOC_START]) {
                lex_advance(lexer);

                if (lexer->lookahead == '<') {
                    lex_advance(lexer);

                    if (lexer->lookahead == '-') {
                        lex_advance(lexer);
                        bool quoted = false;
                        bool got_end_quote = false;

                        if (lexer->lookahead == '\'') {
                            quoted = true;
                            lex_advance(lexer);
                        }

                        // How much space is left for this heredoc identifier.
                        size_t max_word_size = HEREDOC_BUFFER_SIZE - heredoc_current_buffer_size(state);
                        if (max_word_size < 1) {
                            return false;
                        }

                        if (max_word_size > MAX_HEREDOC_WORD_SIZE) {
                            max_word_size = MAX_HEREDOC_WORD_SIZE;
                        }

                        uint8_t word[HEREDOC_BUFFER_SIZE + 4];
                        size_t word_length = 0;

                        // First character must be valid in an identifier, even for a quoted heredoc
                        if (is_ident_part(lexer->lookahead)) {
                            size_t byte_size = codepoint_to_utf8(lexer->lookahead, word);
                            if (byte_size == 0) {
                                // We couldn't convert the codepoint
                                return false;
                            }

                            word_length = byte_size;
                            lex_advance(lexer);
                        } else {
                            return false;
                        }

                        while (word_length <= max_word_size) {
                            if (lexer->lookahead == '\r' || lexer->lookahead == '\n' || lexer->eof(lexer)) {
                                // Reached the end of the line
                                break;
                            }

                            if (lexer->lookahead == '\'' && quoted) {
                                // This must be the end of the identifier
                                got_end_quote = true;
                                lex_advance(lexer);
                                break;
                            }

                            if (quoted || is_ident_part(lexer->lookahead)) {
                                // Add to the identifier buffer
                                size_t byte_size = codepoint_to_utf8(lexer->lookahead, word + word_length);
                                if (byte_size == 0) {
                                    // We couldn't convert the codepoint
                                    return false;
                                }

                                word_length += byte_size;
                                lex_advance(lexer);
                            } else {
                                // Not a valid identifier character
                                break;
                            }
                        }

                        if (word_length == 0) {
                            // There wasn't a valid heredoc identifier
                            return false;
                        } else if ((word_length > max_word_size) || (word_length == max_word_size && is_ident_part(lexer->lookahead))) {
                            // The heredoc identifier is too big to store in state.
                            return false;
                        } else if (quoted && !got_end_quote) {
                            // Unterminated quoted heredoc
                            return false;
                        } else {
                            // word contains a heredoc identifier we can store.
                            Heredoc heredoc = {
                                .allow_escapes = !quoted,
                                .started = false,
                                .identifier = array_new(),
                            };

                            array_extend(&heredoc.identifier, word_length, &word);

                            // double check we can safely store the new heredoc
                            if (!has_room_for_heredoc(state, heredoc)) {
                                array_delete(&heredoc.identifier);
                                return false;
                            }

                            push_heredoc(state, heredoc);

                            lexer->result_symbol = HEREDOC_START;
                            DEBUG("heredoc size = %d", word_length);
                            return true;
                        }
                    }
                }
            }
            break;

        case '+':
            if (valid_symbols[UNARY_PLUS] || valid_symbols[BINARY_PLUS]) {
                lex_advance(lexer);

                if (lexer->lookahead == '=') {
                    return false;
                }

                // Give precedence to the unary operator if:
                // - there is space before but not after, e.g.
                //      puts +foo
                // - we are just after a range operator, e.g.
                //      -5 .. + foo
                bool unary_priority = (state->has_leading_whitespace && !iswspace(lexer->lookahead))
                    || valid_symbols[END_OF_RANGE];

                if (valid_symbols[UNARY_PLUS] && unary_priority) {
                    lexer->result_symbol = UNARY_PLUS;
                } else if (valid_symbols[BINARY_PLUS]) {
                    lexer->result_symbol = BINARY_PLUS;
                } else {
                    lexer->result_symbol = UNARY_PLUS;
                }

                return true;
            }
            break;
        case '-':
            if (valid_symbols[UNARY_MINUS] || valid_symbols[BINARY_MINUS]) {
                lex_advance(lexer);

                if (lexer->lookahead == '=' || lexer->lookahead == '>') {
                    return false;
                }

                bool unary_priority = (state->has_leading_whitespace && !iswspace(lexer->lookahead))
                    || valid_symbols[END_OF_RANGE];

                if (valid_symbols[UNARY_MINUS] && unary_priority) {
                    lexer->result_symbol = UNARY_MINUS;
                } else if (valid_symbols[BINARY_MINUS]) {
                    lexer->result_symbol = BINARY_MINUS;
                } else {
                    lexer->result_symbol = UNARY_MINUS;
                }

                return true;
            }
            break;

        case '*':
            if (valid_symbols[POINTER_STAR] || valid_symbols[UNARY_STAR] || valid_symbols[BINARY_STAR] || valid_symbols[UNARY_DOUBLE_STAR] || valid_symbols[BINARY_DOUBLE_STAR]) {
                lex_advance(lexer);

                if (valid_symbols[POINTER_STAR] && !valid_symbols[ERROR_RECOVERY]) {
                    lexer->result_symbol = POINTER_STAR;
                    return true;
                }

                if (lexer->lookahead == '=') {
                    return false;
                }

                if (lexer->lookahead == '*') {
                    lex_advance(lexer);

                    if (lexer->lookahead == '=') {
                        return false;
                    }

                    bool unary_priority = state->has_leading_whitespace && !iswspace(lexer->lookahead);

                    if (valid_symbols[UNARY_DOUBLE_STAR] && unary_priority) {
                        lexer->result_symbol = UNARY_DOUBLE_STAR;
                        return true;
                    } else if (valid_symbols[BINARY_DOUBLE_STAR]) {
                        lexer->result_symbol = BINARY_DOUBLE_STAR;
                        return true;
                    } else if (valid_symbols[UNARY_DOUBLE_STAR] && !iswspace(lexer->lookahead)) {
                        // A splat _cannot_ have whitespace after the *
                        lexer->result_symbol = UNARY_DOUBLE_STAR;
                        return true;
                    }

                    return false;
                }

                bool unary_priority = state->has_leading_whitespace && !iswspace(lexer->lookahead);

                if (valid_symbols[UNARY_STAR] && unary_priority) {
                    lexer->result_symbol = UNARY_STAR;
                    return true;
                } else if (valid_symbols[BINARY_STAR]) {
                    lexer->result_symbol = BINARY_STAR;
                    return true;
                } else if (valid_symbols[UNARY_STAR] && !iswspace(lexer->lookahead)) {
                    // A splat _cannot_ have whitespace after the *
                    lexer->result_symbol = UNARY_STAR;
                    return true;
                }
            }
            break;

        case '&':
            if (
                valid_symbols[UNARY_WRAPPING_PLUS]
                || valid_symbols[UNARY_WRAPPING_MINUS]
                || valid_symbols[BINARY_WRAPPING_PLUS]
                || valid_symbols[BINARY_WRAPPING_MINUS]
                || valid_symbols[BLOCK_AMPERSAND]
                || valid_symbols[BINARY_AMPERSAND]) {
                lex_advance(lexer);
                lexer->mark_end(lexer);

                if (lexer->lookahead == '+') {
                    lex_advance(lexer);

                    if (lexer->lookahead == '=') {
                        return false;
                    }

                    // The binary form of &+ is always preferred. E.g.
                    //   foo! &+bar
                    // is still binary.
                    if (valid_symbols[BINARY_WRAPPING_PLUS]) {
                        lexer->mark_end(lexer);
                        lexer->result_symbol = BINARY_WRAPPING_PLUS;
                        return true;
                    } else if (valid_symbols[UNARY_WRAPPING_PLUS]) {
                        lexer->mark_end(lexer);
                        lexer->result_symbol = UNARY_WRAPPING_PLUS;
                        return true;
                    }

                    return false;
                }

                if (lexer->lookahead == '-') {
                    lex_advance(lexer);

                    if (lexer->lookahead == '=') {
                        return false;
                    }

                    if (lexer->lookahead == '>') {
                        // In the case of '&->', we always return just the '&',
                        // so mark_end is _not_ called here.

                        bool unary_priority = state->has_leading_whitespace;

                        if (unary_priority && valid_symbols[BLOCK_AMPERSAND]) {
                            lexer->result_symbol = BLOCK_AMPERSAND;
                            return true;
                        } else if (valid_symbols[BINARY_AMPERSAND]) {
                            lexer->result_symbol = BINARY_AMPERSAND;
                            return true;
                        } else if (valid_symbols[BLOCK_AMPERSAND]) {
                            lexer->result_symbol = BLOCK_AMPERSAND;
                            return true;
                        } else {
                            return false;
                        }
                    }

                    if (valid_symbols[BINARY_WRAPPING_MINUS]) {
                        lexer->mark_end(lexer);
                        lexer->result_symbol = BINARY_WRAPPING_MINUS;
                        return true;
                    } else if (valid_symbols[UNARY_WRAPPING_MINUS]) {
                        lexer->mark_end(lexer);
                        lexer->result_symbol = UNARY_WRAPPING_MINUS;
                        return true;
                    }

                    return false;
                }

                if (lexer->lookahead == '*' || lexer->lookahead == '&' || lexer->lookahead == '=') {
                    // Symbols not managed by this external scanner:
                    // '&*', '&**', '&&', '&='
                    return false;
                }

                if (lexer->lookahead == '.') {
                    // '&.' is always treated as a block ampersand.
                    if (valid_symbols[BLOCK_AMPERSAND]) {
                        lexer->result_symbol = BLOCK_AMPERSAND;
                        return true;
                    }

                    return false;
                }
                // Use whitespace to distinguish between '&' as a block
                // argument, and as a binary operator.
                // For example, these are parsed as binary operators:
                //   foo & bar
                //   foo&bar
                // while this is parsed as a block argument:
                //   foo &bar
                bool unary_priority = (state->has_leading_whitespace && !iswspace(lexer->lookahead));
                if (unary_priority && valid_symbols[BLOCK_AMPERSAND]) {
                    lexer->result_symbol = BLOCK_AMPERSAND;
                    return true;
                } else if (valid_symbols[BINARY_AMPERSAND]) {
                    lexer->result_symbol = BINARY_AMPERSAND;
                    return true;
                } else if (valid_symbols[BLOCK_AMPERSAND]) {
                    lexer->result_symbol = BLOCK_AMPERSAND;
                    return true;
                }
            }
            break;

        case '/':
            if (valid_symbols[REGEX_START]
                || valid_symbols[BINARY_SLASH]
                || valid_symbols[BINARY_DOUBLE_SLASH]) {

                lex_advance(lexer);

                if (lexer->lookahead == '=') {
                    if (valid_symbols[REGEX_START] || valid_symbols[BINARY_SLASH]) {
                        if (valid_symbols[REGEX_START] && !valid_symbols[BINARY_SLASH]) {
                            // This is a case like `foo = /=` or `foo(/=`.
                            // We can unambiguously prefer a regex here.
                            lexer->result_symbol = REGEX_START;
                            return true;
                        } else if (valid_symbols[BINARY_SLASH] && !valid_symbols[REGEX_START]) {
                            // This might be a case like `@foo /=`. We assume that `@a /= b` is valid
                            // everywhere `@a / b` is valid, but `/=` is not an external symbol so return false.
                            return false;
                        } else {
                            if (valid_symbols[START_OF_PARENLESS_ARGS]) {
                                // This is a case like `a.b /=`. The Crystal parser uses
                                // `slash_is_not_regex!` in this scenario, so return false.
                                return false;
                            } else {
                                // This could be a case like `a .. /=`. The Crystal parser doesn't
                                // recognize the `/=` token here, so just return false.
                                return false;
                            }
                        }
                    }
                    return false;
                }

                if (lexer->lookahead == '/' && valid_symbols[BINARY_DOUBLE_SLASH]) {
                    lex_advance(lexer);

                    if (lexer->lookahead == '=') {
                        return false;
                    }

                    lexer->result_symbol = BINARY_DOUBLE_SLASH;
                    return true;
                }

                if (valid_symbols[BINARY_SLASH] && !valid_symbols[REGEX_START]) {
                    lexer->result_symbol = BINARY_SLASH;
                    return true;
                } else if (valid_symbols[REGEX_START] && !valid_symbols[BINARY_SLASH]) {
                    lexer->result_symbol = REGEX_START;
                    return true;
                } else {
                    // Both are valid
                    assert(valid_symbols[REGEX_START] && valid_symbols[BINARY_SLASH]);

                    // If we've reached this point, we need to distinguish between two cases:
                    //   foo /
                    //       ^
                    // and
                    //   .. /
                    //      ^
                    // START_OF_PARENLESS_ARGS signals the first case, and END_OF_RANGE signals the
                    // second. If error recovery is active, we won't know for sure which we're in,
                    // so just prefer the first branch.

                    if (valid_symbols[START_OF_PARENLESS_ARGS]) {
                        if (state->has_leading_whitespace
                            && !(lexer->lookahead == ' '
                                || lexer->lookahead == '\t'
                                || lexer->lookahead == '\n'
                                || lexer->lookahead == '\r')) {
                            // If we're in the state
                            //   foo   /a
                            //         ^
                            // then we assume this is the start of a regex.
                            lexer->result_symbol = REGEX_START;
                            return true;
                        } else {
                            // We must be in one of these states:
                            //   foo/a
                            //      ^
                            // or
                            //   foo/ a
                            //      ^
                            // or
                            //   foo / a
                            //       ^
                            // In each of these cases, we give the slash operator
                            // higher precedence over a regex.
                            lexer->result_symbol = BINARY_SLASH;
                            return true;
                        }
                    } else if (valid_symbols[END_OF_RANGE]) {
                        // We must be in this state:
                        // <range_start> .. /
                        //                  ^
                        // Assume this is the start of a regex.
                        lexer->result_symbol = REGEX_START;
                        return true;
                    } else {
                        // This sort of ambiguity should only happen after an identifier without
                        // parentheses, or after a range operator.
                        assert(valid_symbols[START_OF_PARENLESS_ARGS] || valid_symbols[END_OF_RANGE]);
                    }
                }
            }
            break;

        case '%':
            lex_advance(lexer);

            // End of a macro expression
            if (lexer->lookahead == '}') {
                return false;
            }

            // `%=` is not an external token
            if (lexer->lookahead == '=') {
                return false;
            }

            if (valid_symbols[STRING_PERCENT_LITERAL_START]
                || valid_symbols[COMMAND_PERCENT_LITERAL_START]
                || valid_symbols[STRING_ARRAY_PERCENT_LITERAL_START]
                || valid_symbols[SYMBOL_ARRAY_PERCENT_LITERAL_START]
                || valid_symbols[REGEX_PERCENT_LITERAL_START]) {

                LiteralType type = STRING;
                Token return_symbol = STRING_PERCENT_LITERAL_START;

                switch (lexer->lookahead) {
                    case 'Q':
                        lex_advance(lexer);
                        // type is already STRING
                        break;
                    case 'q':
                        lex_advance(lexer);
                        type = STRING_NO_ESCAPE;
                        break;
                    case 'x':
                        lex_advance(lexer);
                        type = COMMAND;
                        return_symbol = COMMAND_PERCENT_LITERAL_START;
                        break;
                    case 'w':
                        lex_advance(lexer);
                        type = STRING_ARRAY;
                        return_symbol = STRING_ARRAY_PERCENT_LITERAL_START;
                        break;
                    case 'i':
                        lex_advance(lexer);
                        type = SYMBOL_ARRAY;
                        return_symbol = SYMBOL_ARRAY_PERCENT_LITERAL_START;
                        break;
                    case 'r':
                        lex_advance(lexer);
                        type = REGEX;
                        return_symbol = REGEX_PERCENT_LITERAL_START;
                        break;
                }

                int32_t opening_char = 0, closing_char;

                switch (lexer->lookahead) {
                    case '{':
                        opening_char = '{';
                        closing_char = '}';
                        break;
                    case '(':
                        opening_char = '(';
                        closing_char = ')';
                        break;
                    case '[':
                        opening_char = '[';
                        closing_char = ']';
                        break;
                    case '<':
                        opening_char = '<';
                        closing_char = '>';
                        break;
                    case '|':
                        opening_char = '|';
                        closing_char = '|';
                        break;
                    default:
                        if (valid_symbols[MODULO_OPERATOR]) {
                            lexer->result_symbol = MODULO_OPERATOR;
                            return true;
                        }
                }

                if (opening_char) {
                    lex_advance(lexer);

                    if (!valid_symbols[return_symbol]) {
                        return false;
                    }

                    lexer->result_symbol = return_symbol;

                    if (state->literals.size >= MAX_LITERAL_COUNT) {
                        // Instead of overflowing the state (and accessing out-of-bounds memory)
                        // we'll just return false, resulting in an error in the syntax tree. The
                        // literals already on the stack can still be parsed successfully.
                        return false;
                    }

                    PUSH_LITERAL(state, ((PercentLiteral){
                                            .opening_char = opening_char,
                                            .closing_char = closing_char,
                                            .type = type,
                                            .nesting_level = 0,
                                        }));

                    return true;
                }

            } else if (valid_symbols[MODULO_OPERATOR]) {
                lexer->result_symbol = MODULO_OPERATOR;
                return true;
            }
            break;

        case '"':
            if (valid_symbols[STRING_LITERAL_START]) {
                lex_advance(lexer);

                PUSH_LITERAL(state, ((PercentLiteral){
                                        .opening_char = '"',
                                        .closing_char = '"',
                                        .type = STRING,
                                        .nesting_level = 0,
                                    }));

                lexer->result_symbol = STRING_LITERAL_START;
                return true;
            } else if (valid_symbols[STRING_LITERAL_END]) {
                lex_advance(lexer);
                lexer->result_symbol = STRING_LITERAL_END;
                return true;
            }
            break;

        case '`':
            if (valid_symbols[COMMAND_LITERAL_START]) {
                lex_advance(lexer);

                PUSH_LITERAL(state, ((PercentLiteral){
                                        .opening_char = '`',
                                        .closing_char = '`',
                                        .type = COMMAND,
                                        .nesting_level = 0,
                                    }));

                lexer->result_symbol = COMMAND_LITERAL_START;
                return true;
            } else if (valid_symbols[COMMAND_LITERAL_END]) {
                lex_advance(lexer);
                lexer->result_symbol = COMMAND_LITERAL_END;
                return true;
            }
            break;

        case '\\':
            if (valid_symbols[LINE_CONTINUATION]) {
                // Don't allow line continuation in a quoted heredoc
                if (has_active_heredoc(state) && !array_get(&state->heredocs, 0)->allow_escapes) {
                    return false;
                }

                // Line continuations may be allowed in some literals
                if (HAS_ACTIVE_LITERAL(state)) {
                    switch (ACTIVE_LITERAL(state)->type) {
                        case STRING_NO_ESCAPE:
                        case REGEX:
                        case STRING_ARRAY:
                        case SYMBOL_ARRAY:
                            // Line continuations aren't allowed here
                            return false;
                        case STRING:
                        case COMMAND:
                            // Continue checking for line continuation
                            break;
                    }
                }

                lex_advance(lexer);

                if (lexer->lookahead == '\r') {
                    lex_advance(lexer);
                }

                if (lexer->lookahead == '\n') {
                    lex_advance(lexer);
                    lexer->result_symbol = LINE_CONTINUATION;
                    state->previous_line_continued = true;
                    return true;
                }
            }
            break;

        case ':':
            if (valid_symbols[START_OF_SYMBOL] || valid_symbols[TYPE_FIELD_COLON]) {
                lex_advance(lexer);

                int32_t lookahead = lexer->lookahead;

                if (state->has_leading_whitespace && iswspace(lookahead) && valid_symbols[TYPE_FIELD_COLON]) {
                    lex_advance(lexer);
                    lexer->result_symbol = TYPE_FIELD_COLON;
                    return true;
                }

                if (!valid_symbols[START_OF_SYMBOL]) {
                    return false;
                }

                switch (lookahead) {
                    case '!':
                    case '%':
                    case '&':
                    case '*':
                    case '+':
                    case '-':
                    case '/':
                    case '<':
                    case '=':
                    case '>':
                    case '[':
                    case '^':
                    case '|':
                    case '~':
                        // start of an operator symbol
                        lexer->result_symbol = START_OF_SYMBOL;
                        return true;
                }

                if (('A' <= lookahead && lookahead <= 'Z')
                    || ('a' <= lookahead && lookahead <= 'z')
                    || (lookahead == '_')
                    || (0x00a0 <= lookahead && lookahead <= 0x10ffffff)) {

                    // This is the start of an unquoted symbol
                    lexer->result_symbol = START_OF_SYMBOL;
                    return true;
                }
            }
            break;
        case '.':
            if (valid_symbols[BEGINLESS_RANGE_OPERATOR] && !valid_symbols[START_OF_PARENLESS_ARGS]) {
                lex_advance(lexer);
                if (lexer->lookahead != '.') {
                    return false;
                }
                lex_advance(lexer);
                if (lexer->lookahead == '.') {
                    lex_advance(lexer);
                }

                lexer->result_symbol = BEGINLESS_RANGE_OPERATOR;
                return true;
            }
            break;

        case 'a':
            if (valid_symbols[ABSTRACT_KEYWORD] || valid_symbols[ALIAS_KEYWORD] || valid_symbols[ALIGNOF_KEYWORD] || valid_symbols[ANNOTATION_KEYWORD] || valid_symbols[ASM_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'b' && valid_symbols[ABSTRACT_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'r') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'a') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'c') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = ABSTRACT_KEYWORD;
                    return true;

                } else if (lexer->lookahead == 'l' && (valid_symbols[ALIAS_KEYWORD] || valid_symbols[ALIGNOF_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'i') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead == 'g' && valid_symbols[ALIGNOF_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 'n') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'o') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'f') { return false; }
                        lex_advance(lexer);

                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = ALIGNOF_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 'a' && valid_symbols[ALIAS_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 's') { return false; }
                        lex_advance(lexer);

                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = ALIAS_KEYWORD;
                        return true;
                    }

                } else if (lexer->lookahead == 'n' && valid_symbols[ANNOTATION_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'o') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'a') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'i') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'o') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = ANNOTATION_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 's' && valid_symbols[ASM_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'm') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = ASM_KEYWORD;
                    return true;
                }
            }
            break;
        case 'b':
            if (valid_symbols[BREAK_KEYWORD] || valid_symbols[BEGIN_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'r' && valid_symbols[BREAK_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'a') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'k') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = BREAK_KEYWORD;
                    return true;

                } else if (lexer->lookahead == 'e' && valid_symbols[BEGIN_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'g') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'i') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = BEGIN_KEYWORD;
                    return true;
                }
            }
            break;
        case 'c':
            if (valid_symbols[CLASS_KEYWORD] || valid_symbols[CASE_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'l' && valid_symbols[CLASS_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'a') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = CLASS_KEYWORD;
                    return true;

                } else if (lexer->lookahead == 'a' && valid_symbols[CASE_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = CASE_KEYWORD;
                    return true;
                }
            }
            break;
        case 'd':
            if (valid_symbols[DEF_KEYWORD] || valid_symbols[DO_KEYWORD]) {
                lex_advance(lexer);

                if (lexer->lookahead == 'o' && valid_symbols[DO_KEYWORD]) {
                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = DO_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 'e' && valid_symbols[DEF_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'f') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = DEF_KEYWORD;
                    return true;
                }
            }
            break;
        case 'e':
            if (valid_symbols[REGULAR_ENSURE_KEYWORD] || valid_symbols[MODIFIER_ENSURE_KEYWORD] || valid_symbols[ENUM_KEYWORD] || valid_symbols[END_KEYWORD] || valid_symbols[EXTEND_KEYWORD] || valid_symbols[ELSE_KEYWORD] || valid_symbols[ELSIF_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'x' && valid_symbols[EXTEND_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'd') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = EXTEND_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 'l' && (valid_symbols[ELSE_KEYWORD] || valid_symbols[ELSIF_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }

                    lex_advance(lexer);
                    if (lexer->lookahead == 'i' && valid_symbols[ELSIF_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 'f') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = ELSIF_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 'e' && valid_symbols[ELSE_KEYWORD]) {
                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = ELSE_KEYWORD;
                        return true;
                    }
                } else if (lexer->lookahead == 'n' && (valid_symbols[REGULAR_ENSURE_KEYWORD] || valid_symbols[MODIFIER_ENSURE_KEYWORD] || valid_symbols[ENUM_KEYWORD] || valid_symbols[END_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead == 'd' && valid_symbols[END_KEYWORD]) {
                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = END_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 'u' && valid_symbols[ENUM_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 'm') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = ENUM_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 's' && (valid_symbols[REGULAR_ENSURE_KEYWORD] || valid_symbols[MODIFIER_ENSURE_KEYWORD])) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 'u') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'r') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'e') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            // This is some other identifier, not 'ensure'
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        if (valid_symbols[MODIFIER_ENSURE_KEYWORD] && !valid_symbols[REGULAR_ENSURE_KEYWORD]) {
                            lexer->result_symbol = MODIFIER_ENSURE_KEYWORD;
                            return true;
                        } else if (valid_symbols[REGULAR_ENSURE_KEYWORD] && !valid_symbols[MODIFIER_ENSURE_KEYWORD]) {
                            lexer->result_symbol = REGULAR_ENSURE_KEYWORD;
                            return true;
                        } else {
                            // Both are valid
                            assert(valid_symbols[MODIFIER_ENSURE_KEYWORD] && valid_symbols[REGULAR_ENSURE_KEYWORD]);

                            // TODO: currently assuming that the modifier always takes
                            // precedence here. Is that correct?
                            lexer->result_symbol = MODIFIER_ENSURE_KEYWORD;
                            return true;
                        }
                    }
                }
            }
            break;
        case 'f':
            if (valid_symbols[FALSE_KEYWORD] || valid_symbols[FUN_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'a' && valid_symbols[FALSE_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'l') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = FALSE_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 'u' && valid_symbols[FUN_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = FUN_KEYWORD;
                    return true;
                }
            }
            break;
        case 'i':
            if (valid_symbols[REGULAR_IF_KEYWORD] || valid_symbols[MODIFIER_IF_KEYWORD] || valid_symbols[INCLUDE_KEYWORD] || valid_symbols[INSTANCE_SIZEOF_KEYWORD] || valid_symbols[IN_KEYWORD] || valid_symbols[INSTANCE_ALIGNOF_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'n' && (valid_symbols[INCLUDE_KEYWORD] || valid_symbols[INSTANCE_SIZEOF_KEYWORD] || valid_symbols[IN_KEYWORD] || valid_symbols[INSTANCE_ALIGNOF_KEYWORD])) {
                    lex_advance(lexer);

                    if (valid_symbols[IN_KEYWORD] && !(next_char_is_identifier(lexer) || next_is_colon_space(lexer))) {
                        lexer->result_symbol = IN_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 'c' && valid_symbols[INCLUDE_KEYWORD]) {
                        if (lexer->lookahead != 'c') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'l') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'u') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'd') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'e') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = INCLUDE_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 's' && (valid_symbols[INSTANCE_SIZEOF_KEYWORD] || valid_symbols[INSTANCE_ALIGNOF_KEYWORD])) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 't') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'a') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'n') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'c') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'e') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != '_') { return false; }
                        lex_advance(lexer);

                        if (lexer->lookahead == 'a' && valid_symbols[INSTANCE_ALIGNOF_KEYWORD]) {
                            lex_advance(lexer);
                            if (lexer->lookahead != 'l') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'i') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'g') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'n') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'o') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'f') { return false; }

                            lex_advance(lexer);
                            if (next_char_is_identifier(lexer)) {
                                return false;
                            } else if (next_is_colon_space(lexer)) {
                                return false;
                            }

                            lexer->result_symbol = INSTANCE_ALIGNOF_KEYWORD;
                            return true;
                        } else if (lexer->lookahead == 's' && valid_symbols[INSTANCE_SIZEOF_KEYWORD]) {
                            lex_advance(lexer);
                            if (lexer->lookahead != 'i') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'z') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'e') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'o') { return false; }
                            lex_advance(lexer);
                            if (lexer->lookahead != 'f') { return false; }

                            lex_advance(lexer);
                            if (next_char_is_identifier(lexer)) {
                                return false;
                            } else if (next_is_colon_space(lexer)) {
                                return false;
                            }

                            lexer->result_symbol = INSTANCE_SIZEOF_KEYWORD;
                            return true;
                        }
                    }
                } else if (lexer->lookahead == 'f' && (valid_symbols[REGULAR_IF_KEYWORD] || valid_symbols[MODIFIER_IF_KEYWORD])) {
                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        // This is some other identifier, not 'if'
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    if (valid_symbols[MODIFIER_IF_KEYWORD] && !valid_symbols[REGULAR_IF_KEYWORD]) {
                        lexer->result_symbol = MODIFIER_IF_KEYWORD;
                        return true;
                    } else if (valid_symbols[REGULAR_IF_KEYWORD] && !valid_symbols[MODIFIER_IF_KEYWORD]) {
                        lexer->result_symbol = REGULAR_IF_KEYWORD;
                        return true;
                    } else {
                        // Both are valid
                        assert(valid_symbols[MODIFIER_IF_KEYWORD] && valid_symbols[REGULAR_IF_KEYWORD]);

                        // This sort of ambiguity may happen after an identifier
                        // without parentheses, or after a keyword like `return`
                        // that takes an optional expression.
                        lexer->result_symbol = MODIFIER_IF_KEYWORD;
                        return true;
                    }
                }
            }
            break;
        case 'l':
            if (valid_symbols[LIB_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead != 'i') { return false; }
                lex_advance(lexer);
                if (lexer->lookahead != 'b') { return false; }

                lex_advance(lexer);
                if (next_char_is_identifier(lexer)) {
                    return false;
                } else if (next_is_colon_space(lexer)) {
                    return false;
                }

                lexer->result_symbol = LIB_KEYWORD;
                return true;
            }
            break;
        case 'm':
            if (valid_symbols[MACRO_KEYWORD] || valid_symbols[MODULE_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'a' && valid_symbols[MACRO_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'c') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'r') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'o') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = MACRO_KEYWORD;
                    return true;

                } else if (lexer->lookahead == 'o' && valid_symbols[MODULE_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'd') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'u') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'l') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = MODULE_KEYWORD;
                    return true;
                }
            }
            break;
        case 'n':
            if (valid_symbols[NEXT_KEYWORD] || valid_symbols[NIL_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'e' && valid_symbols[NEXT_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'x') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = NEXT_KEYWORD;
                    return true;

                } else if (lexer->lookahead == 'i' && valid_symbols[NIL_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'l') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = NIL_KEYWORD;
                    return true;
                }
            }
            break;
        case 'o':
            if (valid_symbols[OFFSETOF_KEYWORD] || valid_symbols[OUT_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'u' && valid_symbols[OUT_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = OUT_KEYWORD;
                    return true;

                } else if (lexer->lookahead == 'f' && valid_symbols[OFFSETOF_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'f') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'o') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'f') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = OFFSETOF_KEYWORD;
                    return true;
                }
            }
            break;
        case 'p':
            if (valid_symbols[PRIVATE_KEYWORD] || valid_symbols[PROTECTED_KEYWORD] || valid_symbols[POINTEROF_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'o' && valid_symbols[POINTEROF_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'i') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'r') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'o') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'f') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = POINTEROF_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 'r' && (valid_symbols[PRIVATE_KEYWORD] || valid_symbols[PROTECTED_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead == 'i' && valid_symbols[PRIVATE_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 'v') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'a') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 't') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'e') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = PRIVATE_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 'o' && valid_symbols[PROTECTED_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 't') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'e') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'c') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 't') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'e') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'd') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = PROTECTED_KEYWORD;
                        return true;
                    }
                }
            }
            break;
        case 'r':
            if (valid_symbols[REGULAR_RESCUE_KEYWORD] || valid_symbols[MODIFIER_RESCUE_KEYWORD] || valid_symbols[REQUIRE_KEYWORD] || valid_symbols[RETURN_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead != 'e') { return false; }
                lex_advance(lexer);

                if (lexer->lookahead == 'q' && valid_symbols[REQUIRE_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'u') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'i') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'r') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = REQUIRE_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 't' && valid_symbols[RETURN_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'u') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'r') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = RETURN_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 's' && (valid_symbols[REGULAR_RESCUE_KEYWORD] || valid_symbols[MODIFIER_RESCUE_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'c') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'u') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        // This is some other identifier, not 'rescue'
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    if (valid_symbols[MODIFIER_RESCUE_KEYWORD] && !valid_symbols[REGULAR_RESCUE_KEYWORD]) {
                        lexer->result_symbol = MODIFIER_RESCUE_KEYWORD;
                        return true;
                    } else if (valid_symbols[REGULAR_RESCUE_KEYWORD] && !valid_symbols[MODIFIER_RESCUE_KEYWORD]) {
                        lexer->result_symbol = REGULAR_RESCUE_KEYWORD;
                        return true;
                    } else {
                        // Both are valid
                        assert(valid_symbols[MODIFIER_RESCUE_KEYWORD] && valid_symbols[REGULAR_RESCUE_KEYWORD]);

                        // TODO: currently assuming that the modifier always takes
                        // precedence here. Is that correct?
                        lexer->result_symbol = MODIFIER_RESCUE_KEYWORD;
                        return true;
                    }
                }
            }
            break;
        case 's':
            if (valid_symbols[SELECT_KEYWORD] || valid_symbols[SIZEOF_KEYWORD] || valid_symbols[STRUCT_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'e' && valid_symbols[SELECT_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'l') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'c') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = SELECT_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 'i' && valid_symbols[SIZEOF_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'z') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'o') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'f') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = SIZEOF_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 't' && valid_symbols[STRUCT_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'r') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'u') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'c') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = STRUCT_KEYWORD;
                    return true;
                }
            }
            break;
        case 't':
            if (valid_symbols[TRUE_KEYWORD] || valid_symbols[TYPEOF_KEYWORD] || valid_symbols[TYPE_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'r' && valid_symbols[TRUE_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'u') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = TRUE_KEYWORD;
                    return true;

                } else if (lexer->lookahead == 'y' && (valid_symbols[TYPEOF_KEYWORD] || valid_symbols[TYPE_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'p') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);

                    if (valid_symbols[TYPE_KEYWORD] && !(next_char_is_identifier(lexer) && next_is_colon_space(lexer))) {
                        lexer->result_symbol = TYPE_KEYWORD;
                        return true;
                    } else if (valid_symbols[TYPEOF_KEYWORD]) {
                        if (lexer->lookahead != 'o') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'f') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = TYPEOF_KEYWORD;
                        return true;
                    }
                }
            }
            break;
        case 'u':
            if (valid_symbols[REGULAR_UNLESS_KEYWORD] || valid_symbols[MODIFIER_UNLESS_KEYWORD] || valid_symbols[UNTIL_KEYWORD] || valid_symbols[UNION_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead != 'n') { return false; }
                lex_advance(lexer);

                if (lexer->lookahead == 'i' && valid_symbols[UNION_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'o') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'n') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = UNION_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 't' && valid_symbols[UNTIL_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'i') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'l') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = UNTIL_KEYWORD;
                    return true;
                } else if (lexer->lookahead == 'l' && (valid_symbols[REGULAR_UNLESS_KEYWORD] || valid_symbols[MODIFIER_UNLESS_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 'e') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 's') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        // This is some other identifier, not 'unless'
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    if (valid_symbols[MODIFIER_UNLESS_KEYWORD] && !valid_symbols[REGULAR_UNLESS_KEYWORD]) {
                        lexer->result_symbol = MODIFIER_UNLESS_KEYWORD;
                        return true;
                    } else if (valid_symbols[REGULAR_UNLESS_KEYWORD] && !valid_symbols[MODIFIER_UNLESS_KEYWORD]) {
                        lexer->result_symbol = REGULAR_UNLESS_KEYWORD;
                        return true;
                    } else {
                        // Both are valid
                        assert(valid_symbols[MODIFIER_UNLESS_KEYWORD] && valid_symbols[REGULAR_UNLESS_KEYWORD]);

                        // This sort of ambiguity may happen after an identifier
                        // without parentheses, or after a keyword like `return`
                        // that takes an optional expression.
                        lexer->result_symbol = MODIFIER_UNLESS_KEYWORD;
                        return true;
                    }
                }
            }
            break;
        case 'w':
            if (valid_symbols[WHILE_KEYWORD] || valid_symbols[WITH_KEYWORD] || valid_symbols[WHEN_KEYWORD]) {
                lex_advance(lexer);
                if (lexer->lookahead == 'h' && (valid_symbols[WHILE_KEYWORD] || valid_symbols[WHEN_KEYWORD])) {
                    lex_advance(lexer);
                    if (lexer->lookahead == 'e' && valid_symbols[WHEN_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 'n') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = WHEN_KEYWORD;
                        return true;
                    } else if (lexer->lookahead == 'i' && valid_symbols[WHILE_KEYWORD]) {
                        lex_advance(lexer);
                        if (lexer->lookahead != 'l') { return false; }
                        lex_advance(lexer);
                        if (lexer->lookahead != 'e') { return false; }

                        lex_advance(lexer);
                        if (next_char_is_identifier(lexer)) {
                            return false;
                        } else if (next_is_colon_space(lexer)) {
                            return false;
                        }

                        lexer->result_symbol = WHILE_KEYWORD;
                        return true;
                    }
                } else if (lexer->lookahead == 'i' && valid_symbols[WITH_KEYWORD]) {
                    lex_advance(lexer);
                    if (lexer->lookahead != 't') { return false; }
                    lex_advance(lexer);
                    if (lexer->lookahead != 'h') { return false; }

                    lex_advance(lexer);
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = WITH_KEYWORD;
                    return true;
                }
            }
            break;
        case 'y':
            if (valid_symbols[END_OF_WITH_EXPRESSSION] || valid_symbols[YIELD_KEYWORD]) {
                // We don't want to consume the yield keyword
                lexer->mark_end(lexer);

                lex_advance(lexer);
                if (lexer->lookahead != 'i') { return false; }
                lex_advance(lexer);
                if (lexer->lookahead != 'e') { return false; }
                lex_advance(lexer);
                if (lexer->lookahead != 'l') { return false; }
                lex_advance(lexer);
                if (lexer->lookahead != 'd') { return false; }
                lex_advance(lexer);

                if (valid_symbols[END_OF_WITH_EXPRESSSION]) {
                    if (next_char_is_identifier(lexer)) {
                        // This is some other identifier, not 'yield'
                        return false;
                    }

                    lexer->result_symbol = END_OF_WITH_EXPRESSSION;
                    return true;
                } else if (valid_symbols[YIELD_KEYWORD]) {
                    if (next_char_is_identifier(lexer)) {
                        return false;
                    } else if (next_is_colon_space(lexer)) {
                        return false;
                    }

                    lexer->result_symbol = YIELD_KEYWORD;
                    return true;
                }
            }
            break;

        case '}':
            lex_advance(lexer);

            // End of a macro expression
            if (lexer->lookahead == '}') {
                return false;
            }
            break;
    }

    return false;
}

bool tree_sitter_crystal_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    DEBUG("starting external scan");
    if (lexer->lookahead == '\n') {
        DEBUG("char is '\\n'");
    } else if (lexer->lookahead == '\0') {
        DEBUG("char is '\\0'");
    } else if (lexer->lookahead == '\r') {
        DEBUG("char is '\\r'");
    } else if (lexer->lookahead == '\t') {
        DEBUG("char is '\\t'");
    } else {
        DEBUG("char is '%c'", lexer->lookahead);
    }
    DEBUG("valid symbols are:");

#define LOG_SYMBOL(sym) \
    if (valid_symbols[sym]) { DEBUG("      " #sym); }

    LOG_SYMBOL(LINE_BREAK);
    LOG_SYMBOL(LINE_CONTINUATION);
    LOG_SYMBOL(START_OF_BRACE_BLOCK);
    LOG_SYMBOL(START_OF_HASH_OR_TUPLE);
    LOG_SYMBOL(START_OF_NAMED_TUPLE);
    LOG_SYMBOL(START_OF_TUPLE_TYPE);
    LOG_SYMBOL(START_OF_NAMED_TUPLE_TYPE);
    LOG_SYMBOL(START_OF_INDEX_OPERATOR);
    LOG_SYMBOL(END_OF_WITH_EXPRESSSION);
    LOG_SYMBOL(UNARY_PLUS);
    LOG_SYMBOL(UNARY_MINUS);
    LOG_SYMBOL(BINARY_PLUS);
    LOG_SYMBOL(BINARY_MINUS);
    LOG_SYMBOL(UNARY_WRAPPING_PLUS);
    LOG_SYMBOL(UNARY_WRAPPING_MINUS);
    LOG_SYMBOL(BINARY_WRAPPING_PLUS);
    LOG_SYMBOL(BINARY_WRAPPING_MINUS);
    LOG_SYMBOL(UNARY_STAR);
    LOG_SYMBOL(BINARY_STAR);
    LOG_SYMBOL(UNARY_DOUBLE_STAR);
    LOG_SYMBOL(BINARY_DOUBLE_STAR);
    LOG_SYMBOL(BLOCK_AMPERSAND);
    LOG_SYMBOL(BINARY_AMPERSAND);
    LOG_SYMBOL(BEGINLESS_RANGE_OPERATOR);
    LOG_SYMBOL(REGEX_START);
    LOG_SYMBOL(BINARY_SLASH);
    LOG_SYMBOL(BINARY_DOUBLE_SLASH);
    LOG_SYMBOL(REGULAR_IF_KEYWORD);
    LOG_SYMBOL(MODIFIER_IF_KEYWORD);
    LOG_SYMBOL(REGULAR_UNLESS_KEYWORD);
    LOG_SYMBOL(MODIFIER_UNLESS_KEYWORD);
    LOG_SYMBOL(REGULAR_RESCUE_KEYWORD);
    LOG_SYMBOL(MODIFIER_RESCUE_KEYWORD);
    LOG_SYMBOL(REGULAR_ENSURE_KEYWORD);
    LOG_SYMBOL(MODIFIER_ENSURE_KEYWORD);
    LOG_SYMBOL(MODULO_OPERATOR);
    LOG_SYMBOL(START_OF_SYMBOL);
    LOG_SYMBOL(UNQUOTED_SYMBOL_CONTENT);
    LOG_SYMBOL(ABSTRACT_KEYWORD);
    LOG_SYMBOL(ALIAS_KEYWORD);
    LOG_SYMBOL(ALIGNOF_KEYWORD);
    LOG_SYMBOL(ANNOTATION_KEYWORD);
    LOG_SYMBOL(ASM_KEYWORD);
    LOG_SYMBOL(BEGIN_KEYWORD);
    LOG_SYMBOL(BREAK_KEYWORD);
    LOG_SYMBOL(CASE_KEYWORD);
    LOG_SYMBOL(CLASS_KEYWORD);
    LOG_SYMBOL(DEF_KEYWORD);
    LOG_SYMBOL(DO_KEYWORD);
    LOG_SYMBOL(ELSE_KEYWORD);
    LOG_SYMBOL(ELSIF_KEYWORD);
    LOG_SYMBOL(END_KEYWORD);
    LOG_SYMBOL(ENUM_KEYWORD);
    LOG_SYMBOL(EXTEND_KEYWORD);
    LOG_SYMBOL(FALSE_KEYWORD);
    LOG_SYMBOL(FUN_KEYWORD);
    LOG_SYMBOL(IN_KEYWORD);
    LOG_SYMBOL(INCLUDE_KEYWORD);
    LOG_SYMBOL(INSTANCE_ALIGNOF_KEYWORD);
    LOG_SYMBOL(INSTANCE_SIZEOF_KEYWORD);
    LOG_SYMBOL(LIB_KEYWORD);
    LOG_SYMBOL(MACRO_KEYWORD);
    LOG_SYMBOL(MODULE_KEYWORD);
    LOG_SYMBOL(NEXT_KEYWORD);
    LOG_SYMBOL(NIL_KEYWORD);
    LOG_SYMBOL(OFFSETOF_KEYWORD);
    LOG_SYMBOL(OUT_KEYWORD);
    LOG_SYMBOL(POINTEROF_KEYWORD);
    LOG_SYMBOL(PRIVATE_KEYWORD);
    LOG_SYMBOL(PROTECTED_KEYWORD);
    LOG_SYMBOL(REQUIRE_KEYWORD);
    LOG_SYMBOL(RETURN_KEYWORD);
    LOG_SYMBOL(SELECT_KEYWORD);
    LOG_SYMBOL(SIZEOF_KEYWORD);
    LOG_SYMBOL(STRUCT_KEYWORD);
    LOG_SYMBOL(TRUE_KEYWORD);
    LOG_SYMBOL(TYPE_KEYWORD);
    LOG_SYMBOL(TYPEOF_KEYWORD);
    LOG_SYMBOL(UNION_KEYWORD);
    LOG_SYMBOL(UNTIL_KEYWORD);
    LOG_SYMBOL(WHEN_KEYWORD);
    LOG_SYMBOL(WHILE_KEYWORD);
    LOG_SYMBOL(WITH_KEYWORD);
    LOG_SYMBOL(YIELD_KEYWORD);
    LOG_SYMBOL(TYPE_FIELD_COLON);
    LOG_SYMBOL(STRING_LITERAL_START);
    LOG_SYMBOL(DELIMITED_STRING_CONTENTS);
    LOG_SYMBOL(STRING_LITERAL_END);
    LOG_SYMBOL(COMMAND_LITERAL_START);
    LOG_SYMBOL(COMMAND_LITERAL_END);
    LOG_SYMBOL(STRING_PERCENT_LITERAL_START);
    LOG_SYMBOL(COMMAND_PERCENT_LITERAL_START);
    LOG_SYMBOL(STRING_ARRAY_PERCENT_LITERAL_START);
    LOG_SYMBOL(SYMBOL_ARRAY_PERCENT_LITERAL_START);
    LOG_SYMBOL(REGEX_PERCENT_LITERAL_START);
    LOG_SYMBOL(PERCENT_LITERAL_END);
    LOG_SYMBOL(DELIMITED_ARRAY_ELEMENT_START);
    LOG_SYMBOL(DELIMITED_ARRAY_ELEMENT_END);
    LOG_SYMBOL(HEREDOC_START);
    LOG_SYMBOL(HEREDOC_BODY_START);
    LOG_SYMBOL(HEREDOC_CONTENT);
    LOG_SYMBOL(HEREDOC_END);
    LOG_SYMBOL(REGEX_MODIFIER);
    LOG_SYMBOL(MACRO_START);
    LOG_SYMBOL(MACRO_DELIMITER_END);
    LOG_SYMBOL(MACRO_DELIMITER_ELSE);
    LOG_SYMBOL(MACRO_DELIMITER_ELSIF);
    LOG_SYMBOL(MACRO_CONTENT);
    LOG_SYMBOL(MACRO_CONTENT_NESTING);
    LOG_SYMBOL(START_OF_PARENLESS_ARGS);
    LOG_SYMBOL(END_OF_RANGE);
    LOG_SYMBOL(START_OF_MACRO_VAR_EXPS);
    LOG_SYMBOL(ERROR_RECOVERY);

    bool result = inner_scan(payload, lexer, valid_symbols);

    if (result) {
        DEBUG("external scan got a result");
    } else {
        DEBUG("external scan returned nothing");
    }

    return result;
}

void *tree_sitter_crystal_external_scanner_create(void) {
    State *state;

    state = ts_calloc(1, sizeof(State));

    state->has_leading_whitespace = false;
    state->previous_line_continued = false;

    reset_macro_state(state);

    array_init(&state->literals);
    array_init(&state->heredocs);

    return state;
}

// Release any memory allocated for heredoc identifiers
static void free_old_heredoc_identifiers(State *state) {
    for (size_t i = 0; i < state->heredocs.size; i++) {
        Heredoc *heredoc = array_get(&state->heredocs, i);
        array_delete(&heredoc->identifier);
    }
}

void tree_sitter_crystal_external_scanner_destroy(void *payload) {
    State *state = (State *)payload;

    free_old_heredoc_identifiers(state);
    array_delete(&state->literals);
    array_delete(&state->heredocs);
    ts_free(state);
}

unsigned tree_sitter_crystal_external_scanner_serialize(void *payload, char *buffer) {
    State *state = (State *)payload;
    size_t offset = 0;

    buffer[offset++] = (char)state->has_leading_whitespace;
    buffer[offset++] = (char)state->previous_line_continued;

    buffer[offset++] = (char)state->macro_state.in_comment;
    buffer[offset++] = (char)state->macro_state.non_modifier_keyword_can_begin;

    // It's safe to cast the literal count into a char since it will always be
    // less than MAX_LITERAL_COUNT.
    buffer[offset++] = (char)state->literals.size;

    // The literals array can be serialized in one chunk.
    size_t literal_content_size = state->literals.size * array_elem_size(&state->literals);
    memcpy(&buffer[offset], state->literals.contents, literal_content_size);
    offset += literal_content_size;

    // It's safe to cast the heredoc count into a char since it will always be
    // less than MAX_HEREDOC_COUNT.
    buffer[offset++] = (char)state->heredocs.size;

    // Heredoc are serialized one at a time, with their identifier buffers inlined.
    for (uint8_t i = 0; i < state->heredocs.size; i++) {
        Heredoc *heredoc = array_get(&state->heredocs, i);

        buffer[offset++] = (char)heredoc->allow_escapes;
        buffer[offset++] = (char)heredoc->started;

        // It's safe to cast the identifier size into a char since it will
        // always be less than or equal to MAX_HEREDOC_WORD_SIZE.
        buffer[offset++] = (char)heredoc->identifier.size;

        memcpy(&buffer[offset], heredoc->identifier.contents, heredoc->identifier.size);
        offset += heredoc->identifier.size;
    }

    assert(offset <= TREE_SITTER_SERIALIZATION_BUFFER_SIZE);

    return offset;
}

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
static_assert(
    2                                                    // boolean variables
            + sizeof(MacroState)                         // macro state
            + 1                                          // literals count
            + sizeof(PercentLiteral) * MAX_LITERAL_COUNT // each literal
            + 1                                          // heredocs count
            + 3 * MAX_HEREDOC_COUNT                      // each heredoc object
            + HEREDOC_BUFFER_SIZE                        // heredoc buffer total
        <= TREE_SITTER_SERIALIZATION_BUFFER_SIZE,
    "Maximum serialized size is too large");
#endif

void tree_sitter_crystal_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    State *state = (State *)payload;

    // Any deserialized heredocs will overwrite what's here already, so free the
    // existing heredoc references.
    free_old_heredoc_identifiers(state);

    // Reset the size of the arrays, but don't touch their reserved memory or capacity.
    // We can reuse the same content buffers without freeing and reallocating memory.
    array_clear(&state->literals);
    array_clear(&state->heredocs);

    if (length == 0) {
        // Sometimes this function is called with a length of zero. In that
        // case we just finish resetting the state.
        state->has_leading_whitespace = false;
        state->previous_line_continued = false;

        reset_macro_state(state);
        return;
    }

    size_t offset = 0;
    state->has_leading_whitespace = (bool)buffer[offset++];
    state->previous_line_continued = (bool)buffer[offset++];

    state->macro_state.in_comment = (bool)buffer[offset++];
    state->macro_state.non_modifier_keyword_can_begin = (bool)buffer[offset++];

    // The literals array can be deserialized in one chunk.
    uint8_t literals_size = (uint8_t)buffer[offset++];
    array_extend(&state->literals, literals_size, &buffer[offset]);
    offset += literals_size * array_elem_size(&state->literals);

    // Each heredoc must be deserialized individually to handle the identifier buffer.
    uint8_t heredocs_size = (uint8_t)buffer[offset++];
    for (uint8_t i = 0; i < heredocs_size; i++) {
        Heredoc heredoc = {
            .allow_escapes = false,
            .started = false,
            .identifier = array_new(),
        };
        heredoc.allow_escapes = (bool)buffer[offset++];
        heredoc.started = (bool)buffer[offset++];

        uint8_t identifier_size = (uint8_t)buffer[offset++];
        array_extend(&heredoc.identifier, identifier_size, &buffer[offset]);
        offset += identifier_size;

        array_push(&state->heredocs, heredoc);
    }
    assert(offset == length);
}
